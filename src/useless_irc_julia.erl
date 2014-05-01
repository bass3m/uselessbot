-module(useless_irc_julia).

%% this should be a supervisor somehow which each pending
%% session as a worker
-behavior(gen_server).
-export([start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-compile(export_all). % replace with export later

-define(SERVER, ?MODULE).
-define(JULIAPREFIX,"julia").
-define(TIMEOUT,3000).
-define(SESSION_TIMEOUT,120000).
-define(MAX_SESSIONS,3).
-define(MANDELBROT,"http://localhost:8080").

%% pending will have a list of tuple containing nick, calc etc..
-record(state, {pending = []}).
-record(worker_state, {user, chan, id, parent, time}).

start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() -> gen_server:call(?SERVER, stop).

get_num_pending() ->
    gen_server:call(?SERVER,num_pending).

init([]) ->
    inets:start(), % to start using httpc client
    %% register our julia service
    useless_irc_services:register_service(julia, ?JULIAPREFIX, self(), node(), ?SERVER),
    {ok, #state{}}.

handle_call({num_pending}, _From, State) ->
    {reply, length(State#state.pending), State};

handle_call(stop, _From, State) ->
            {stop, normal, stopped, State}.

mandelbrot_api_request(Method,Url) ->
    mandelbrot_api_request(Method,Url,"","").

mandelbrot_api_request(Method,Url,UrlEncoding,QueryStr) ->
    case httpc:request(Method,{Url,[],UrlEncoding,QueryStr},[],[]) of
        {ok, {{_HttpVer, Code, _Msg}, _Headers, Body}}
          when 200 =< Code andalso Code < 300 ->
            {_, JsonBody} = mochijson2:decode(Body),
            {ok, JsonBody};
        Failed ->
            io:format("Failed request to Mandelbrot~p~n",[Failed]),
            {failed}
    end.

create_julia_session() ->
    case mandelbrot_api_request(post,?MANDELBROT ++ "/sessions") of
        {ok, JsonBody} ->
            Id = binary_to_list(proplists:get_value(<<"sid">>,JsonBody)),
            io:format("Got a session ~p~n",[Id]),
            {created, Id};
        {failed} -> {failed}
    end.

check_julia_result(ResultJsonBody) ->
    case proplists:lookup(<<"value">>,ResultJsonBody) of
        {_, Result} -> {result, binary_to_list(Result)};
        _ -> {failed, binary_to_list(proplists:get_value(<<"error">>,
                                                         ResultJsonBody))}
    end.

execute_julia_cmd(Id,Cmd) ->
    QueryStr = "sid=" ++ Id ++
               "&fetch=true&format=text&proc=" ++ http_uri:encode(Cmd),
    case mandelbrot_api_request(post,?MANDELBROT ++ "/references",
                                "application/x-www-form-urlencoded",QueryStr) of
        {ok, JsonBody} ->
            %% extract the result in val
            {_, Result} = check_julia_result(JsonBody),
            io:format("Got the result ~p~n",[Result]),
            {result, Result};
        {failed} ->
            {failed}
    end.

delete_julia_session(Id) when Id =:= "" ->
    ok;

delete_julia_session(Id) ->
    {ok, JsonBody} = mandelbrot_api_request(delete,
                                            ?MANDELBROT ++ "/sessions/" ++ Id),
    Id = binary_to_list(proplists:get_value(<<"sid">>,JsonBody)),
    io:format("Deleted session ~p~n",[Id]),
    {deleted, Id}.

%% ParentPid is the gen_server which spawned this worker process
run_julia_run(State = #worker_state{user=User,parent=ParentPid}) ->
    receive
        {new_cmd, Cmd} ->
            case create_julia_session() of
                {created, Id} ->
                    % run the command
                    case execute_julia_cmd(Id,Cmd) of
                        {result, Result} ->
                            ParentPid ! {cmd_resp, User, Id, Result},
                            run_julia_run(State#worker_state{id = Id});
                        {failed} ->
                            ParentPid ! {cmd_run_failed, User, Id}
                    end;
                {failed} ->
                    ParentPid ! {cmd_run_failed, User, no_id}
            end;
        {cmd, Cmd} ->
            Id = State#worker_state.id,
            case execute_julia_cmd(Id,Cmd) of
                {result, Result} ->
                    ParentPid ! {cmd_resp, User, Id, Result},
                    run_julia_run(State);
                {failed} ->
                    ParentPid ! {cmd_run_failed, User, Id}
            end
    after ?SESSION_TIMEOUT -> %% 2 minutes of inactivity
        Id = State#worker_state.id,
        delete_julia_session(Id),
        ParentPid ! {timeout, User}
    end.

handle_cast(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

%% use the User as the lookup key, since we shouldn't create more than
%% 1 session per user
handle_info({timeout, User}, #state{pending=Pending} = _State) ->
    io:format("Timeout for User ~p~n",[User]),
    NewState = #state{pending = lists:keydelete(User,1,Pending)},
    {noreply, NewState};

handle_info({run, CommandToRun, Chan, User, FromPid},
            #state{pending=Pending} = State) ->
    case lists:keyfind(User,1,Pending) of
        %% check that user could be one of the active sessions
        {_, _, _, Worker} ->
            %% try not accept another command if have one outstanding for that
            %% same user
            Worker ! {cmd, CommandToRun},
            {noreply, State};
        false ->
            case length(Pending) >= ?MAX_SESSIONS of
                true ->
                    {reply, busy, State};
                false ->
                    Worker = spawn_link(?MODULE, run_julia_run,
                                        [#worker_state{user = User, chan = Chan,
                                                       id = "", parent = self()}]),
                    Worker ! {new_cmd, CommandToRun},
                    NewState = #state{pending = [{User, Chan, FromPid, Worker} | Pending]},
                    {noreply, NewState}
            end
    end;

handle_info({cmd_resp, User, _Id, Result}, #state{pending=Pending} = State) ->
    % when get response, now we have to send that response back to
    case lists:keyfind(User,1,Pending) of
        false ->
            io:format("Not found New Cmd Resp ~p Result ~p State ~p~n",
                      [User,Result,State]),
            not_found;
        {User, Chan, From, _Worker} ->
            From ! {cmd_resp, User, Chan, Result}
    end,
    {noreply, State};

handle_info({cmd_run_failed, User, _Id}, #state{pending=Pending} = State) ->
    % when get response, now we have to send that response back
    NewState = case lists:keyfind(User,1,Pending) of
                   false ->
                       io:format("User ~p Not found~n",
                                 [User]),
                       State;
                   {User, Chan, From, _Worker} ->
                       io:format("Cmd run failed for User ~p Chan ~p~n",
                                 [User,Chan]),
                       From ! {cmd_run_failed, User},
                       #state{pending = lists:keydelete(User,1,Pending)}
    end,
    {noreply, NewState};

handle_info(Msg, State) ->
    io:format("Unexpected message rcvd: ~p State ~p~n",[Msg,State]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
