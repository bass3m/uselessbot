-module(useless_irc_timer).

-behavior(gen_server).
-export([start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-compile(export_all). % replace with export later

-define(SERVER, ?MODULE).
-define(TIMERPREFIX,"timer").
-define(MAX_PENDING_TIMERS,10).

-record(state, {pending = []}).
-record(worker_state, {user, message, timer, parent}).

start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() -> gen_server:call(?SERVER, stop).

init([]) ->
    random:seed(now()),
    %% register our timer service
    useless_irc_services:register_service(timer,
                                          ?TIMERPREFIX,
                                          self(), node(),
                                          ?SERVER),
    {ok, #state{}}.

handle_call(stop, _From, State) ->
            {stop, normal, stopped, State}.

handle_cast(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

run_timer(_State = #worker_state{user=User,parent=ParentPid,
                                timer=Timer,message=Message}) ->
    receive
    after Timer*1000 ->
        ParentPid ! {cmd_resp, User, "", Message}
        %% have to delete timer from pending after it expires
    end.

handle_info({run, Request, Chan, User, From}, State)
  when Request =:= "" orelse Request =:= "help" ->
    From ! {cmd_resp, User, Chan, "Set a timer in h:m:s format."},
    {noreply, State};

handle_info({run, _Request, Chan, User, FromPid},
            #state{pending=Pending} = State) when Chan =:= ""->
    case lists:keyfind(User,1,Pending) of
        %% check that user could be one of the active sessions
        {_, Chan, _, _Worker} ->
            %% try not accept another command if have one outstanding for that
            %% same user
            FromPid ! {cmd_resp, User, Chan, "You already have a timer running"},
            {noreply, State};
        _ ->
            case length(Pending) >= ?MAX_PENDING_TIMERS of
                true ->
                    FromPid ! {cmd_resp, User, Chan, "Too many timers running"},
                    {noreply, State};
                false ->
                    Worker = spawn_link(?MODULE, run_timer,
                                        [#worker_state{user = User,
                                                       timer = 20, message = "Tea time",
                                                       parent = self()}]),
                    NewState = #state{pending = [{User, FromPid, Worker} | Pending]},
                    {noreply, NewState}
            end
    end;

handle_info({run, _Request, Chan, User, From}, State) ->
    From ! {cmd_resp, User, Chan, "Msg me in private"},
    {noreply, State};

handle_info({cmd_resp, User, _Id, Result}, #state{pending=Pending} = State) ->
    % when get response, now we have to send that response back to
    case lists:keyfind(User,1,Pending) of
        false ->
            io:format("Not found New Cmd Resp ~p Result ~p State ~p~n",
                      [User,Result,State]),
            not_found;
        {User, From, _Worker} ->
            From ! {cmd_resp, User, "", Result}
    end,
    {noreply, State};

handle_info(Msg, State) ->
    io:format("Unexpect Msg rcvd ~p~n",[Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

