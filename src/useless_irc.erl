-module(useless_irc).
-behavior(gen_server).
-export([start/0, login/1, join/1, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-compile(export_all). % replace with export later

-define(SERVER, ?MODULE).
-define(REALNAME, "UselessBot").
-define(CRLF, "\r\n").

%% should be map that contains an an array of plugin maps
-record(state, {server, port, nick, pass, username, realname, channel, sock}).

%% Client APIs
%% all Synchronous calls right now
start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() -> gen_server:call(?SERVER, stop).

login(Nick) ->
    gen_server:call(?SERVER, {login, Nick, ?REALNAME}).

join(Channel) ->
    gen_server:call(?SERVER, {join_channel, Channel}).

%% Server APIs
init([]) ->
    {ok, Server} = application:get_env(server),
    {ok, Port} = application:get_env(port),
    Options = [binary, {active, true},
               {packet, line}, {keepalive, true}],
    {ok,Socket} = gen_tcp:connect(Server,Port,Options),
    io:format("connected to:~p~n",[Socket]),
    {ok, #state{server = Server, port = Port, sock = Socket}}.

handle_call({login, Nick, Realname}, _From, State) ->
    %% write to socket with NICK and USER
    ok = gen_tcp:send(State#state.sock, "NICK " ++ Nick ++ ?CRLF),
    Reply = gen_tcp:send(State#state.sock,
                         string:join(["USER",Nick,"0 * :",
                                      Realname,?CRLF]," ")),
    {reply, Reply, State#state{nick = Nick, realname = Realname}};

%% worry about chankeys later
handle_call({join_channel, Channel}, _From, State) ->
    Reply = gen_tcp:send(State#state.sock,
                         "JOIN " ++ Channel ++ ?CRLF),
    {reply, Reply, State#state{channel = Channel}};

handle_call(stop, _From, State) ->
            {stop, normal, stopped, State}.

terminate(_Reason, _State) -> ok.

handle_cast(_Msg, State) -> {noreply, State}.

process_prefix("help",_Request,Chan,FromNick,State) ->
    ReplyTo = case Chan of
                  "" -> FromNick;
                  _ -> Chan
              end,
    % call get_all to say that these are current available plugins
    AvailServices = useless_irc_services:get_all_services(),
    [gen_tcp:send(State#state.sock,
                  string:join(["PRIVMSG",ReplyTo,
                               string:join([":Plugin:",
                                            atom_to_list(Service),
                                            "with prefix :",
                                            Prefix,?CRLF]," ")
                              ], " ")) || {Service, Prefix} <- AvailServices];

process_prefix(Prefix,Request,Chan,FromNick,_State) ->
    %% find service that matches the prefix
    case useless_irc_services:get_service(Prefix) of
        %% for now, ignore the node, and just use the pid
        {_, _, {ServicePid, _, _Mod}} ->
            ServicePid ! {run, Request, Chan, FromNick, self()};
        not_found ->
            io:format("Ignoring Prefix ~p Request ~p Chan ~p FromNick ~p~n",
                      [Prefix,Request,Chan,FromNick]),
            ignored % just log here
    end.

% might be nice if service is invoked with no args etc..
% to return a response with the help
handle_info({cmd_resp, User, Chan, Result}, State) when Chan =:= "" ->
    [gen_tcp:send(State#state.sock,
                  string:join(["PRIVMSG",User,":" ++ Line ++ ?CRLF]," ")) ||
     Line <- string:tokens(Result,"\n")],
    {noreply, State};

handle_info({tcp_closed, Reason}, State) ->
    io:format("Tcp closed with reason: ~p~n",[Reason]),
    {noreply, State};

%% TODO use ref as a way to identify the response
handle_info({tcp, _Socket, Msg}, State) ->
    % maybe parse msg can find the help
    case useless_irc_parser:parse_msg(Msg) of
        {response, Response} ->
            io:format("Respond to server with ~p~n",[Response]),
            gen_tcp:send(State#state.sock, Response ++ ?CRLF);
        {msg, FromNick, [ToNick | Privmsg]} when ToNick =:= State#state.nick ->
            [Prefix|Request] = useless_irc_parser:process_private_msg(Privmsg),
            process_prefix(Prefix,Request,"",FromNick,State);
        {msg, FromNick, [Chan | Chanmsg]} when Chan =:= State#state.channel ->
            [Prefix|Request] = useless_irc_parser:process_channel_msg(Chanmsg),
            process_prefix(Prefix,Request,Chan,FromNick,State);
        _ -> ignored
    end,
    {noreply, State};

handle_info({cmd_resp, User, Chan, Result}, State) when Chan =:= "" ->
    [gen_tcp:send(State#state.sock,
                  string:join(["PRIVMSG",User,":" ++ Line ++ ?CRLF]," ")) ||
     Line <- string:tokens(Result,"\n")],
    {noreply, State};

handle_info({cmd_resp, _User, Chan, Result}, State) ->
    [gen_tcp:send(State#state.sock,
                  string:join(["PRIVMSG",Chan,":" ++ Line ++ ?CRLF]," ")) ||
     Line <- string:tokens(Result,"\n")],
    {noreply, State};

handle_info({cmd_run_failed, User, Chan, ResponseMsg}, State) when Chan =:= "" ->
    gen_tcp:send(State#state.sock,
                 string:join(["PRIVMSG",User,":" ++ ResponseMsg ++ ?CRLF]," ")),
    {noreply, State};

handle_info({cmd_run_failed, _User, Chan, ResponseMsg}, State) ->
    gen_tcp:send(State#state.sock,
                 string:join(["PRIVMSG",Chan,":" ++ ResponseMsg ++ ?CRLF]," ")),
    {noreply, State};

handle_info(Msg, State) ->
    io:format("IRC Server Unexpected message rcvd: ~p~n",[Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
