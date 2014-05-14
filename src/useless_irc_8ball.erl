-module(useless_irc_8ball).

-behavior(gen_server).
-export([start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-compile(export_all). % replace with export later

-define(SERVER, ?MODULE).
-define(MAGIC8BALLPREFIX,"8ball").

-record(answers, {answers = ["As I see it, yes.",
                             "It is certain.",
                             "It is decidedly so.",
                             "Most likely.",
                             "Outlook good.",
                             "Signs point to yes.",
                             "Without a doubt.",
                             "Yes.",
                             "Yes - definitely.",
                             "You may rely on it.",
                             "Reply hazy, try again.",
                             "Ask again later.",
                             "Better not tell you now.",
                             "Cannot predict now.",
                             "Concentrate and ask again.",
                             "Don't count on it.",
                             "My reply is no.",
                             "My sources say no.",
                             "Outlook not so good.",
                             "Very doubtful."]}).

start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() -> gen_server:call(?SERVER, stop).

init([]) ->
    random:seed(now()),
    %% register our 8ball service
    useless_irc_services:register_service(magic_8ball,
                                          ?MAGIC8BALLPREFIX,
                                          self(), node(),
                                          ?SERVER),
    {ok, #answers{}}.

handle_call(stop, _From, State) ->
            {stop, normal, stopped, State}.

handle_cast(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

ask_question(#answers{answers = Answers} = _State) ->
    lists:nth(random:uniform(length(Answers)),Answers).

handle_info({run, Request, Chan, User, From}, State)
  when Request =:= "" orelse Request =:= "help" ->
    From ! {cmd_resp, User, Chan, "Ask me a question."},
    {noreply, State};

handle_info({run, _Request, Chan, User, From}, State) ->
    From ! {cmd_resp, User, Chan, ask_question(State)},
    {noreply, State};

handle_info(Msg, State) ->
    io:format("Unexpect Msg rcvd ~p~n",[Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
