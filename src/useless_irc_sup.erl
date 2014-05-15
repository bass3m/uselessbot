-module(useless_irc_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    IrcBotServer = {useless_irc, {useless_irc, start, []},
                    permanent, 2000, worker, [useless_irc]},
    IrcBotServices = {useless_irc_services, {useless_irc_services, start, []},
                    permanent, 2000, worker, [useless_irc_services]},
    IrcBotJulia = {useless_irc_julia, {useless_irc_julia, start, []},
                    permanent, 2000, worker, [useless_irc_julia]},
    IrcBotInsults = {useless_irc_insults, {useless_irc_insults, start, []},
                    permanent, 2000, worker, [useless_irc_insults]},
    IrcBotWeather = {useless_irc_weather, {useless_irc_weather, start, []},
                    permanent, 2000, worker, [useless_irc_weather]},
    IrcBot8Ball = {useless_irc_8ball, {useless_irc_8ball, start, []},
                    permanent, 2000, worker, [useless_irc_8ball]},
    IrcBotTimer = {useless_irc_timer, {useless_irc_timer, start, []},
                    permanent, 2000, worker, [useless_irc_timer]},
    Children = [IrcBotServer,IrcBotServices,
                IrcBotJulia,IrcBotInsults,
                IrcBotWeather,IrcBot8Ball,IrcBotTimer],
    RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.

