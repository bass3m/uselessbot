-module(useless_irc_services).
-behavior(gen_server).
-export([start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-compile(export_all). % replace with export later

-define(SERVER, ?MODULE).

register_service(Service, Prefix, Mod) ->
    register_service(Service, Prefix, self(), node(), Mod).

register_service(Service, Prefix, Pid, Node, Mod) ->
    gen_server:cast(?MODULE, {register, {Service, Prefix, Pid, Node, Mod}}).

remove_service(Service) ->
    gen_server:cast(?MODULE, {remove, Service}).

%% return the service given the prefix
get_service(Prefix) ->
    gen_server:call(?SERVER, {get, Prefix}).

get_all_services() ->
    gen_server:call(?SERVER, get_all).


%% Client APIs
%% all Synchronous calls right now
start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() -> gen_server:call(?SERVER, stop).

%% Server APIs
init([]) ->
    {ok, []}.

%% can have multiple nodes per service
%% TODO monitor that process and remove if/when died
%% [{service1,prefix1,[nodex,nodey]},{service2,prefix2,[nodea,nodeb]}]
handle_cast({register, {Service, Prefix, Pid, Node, Mod}}, State) when is_atom(Service) ->
    %% returns false only if if it's a new {service,prefix} or {service,prefix}
    %% already exists (for the later case add another node to our list)
    case lists:keymember(Service,1,State) xor
         lists:keymember(Prefix,2,State) of
         true ->
            io:format("~p,~p already exists~n",[Service,Prefix]),
            {noreply, State}; % service or prefix should be a unique pair
         false ->
            NewState =
                case lists:keyfind(Service,1,State) of
                    false ->
                        [{Service, Prefix, {Pid, Node, Mod}} | State];
                    {Service, Prefix, Nodes} ->
                        lists:keyreplace(Service, 1, State,
                                         {Service, Prefix,
                                          lists:flatten([{Pid, Node, Mod} |
                                                         lists:delete({Pid, Node, Mod}, [Nodes])])})
                end,
            io:format("Adding new Service ~p with prefix ~p~n",[Service,Prefix]),
            {noreply, NewState}
    end;

%% TODO remove from monitor
handle_cast({remove, Service}, State) when is_atom(Service) ->
    case lists:keymember(Service,1,State) of
         true ->
            io:format("~p exists removing it~n",[Service]),
            {noreply, lists:keydelete(Service,1,State)};
         false ->
            io:format("~p doesn't exists~n",[Service]),
            {noreply, State}
    end;

handle_cast(_Msg, State) -> {noreply, State}.

% get a service matching the requested prefix
handle_call({get, Prefix}, _From, State) ->
    %io:format("Fetching Prefix: ~p from State ~p~n",[Prefix,State]),
    Reply = case lists:keyfind(Prefix,2,State) of
                false -> not_found;
                {Service, Prefix, Nodes} -> {Service, Prefix, Nodes}
            end,
    {reply, Reply, State};

handle_call(stop, _From, State) ->
            {stop, normal, stopped, State}.

terminate(_Reason, _State) -> ok.

handle_info(Msg, State) ->
    io:format("Unexpected message rcvd: ~p~n",[Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
