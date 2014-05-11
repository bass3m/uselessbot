-module(useless_irc_weather).

-behavior(gen_server).
-export([start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-compile(export_all). % replace with export later

-define(SERVER, ?MODULE).
-define(WEATHERPREFIX,"weather").
-define(TIMEOUT,3000).
-define(WEATHERURL,"http://api.wunderground.com/api/").

%% key should go to env vars

start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() -> gen_server:call(?SERVER, stop).

init([]) ->
    inets:start(), % to start using httpc client
    %% register our weather service
    useless_irc_services:register_service(weather, ?WEATHERPREFIX,
                                          self(), node(), ?SERVER),
    {ok, []}.

handle_call(stop, _From, State) ->
            {stop, normal, stopped, State}.

handle_cast(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

check_errored_response([{_,{_,JsonResponse}}]) ->
    case lists:any(fun(Key)-> proplists:is_defined(Key,JsonResponse) end,
                   [<<"error">>,<<"results">>]) of
        true ->
            {error, "Failed to find location!"};
        false ->
            ok
    end;

check_errored_response(_) ->
    ok.

filter_api_response({error,_},_Keys) ->
    {error, "Error response from weather server"};

filter_api_response(JsonResp,Keys) ->
    case check_errored_response(JsonResp) of
        ok ->
            lists:foldl(fun(X,Acc)-> element(2,proplists:get_value(X,Acc)) end,
                        JsonResp,
                        Keys);
        {error, ErrMsg} -> {error, ErrMsg}
    end.

call_api(Feature,Location) ->
    {ok, Token} = application:get_env(weather_token),
    Url = string:join([?WEATHERURL,Token,"/",Feature,
                       "/q/",Location,".json"],""),
    case httpc:request(Url) of
        {ok, {{_HttpVer, _Code, _Msg}, _Headers, Body}} ->
            {_,Jbody} = mochijson2:decode(Body),
            Jbody;
        Error ->
            Error
    end.

forecast(Location) ->
    JResponse = call_api("forecast",Location),
    case filter_api_response(JResponse,
                             [<<"forecast">>,
                              <<"txt_forecast">>]) of
        {error, ErrMsg} -> ErrMsg;
        TxtFcast ->
            [{_,FcastDay}|_] = proplists:get_value(<<"forecastday">>,TxtFcast),
            string:join(["Forecast for",
                         binary_to_list(proplists:get_value(<<"title">>,FcastDay)),
                         "is",
                         binary_to_list(proplists:get_value(<<"fcttext">>,FcastDay))],
                        " ")
    end.

parse_time(Key,Moonphase) ->
    {_, Time} = proplists:get_value(Key, Moonphase),
    string:join([binary_to_list(proplists:get_value(<<"hour">>,Time)),
                 ":",
                 binary_to_list(proplists:get_value(<<"minute">>,Time))],"").

astro(Location) ->
    JResponse = call_api("astronomy",Location),
    case filter_api_response(JResponse,
                             [<<"moon_phase">>]) of
        {error, ErrMsg} ->
            ErrMsg;
        Moonphase ->
            string:join(["Percentage of moon illuminated: ",
                         binary_to_list(proplists:get_value(<<"percentIlluminated">>,
                                                            Moonphase)),
                         ", Age of moon: ",
                         binary_to_list(proplists:get_value(<<"ageOfMoon">>,
                                                            Moonphase)),
                         ", Phase: ",
                         binary_to_list(proplists:get_value(<<"phaseofMoon">>,
                                                            Moonphase)),
                         ", Sunrise: ",parse_time(<<"sunrise">>,Moonphase),
                         ", Sunset: ",parse_time(<<"sunset">>,Moonphase)],
                        "")
    end.

conditions(Location) ->
    JResponse = call_api("conditions",Location),
    case filter_api_response(JResponse,
                             [<<"current_observation">>]) of
        {error, ErrMsg} ->
            ErrMsg;
        Current ->
            string:join([binary_to_list(proplists:get_value(<<"observation_time">>,
                                                            Current)),
                         ". ",
                         binary_to_list(proplists:get_value(<<"weather">>,
                                                            Current)),
                         ". Temperature: ",
                         binary_to_list(proplists:get_value(<<"temperature_string">>,
                                                            Current)),
                         ". Precipitation today: ",
                         binary_to_list(proplists:get_value(<<"precip_today_string">>,
                                                            Current)),
                         ". Wind ",
                         binary_to_list(proplists:get_value(<<"wind_string">>,
                                                            Current))],
                        "")
    end.

help(_) ->
    "weather [forecast|conditions|astro|help] location".


parse_location([]) ->
    "95129";
parse_location([Location]) ->
    Location;
parse_location([F,S|_]) ->
    string:join([F,S],"/").

parse_weather_request([]) ->
    help([]);
parse_weather_request(Request) ->
    [Action | ReqTokens] = string:tokens(Request," "),
    case lists:member(list_to_atom(Action),[forecast,astro,conditions,help]) of
        true ->
            apply(?SERVER,list_to_atom(Action),[parse_location(ReqTokens)]);
        false ->
            "No comprendo"
    end.

handle_info({run, Request, Chan, User, From}, State) ->
    From ! {cmd_resp, User, Chan, parse_weather_request(Request)},
    {noreply, State};

handle_info(Msg, State) ->
    io:format("Unexpected message rcvd: ~p State ~p~n",[Msg,State]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

