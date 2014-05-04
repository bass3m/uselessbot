-module(useless_irc_insults).

%% insults based on http://insulthost.colorado.edu/
-behavior(gen_server).
-export([start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-compile(export_all). % replace with export later

-record(insults, {adj = ["acidic",
                         "antique",
                         "contemptible",
                         "culturally-unsound",
                         "despicable",
                         "evil",
                         "fermented",
                         "festering",
                         "foul",
                         "fulminating",
                         "humid",
                         "impure",
                         "inept",
                         "inferior",
                         "industrial",
                         "left-over",
                         "low-quality",
                         "malodorous",
                         "off-color",
                         "penguin-molesting",
                         "petrified",
                         "pointy-nosed",
                         "salty",
                         "sausage-snorfling",
                         "tastless",
                         "tempestuous",
                         "tepid",
                         "tofu-nibbling",
                         "unintelligent",
                         "unoriginal",
                         "uninspiring",
                         "weasel-smelling",
                         "wretched",
                         "spam-sucking",
                         "egg-sucking",
                         "decayed",
                         "halfbaked",
                         "infected",
                         "squishy",
                         "porous",
                         "pickled",
                         "coughed-up",
                         "thick",
                         "vapid",
                         "hacked-up",
                         "unmuzzled",
                         "bawdy",
                         "vain",
                         "lumpish",
                         "churlish",
                         "fobbing",
                         "rank",
                         "craven",
                         "puking",
                         "jarring",
                         "fly-bitten",
                         "pox-marked",
                         "fen-sucked",
                         "spongy",
                         "droning",
                         "gleeking",
                         "warped",
                         "currish",
                         "milk-livered",
                         "surly",
                         "mammering",
                         "ill-borne",
                         "beef-witted",
                         "tickle-brained",
                         "half-faced",
                         "headless",
                         "wayward",
                         "rump-fed",
                         "onion-eyed",
                         "beslubbering",
                         "villainous",
                         "lewd-minded",
                         "cockered",
                         "full-gorged",
                         "rude-snouted",
                         "crook-pated",
                         "pribbling",
                         "dread-bolted",
                         "fool-born",
                         "puny",
                         "fawning",
                         "sheep-biting",
                         "dankish",
                         "goatish",
                         "weather-bitten",
                         "knotty-pated",
                         "malt-wormy",
                         "saucyspleened",
                         "motley-mind",
                         "it-fowling",
                         "vassal-willed",
                         "loggerheaded",
                         "clapper-clawed",
                         "frothy",
                         "ruttish",
                         "clouted",
                         "common-kissing",
                         "pignutted",
                         "folly-fallen",
                         "plume-plucked",
                         "flap-mouthed",
                         "swag-bellied",
                         "dizzy-eyed",
                         "gorbellied",
                         "weedy",
                         "reeky",
                         "measled",
                         "spur-galled",
                         "mangled",
                         "impertinent",
                         "bootless",
                         "toad-spotted",
                         "hasty-witted",
                         "horn-beat",
                         "yeasty",
                         "boil-brained",
                         "tottering",
                         "hedge-born",
                         "hugger-muggered",
                         "elf-skinned"],
                  amt = ["accumulation",
                         "bucket",
                         "coagulation",
                         "enema-bucketful",
                         "gob",
                         "half-mouthful",
                         "heap",
                         "mass",
                         "mound",
                         "petrification",
                         "pile",
                         "puddle",
                         "stack",
                         "thimbleful",
                         "tongueful",
                         "ooze",
                         "quart",
                         "bag",
                         "plate",
                         "ass-full",
                         "assload"],
                  noun = ["bat toenails",
                          "bug spit",
                          "cat hair",
                          "chicken piss",
                          "dog vomit",
                          "dung",
                          "fat-woman's stomach-bile",
                          "fish heads",
                          "guano",
                          "gunk",
                          "pond scum",
                          "rat retch",
                          "red dye number-9",
                          "Sun IPC manuals",
                          "waffle-house grits",
                          "yoo-hoo",
                          "dog balls",
                          "seagull puke",
                          "cat bladders",
                          "pus",
                          "urine samples",
                          "squirrel guts",
                          "snake assholes",
                          "snake bait",
                          "buzzard gizzards",
                          "cat-hair-balls",
                          "rat-farts",
                          "pods",
                          "armadillo snouts",
                          "entrails",
                          "snake snot",
                          "eel ooze",
                          "slurpee-backwash",
                          "toxic waste",
                          "Stimpy-drool",
                          "poopy",
                          "poop",
                          "dookie",
                          "big meaty chuds",
                          "fudge dragons",
                          "chocolate hotdogs",
                          "mud monkeys",
                          "craptacular carpet droppings",
                          "jizzum",
                          "cold sores",
                          "anal warts"]}).

-define(SERVER, ?MODULE).
-define(INSULTPREFIX,"insult").

start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() -> gen_server:call(?SERVER, stop).

init([]) ->
    random:seed(now()),
    %% register our insulting service
    useless_irc_services:register_service(insults,
                                          ?INSULTPREFIX,
                                          self(), node(),
                                          ?SERVER),
    {ok, #insults{}}.

% for testing purposes
handle_call(insult, _From, State) ->
    %% we also need to respond to a help message, for each plugin to define
    %% it's own help
    {reply, "Insult", State};

handle_call(stop, _From, State) ->
            {stop, normal, stopped, State}.

handle_cast(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

unique_nums(Len,N1,N2) when N1 =:= N2 ->
    unique_nums(Len,N1,random:uniform(Len));

unique_nums(_Len,N1,N2) ->
    {N1,N2}.

get_insult_start([V | _] = _Adj1) when V =:= $a orelse
                                       V =:= $i orelse
                                       V =:= $u orelse
                                       V =:= $o orelse
                                       V =:= $e ->
    "an";

get_insult_start(_Adj1) ->
    "a".

do_insult(Adj1,Amt,Adj2,Noun) ->
    string:join([get_insult_start(Adj1),Adj1,Amt,"of",Adj2,Noun]," ").

generate_insult(#insults{adj = Adj, amt = Amts, noun = Nouns}) ->
    Len = length(Adj),
    {N1, N2} = unique_nums(Len,random:uniform(Len),
                               random:uniform(Len)),
    Adj1 = lists:nth(N1,Adj),
    Adj2 = lists:nth(N2,Adj),
    Noun = lists:nth(random:uniform(length(Nouns)),Nouns),
    Amt = lists:nth(random:uniform(length(Amts)),Amts),
    do_insult(Adj1,Amt,Adj2,Noun).

handle_info({run, _Request, Chan, User, From}, State) when Chan =:= "" ->
    % you are a/an
    From ! {cmd_resp, User, Chan, "You are " ++ generate_insult(State)},
    {noreply, State};

handle_info({run, _Request, Chan, User, From}, State) ->
    % you are a/an
    From ! {cmd_resp, User, Chan, User ++ " is " ++ generate_insult(State)},
    {noreply, State};

handle_info(Msg, State) ->
    io:format("Unexpect Msg rcvd ~p~n",[Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
