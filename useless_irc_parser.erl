-module(useless_irc_parser).
-compile(export_all). % replace with export later

-define(BOTPREFIX,":,").

% return a list containing tuples with nick, user and host
% or tuple containing the server
parse_prefix(Prefix) ->
    % try to split tokens, if it's one element then it's the server
    case string:tokens(Prefix,"!|@") of
        [Nick, User, Host] -> lists:zip([nick,user,host], [Nick, User, Host]);
        [Server] -> {server, Server}
    end.

handle_prefix([[$:|Prefix] | Rest]) ->
    [parse_prefix(Prefix) | Rest];

handle_prefix(Tokens) ->
    Tokens.

parse_msg(Msg) ->
    MsgStr = binary_to_list(Msg),
    StripedMsg = string:substr(MsgStr,1,string:len(MsgStr) - 2),
    Tokens = string:tokens(StripedMsg," "),
    Prefix = handle_prefix(Tokens),
    parse_params(Prefix).

parse_params([{server, Server}, "462", Nick | _Tokens]) ->
    io:format("Nick ~p is already registered on server ~p~n",
              [Nick,Server]),
    %% disconnect here TODO
    ok;

parse_params([{server, _Server} | _Tokens]) ->
    ignored;

parse_params([[{nick, Nick},{_, _},{_,_ }] | ["PRIVMSG" | Rest]]) ->
    {msg, Nick, Rest};

parse_params([[{nick, _Nick},{_, _},{_,_ }] | _Tokens]) ->
    ignored;

parse_params(Command) ->
    process_cmd(Command).

process_cmd(["PING" | Server]) ->
    %% string the : in server name
    {response, "PONG " ++ string:substr(hd(Server),2)};

process_cmd(_Command) ->
    ignored.

process_channel_msg(Msg) ->
    case re:run(hd(Msg),?BOTPREFIX) of
        {match,_} -> process_bot_msg(string:join(Msg," "));
        _ -> io:format("Channel Msg ignoring ~p~n",[Msg]),
             ""
    end.

process_bot_msg(Str) ->
    %% remove bot prefix, XXX should use pattern matching on strings instead of re
    PrefixLessStr = string:strip(re:replace(Str,?BOTPREFIX,"",[{return, list}])),
    Tokenized = string:tokens(PrefixLessStr," "),
    [hd(Tokenized) | string:join(tl(Tokenized)," ")].

is_ctcp([Msg]) when is_list(Msg) ->
    [hd(Msg)] =:= ":" andalso hd(tl(Msg)) =:= 1 andalso lists:last(Msg) =:= 1;

is_ctcp(_) ->
    false.

process_private_msg(Msg) ->
    Str = string:join(Msg," "),
    case is_ctcp(Msg) of
       %% ignore ctcp messages
       true -> io:format("CTCP Msg ignored ~p~n",[Str]);
       false -> case re:run(hd(Msg),?BOTPREFIX) of
                    {match,_} -> process_bot_msg(Str);
                    _ -> io:format("Private Msg ignoring ~p~n",[Msg]),
                         ""
                end
    end.

