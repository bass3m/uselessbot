**[Quickstart](#quickstart)** |
**[Plugins](#plugins)** |
**[Adding plugins](#adding-plugins)**

# uselessbot :
==========

An IRC bot that supports plugins. It was a project for learning Erlang. I'm sure the code is far from idiomatic Erlang, but it was tons of fun nonetheless.

## Quickstart

```
	%% From the erlang shell
	%% start useless bot and connect to IRC server
	> application:start(useless_irc).
	%% join #testme IRC channel 
	> useless_irc:join("#testme").
	%% login as user test
	> useless_irc:login("test").
```

## Plugins

TODO

## Adding plugins

TODO