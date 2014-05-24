**[Quickstart](#quickstart)** |
**[Configuration](#configuration)** |
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
## Configuration

The following variables are configurable (located in ebin/useless_irc.app):

- IRC server ip address
- IRC server port
- IRC channel to join
- IRC nick to use
- you can add other configurations like tokens, keys etc..
	

## Plugins

Command to the bot use a prefix of `,` (i will make that configurable as an environment variable).

#### Weather plugin

#### Insult plugin

#### Magic 8 ball plugin

#### Timer plugin

#### Julia repl plugin

## Adding plugins

TODO