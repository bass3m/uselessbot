**[Quickstart](#quickstart)** |
**[Configuration](#configuration)** |
**[Plugins](#plugins)** |
**[Adding plugins](#adding-plugins)**

# uselessbot
------------


An IRC bot that supports plugins. It was a project for learning Erlang. I'm sure the code is far from idiomatic Erlang, but it was tons of fun nonetheless.
Each plugin can register itself with the prefix that is used to invoke it.

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
In your IRC client of choice, you can type `,help` and that will iterate over all the installed plugins and provide some basic help on how to invoke them.

```
[11:56:42]  <test>   Plugin: timer with prefix : timer
[11:56:42]  <test>	 Plugin: magic_8ball with prefix : 8ball
[11:56:42]  <test>	 Plugin: weather with prefix : weather
[11:56:42]  <test>	 Plugin: insults with prefix : insult
[11:56:42]  <test>	 Plugin: julia with prefix : julia
```

#### Weather plugin

#### Insult plugin

#### Magic 8 ball plugin

#### Timer plugin

#### Julia repl plugin

## Adding plugins

TODO