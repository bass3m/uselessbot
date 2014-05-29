**[Quickstart](#quickstart)** |
**[Configuration](#configuration)** |
**[Plugins](#plugins)** |
**[Adding plugins](#adding-plugins)**

# uselessbot
------------


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


#### Plugin help
Commands to the bot use a prefix of `,` (i will make that configurable as an environment variable).
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

You can invoke the insult plugin in the channel or you can send the bot a privage message, in order to generate a fresh insult for your pleasure.

```
%% in your IRC client as nick testnick in channel
,insult
%% IRC bot will generate a fresh insult for you
testnick is a lumpish thimbleful of fly-bitten craptacular carpet droppings
%% You can send the bot a private message as well, uselessbot is bot nick in this example
/msg uselessbot ,insult
%% the bot will respond with 
You are a low-quality accumulation of vain bat toenails
```

#### Magic 8 ball plugin

Ask the magic 8 ball a question by prefixing a question with `,8ball` followed by your question.
For example:

```
%% in your IRC client ask the bot a question
,8ball should i learn Elixir ?
%% the magic 8 ball responds
As I see it, yes.
```

#### Timer plugin

The timer plugin allows a user to set a timer for a time period expressed as `hh:mm:ss` followed by a reminder message. The bot will send a privage message to that user after the timer expires.

```
%% send a private message to the bot to set a timer for 10 secs 0:0:10
%% and when the timer expires to remind me to 'wake me up'
/msg test ,timer 0:0:10 wake me up
%% after 10 seconds the bot messages the nick with the message
wake me up
```

#### Julia repl plugin

## Adding plugins

Plugins are gen_servers that register with the bot. Plugins are required to implement a couple of messages in order to handle the input from the IRC client and forward the output back to the bot.

#### Registering a plugin
Each plugin can register itself with a prefix that is used to invoke it.
For example:

```
%% register our timer service
useless_irc_services:register_service(timer,"timer",self(), node(),SERVER)
```
Registers plugin **timer** with a prefix of **"timer"** which can be invoked by `,timer`.

#### Plugin messages
TODO