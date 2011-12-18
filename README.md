Rebus
=====

A stupid simple, internal, attribute-driven, pub/sub message bus for Erlang!
----------------------------------------------------------------------------

What! Why? You think. Well, perhaps it's just me, but as I looked for simple
internal pub/sub in Erlang, and got `gen_event` thrown in my face, I could not
help but feel a bit like **the nineties called, and they want their
Erlang/OTP libraries back**. I'm sorry, it is just too much for too little.

Don't get me wrong though. I have the greatest respect for the Erlang developer
community and the developers (as of writing R15B - great work, yay!). But it
seems to me that simple pub/sub is somehow missing.

The general idea of Rebus is to make pub/sub as simple as possible. Just one
global message dispatching API - stupid simple: `rebus:publish/1`, for _global_
messages, and `rebus:publish/2` for messages published on a specific topic.

Of course message subscription should be equally simple, using a single simple
module attribute: `subscribe`.

Getting started
---------------

Using Rebus currently requires that one manually starts the _service_. This
can (or should) of course be wrapped into an Erlang/OTP application, ensuring
that the service is available in the application.

    start_link(_Args) ->
      %% …some other initializations
      rebus:start().

This starts the Rebus service that will listen for processes as they are
spawned.

Any module you write can simply be annotated with the attribute `subscribe`,
with either just an empty list (`[]`) or an `atom()` list of the topics to
subscribe to.

    -module(counter).
    -export([start/0]).
    -subscribe([cats, dogs]).

    start() ->
      spawn(fun listen/0).
    
    listen() ->
      receive
        {cats, Msg} ->
          %% …do something
          listen();

        {dogs, Msg} ->
          %% …do something else
          listen();

        _Other ->
          %% …ignored
          listen()
      end.

A module may also subscribe to global messages by simply subscribing with an
empty list as attribute definition.

    -subscribes([]).

Rebus can of course also be used together with Erlang/OTP, so the published
messages arrive to the `handle_info` function.

    handle_info({cats, Msg}, State) ->
      %% …do something
      {no reply, State};

    handle_info({dogs, Msg}, State) ->
      %% …do something
      {no reply, State}.

Message publishing is, like I've already mentioned, very easy. Any process of any
module can simply use the public API, as long as the service is running.

    add_dog(Dog) ->
      %% ..does something
      rebus:publish(dogs, {added, Dog}),
      rebus:publish({animal, added}).

    add_cat(Cat) ->
      %% …does something
      rebus:publish(cats, {added, Cat}),
      rebus:publish({animal, added}).

What now?
---------

Well, Rebus is still very very alpha, and in no way production ready, but I hope
that it is a good enough proof-of-concept to gather some momentum for further
development.

This means **you can help**. Just fork the git repository and hack away. I'll
be glad to merge any changes and solutions that keep to the original idea of
building a stupid simple internal pub/sub solution for Erlang.
