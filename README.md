Rebus
=====

A stupid simple, internal, attribute-driven, pub/sub message bus for Erlang.
----------------------------------------------------------------------------

What! Why? You think. Well, perhaps it's just me, but as I looked for simple
internal pub/sub in Erlang, and got `gen_event` thrown in my face, I could not
help but feel a bit like **the nineties called, and they want their
Erlang/OTP libraries back**. But kidding aside, it is somewhat rather big for
something that could be so slender.

Another approach
----------------

The idea of Rebus is to make pub/sub as simple as possible. Just one message
dispatching API - stupid simple: `rebus:publish/1` and `rebus:publish/2`.

Message subscription should be equally simple, using a single simple module
attribute: `subscribe`.

Also, pure Erlang only, it should be simple to use without any requirement to
haul the whole OTP stack into the mix. Lightweight, anyone?

Does it work?
-------------

Well yes, of course. The Rebus event bus (or message bus) have to be started.
Typically by a supervised main application. It is of course free to choose.

    start_link(_Args) ->
      %% …other initialization stuff
      rebus:start().

Now any spawned processes from modules that are annotated will be added as
subscribers to the Rebus service.

    -module(animal_counter).
    -export([start/0]).
    -subscribe([dogs, cats]).

Subscription is easy, any `atom()` can be subscribed to as a topic. All
subscribers are also notified on the _global_ empty topic.

A module can also define a subscription to the empty list (`[]`) - meaning
that it will only receive the globally published messages. In our example,
the following clauses are valid and would be received by the module above:

    receive
      {cats, Msg} ->
        %% …handle messages on the `cats` topic
        listen();

      {dogs, Msg} ->
        %% …handle messages on the `dogs` topic
        listen();

      _Other ->
        %% …handle other messages, for example globally published ones
        listen()
    end.

Rebus can of course also be used in a real Erlang/OTP application. The
published messages will then simply arrive at the `handle_info` function.

    handle_info({cats, Msg}, State) ->
      %% …do something
      {no reply, State};

    handle_info({dogs, Msg}, State) ->
      %% …do something
      {no reply, State}.

Message publishing is very easy. Any process of any module can simply use the
public API, as long as the service is running, to publish messages on any
topics.

    add_dog(Dog) ->
      %% ..does something
      rebus:publish(dogs, {added, Dog}),
      rebus:publish({animal, added}).

    add_cat(Cat) ->
      %% …does something
      rebus:publish(cats, {added, Cat}),
      rebus:publish({animal, added}).

Is it all butterflies and jellybeans?
-------------------------------------

Well, no. Rebus is still very very alpha, and in no way production ready, but
I hope that it is a good enough proof-of-concept. Hopefully it may draw some
interest gather a little momentum for further development.

This means **you can help**. Just fork the git repository and hack away. I'll
be glad to merge any changes and solutions that keep to the original idea of
building a stupid simple internal pub/sub solution for Erlang.

## Getting started!

This project is now using `rebar` as build and test-tool. There are even some
EUnit tests. Please, go ahead.

    rebar clean compile eunit
    
Good luck!
