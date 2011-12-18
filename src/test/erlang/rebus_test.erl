%%%-------------------------------------------------------------------
%%% @author Olle Törnström <olle@studiomediatech.com>
%%% @copyright (C) 2011, Olle Törnström
%%% @doc
%%% TODO:
%%% @end
%%%-------------------------------------------------------------------
-module(rebus_test).

-include_lib("eunit/include/eunit.hrl").

-define(SLEEP(Millis), receive after Millis -> ok end).

add_to_subscribers_test() ->
    ?assertMatch([1], 
		 rebus:add_to_subscribers(1, [])),

    ?assertMatch([1, 2],
		 rebus:add_to_subscribers(2, [1])),

    ?assertMatch([1, 2, 3],
		 rebus:add_to_subscribers(3, [1, 2])),

    ?assertMatch([2, 3, 1],
		 rebus:add_to_subscribers(1, [1, 2, 3])).

add_to_subscription_test() ->
    ?assertMatch([{a, [1]}],
		 rebus:add_to_subscription(1, a, [])),

    ?assertMatch([{a, [1]}, {b, [1]}],
		 rebus:add_to_subscription(1, b, [{a, [1]}])),

    ?assertMatch([{a, [2, 3, 1]}],
		 rebus:add_to_subscription(1, a, [{a, [2, 3]}])).

add_to_subscriptions_test() ->
    ?assertMatch([{a, [1]}],
		 rebus:add_to_subscriptions(1, [a], [])),

    ?assertMatch([{a, [1]}, {b, [1]}],
		 rebus:add_to_subscriptions(1, [a, b], [])),

    ?assertMatch([{a, [1]}, {b, [1, 2]}, {c, [2]}],
		 rebus:add_to_subscriptions(2, [b, c], [{a, [1]}, {b, [1]}])).

lifecycle_test() ->
    ?assertMatch(ok, rebus:start()),

    Foo = foo:start(),
    Bar = bar:start(),
    Foobar = foobar:start(),

    %% Listeners are added asynchronously, so in tests we must compensate
    ?SLEEP(1),
    rebus:debug(),

    rebus:publish(foo, message),
    rebus:publish(void, message),
    rebus:publish(bar, message),
    rebus:publish(void, message),
    rebus:publish(message),

    %% Message dispatching is asynchronous too, so in tests we compensate again
    ?SLEEP(1),

    %% Our listeners save their messages LIFO

    ?assertMatch({ok, [message, {foo, message}]},
		 foo:stop(Foo)),

    ?assertMatch({ok, [message, {bar, message}]},
		 bar:stop(Bar)),

    ?assertMatch({ok, [message, {bar, message}, {foo, message}]},
		 foobar:stop(Foobar)),

    ?assertMatch(ok, rebus:stop()).

