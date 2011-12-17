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
    ?assertMatch([1], rebus:add_to_subscribers(1, [])),
    ?assertMatch([1, 2], rebus:add_to_subscribers(2, [1])),
    ?assertMatch([1, 2, 3], rebus:add_to_subscribers(3, [1, 2])),
    ?assertMatch([2, 3, 1], rebus:add_to_subscribers(1, [1, 2, 3])).

add_to_subscription_test() ->
    ?assertMatch([{a, [1]}], rebus:add_to_subscription(1, a, [], [])),
    ?assertMatch([{a, [1]}, {b, [1]}], rebus:add_to_subscription(1, b, [{a, [1]}], [])),
    ?assertMatch([{a, [2, 3, 1]}], rebus:add_to_subscription(1, a, [{a, [2, 3]}], [])).

add_to_subscriptions_() ->
    ?assertMatch([{a, [1]}], rebus:add_to_subscriptions(1, [a], [])),
    ?assertMatch([{a, [1]}, {b, [1]}], rebus:add_to_subscriptions(1, [a, b], [])).

lifecycle_() ->
    ?assertMatch(ok, rebus:start()),

    Foo = foo:start(),

    %% Listeners are added asynchronously, so in tests we must compensate
    ?SLEEP(1),
    rebus:debug(),

    rebus:publish(topic, message),
    rebus:publish(foo, message),
    rebus:publish(bar, message),
    rebus:publish(baz, message),
    rebus:publish(message),

    %% Message dispatching is asynchronous too, so in tests we compensate again
    ?SLEEP(1),

    ?assertMatch({ok, [message, {foo, message}]}, foo:stop(Foo)),

    ?assertMatch(ok, rebus:stop()).

