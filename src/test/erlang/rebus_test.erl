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

remove_from_subscriptions_test() ->
    ?assertMatch([{a, [2, 3]}, {b, [2, 3]}, {c, [3]}],
		 rebus:remove_from_subscriptions(1, [{a, [1, 2, 3]}, {b, [2, 3]}, {c, [3]}])),
    ?assertMatch([{a, [3]}, {b, [3]}, {c, [3]}],
		 rebus:remove_from_subscriptions(2, [{a, [2, 3]}, {b, [2, 3]}, {c, [3]}])),
    ?assertMatch([],
		 rebus:remove_from_subscriptions(3, [{a, [3]}, {b, [3]}, {c, [3]}])).

remove_from_subscribers_test() ->
    ?assertMatch([2, 3],
		 rebus:remove_from_subscribers(1, [1, 2, 3])),
    ?assertMatch([3],
		 rebus:remove_from_subscribers(2, [2, 3])),    
    ?assertMatch([],
		 rebus:remove_from_subscribers(3, [3])).

lifecycle_test() ->
    ?assertMatch(ok, rebus:start()),

    Foo = foo:start(),
    Bar = bar:start(),
    Foobar = foobar:start(),
    
    %% Test listeners spawn their own processes, we give them some time to start...
    ?SLEEP(1000),

    rebus:publish(foo, message),
    rebus:publish(void, message),
    rebus:publish(bar, message),
    rebus:publish(void, message),
    rebus:publish(message),

    ?SLEEP(1000),

    ?assertMatch([message, {foo, message}], foo:stop(Foo)),
    ?assertMatch([message, {bar, message}], bar:stop(Bar)),
    ?assertMatch([message, {bar, message}, {foo, message}], foobar:stop(Foobar)),

    ?assertMatch(ok, rebus:stop()).

