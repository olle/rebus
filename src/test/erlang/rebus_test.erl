%%%-------------------------------------------------------------------
%%% @author Olle Törnström <olle@studiomediatech.com>
%%% @copyright (C) 2011, Olle Törnström
%%% @doc
%%% TODO:
%%% @end
%%%-------------------------------------------------------------------
-module(rebus_test).

-include_lib("eunit/include/eunit.hrl").

-define(SLEEP(Time), receive after Time -> ok end).

-subscribes([foo, bar]).

lifecycle_test() ->
    ?assertMatch(ok, rebus:start()),

    A = spawn(fun a/0),

    rebus:publish(foo, message),
    rebus:publish(bar, message),
    rebus:publish(baz, message),
    rebus:publish(message),

    ?SLEEP(123),

    A ! {stop, self()},
    receive
        AState ->
            ?assertMatch([{all, message}, {foo, message}, {bar, message}], AState)
    end,

    ?assertMatch(ok, rebus:stop()).

a() ->
    a([]).

a(State) ->
    receive
        stop ->
            State;
        Message ->
            a([Message | State])
    end.
