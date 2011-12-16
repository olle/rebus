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

lifecycle_test() ->
    ?assertMatch(ok, rebus:start()),

    Foo = foo:start(),

    %% ?SLEEP(100),

    rebus:publish(foo, message),
    %% rebus:publish(bar, message),
    %% rebus:publish(baz, message),
    %% rebus:publish(message),

    %% ?SLEEP(100),
    
    %% A ! {stop, self()},
    %% receive
    %%     AState ->
    %%         ?assertMatch([{all, message}, {foo, message}, {bar, message}], AState)
    %% end,

    ?assertMatch({ok, state}, foo:stop(Foo)),
    
    ?assertMatch(ok, rebus:stop()).
