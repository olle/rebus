-module(foobar).
-export([start/0,
         stop/1]).

-subscribe([foo, bar]).

start() ->
    spawn(fun loop/0).

stop(Pid) ->
    Pid ! {stop, self()},
    receive
        State ->
            {ok, State}
    end.

loop() ->
    loop([]).

loop(State) ->
    receive
        {stop, From} ->
            From ! State;
        Message ->
            loop([Message | State])
    end.
