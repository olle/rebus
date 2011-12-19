-module(foo).
-export([start/0,
         stop/1]).

-subscribe([foo]).
                                       
start() ->
    spawn(fun loop/0).

stop(Pid) ->
    Pid ! {stop, self()},
    receive
        Result ->
            Result
    end.

loop() ->
    loop([]).

loop(State) ->
    receive
        {stop, From} ->
            From ! State;
        
        Msg ->
            error_logger:info_report([foo, received, {message, Msg}]),
            loop([Msg | State])
    end.
