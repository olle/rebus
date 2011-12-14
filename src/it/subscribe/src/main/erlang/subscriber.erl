%%%-------------------------------------------------------------------
%%% @author Olle Törnström <olle@studiomediatech.com>
%%% @copyright (C) 2011, Olle Törnström
%%% @doc
%%% Test module, implements a very simple message subscriber - I'm
%%% telling you, veeeeery simple.
%%% @end
%%%-------------------------------------------------------------------
-module(subscriber).

-export([start/0, stop/0]).

%% -subscribe([topic1, topic2]).

start() ->
    Pid = spawn(?MODULE, loop, []),
    put(pid, Pid).

stop() ->
    Pid = get(pid),
    Pid ! {stop, self()}.

%% ---- private functions

loop(State) ->
    receive
	{stop, From} ->
	    From ! State;
	Message ->
	    NextState = State ++ Message,
	    loop(NextState)
    end.
