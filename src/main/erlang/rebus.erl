%%%-------------------------------------------------------------------
%%% @author Olle Törnström <olle@studiomediatech.com>
%%% @copyright (C) 2011, Olle Törnström
%%% @doc
%%% TODO:
%%% @end
%%%-------------------------------------------------------------------
-module(rebus).

%% Public API
-export([start/0,
	 stop/0,
	 publish/1,
	 publish/2]).

%% Private API
-export([loop/1]).

-define(REBUS(Pid), put({?MODULE, pid}, Pid)).
-define(REBUS, get({?MODULE, pid})).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the Rebus service.
%% @end
%%--------------------------------------------------------------------
start() ->
    ?REBUS(spawn(?MODULE, loop, [[]])),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Stops the Rebus service.
%% @end
%%--------------------------------------------------------------------
stop() ->
    ?REBUS ! {stop, self()},
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Publishes the given message on the fixed public topic `all', that
%% all processes that use the module attribute `subscribes' will
%% receive.
%% @end
%%--------------------------------------------------------------------
publish(Message) ->
    publish(all, Message).

%%--------------------------------------------------------------------
%% @doc
%% Publishes the given message on the specified topic, received by
%% processes that explicit declare this topic in their `subscribes'
%% module attribute. Please note the exception that the the `all'
%% topic is always published, to all processes using the `subscribes'
%% attribute (even when it's empty).
%% @end
%%--------------------------------------------------------------------
publish(Topic, Message) ->
    ?REBUS ! {publish, Topic, Message},
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
loop(State) ->
    receive
	{stop, From} ->
	    From ! State;
	Other ->
	    error_logger:debug_report([{other_message, Other}, {state, State}]),
	    loop(State)
    end.
