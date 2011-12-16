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

-define(SLEEP(Millis), receive after Millis -> ok end).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the Rebus service.
%% @end
%%--------------------------------------------------------------------
start() ->
    Dispatching = dispatching(start),
    _Monitoring = monitoring({start, Dispatching}),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Stops the Rebus service.
%% @end
%%--------------------------------------------------------------------
stop() ->
    dispatching(stop),
    monitoring(stop),
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
    Pid = get(dispatch),
    Pid ! {publish, Topic, Message}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
dispatching(start) ->
    Pid = spawn(fun dispatch/0),
    put(dispatch, Pid),
    Pid;

dispatching(stop) ->
    Pid = get(dispatch),
    Pid ! {stop, self()},
    receive
        {stopping, _State} ->
            erase(dispatch),
            ok
    after 123 ->
            {error, "No response"}
    end.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
dispatch() ->
    process_flag(sensitive, true),
    dispatch([]).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
dispatch(State) ->
    receive
        {stop, From} ->
            From ! {stopping, State};

        {add, Process} ->
            error_logger:info_report([dispatch, {add, Process}, {state, State}]),
            dispatch(State ++ Process);

        {publish, Topic, Message} ->
            [catch P ! {Topic, Message} || P <- State],
            dispatch(State);

        Other ->
            error_logger:info_report([dispatch, {message, Other}, {state, State}]),
            dispatch(State)
    end.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
monitoring({start, Dispatcher}) when is_pid(Dispatcher) ->
    Pid = spawn(fun monitor/0),
    put(monitor, Pid),
    ?SLEEP(123),
    Pid ! {dispatcher, Dispatcher},
    Pid;

monitoring(stop) ->
    Pid = get(monitor),
    Pid ! {stop, self()},
    receive
        {stopping, _State} ->
            erase(monitor),
            ok
    after 123 ->
            {error, "No response"}
    end.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
monitor() ->
    process_flag(sensitive, true),
    erlang:trace_pattern({'_', '_', '_'}, [{'_', [], []}], [local]),
    erlang:trace(all, true, [procs]),
    erlang:trace(self(), false, [procs]),
    monitor([]),
    ok.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
monitor(State) ->
    receive
        {stop, From} ->
            From ! {stopping, State};

        {dispatcher, Dispatcher} ->
            monitor(Dispatcher);

        {trace, _, spawn, Pid, _} ->
            monitor_process(Pid, State),
            monitor(State);

        _ ->
            monitor(State)
    end.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
monitor_process(Pid, Dispatcher) ->
    case erlang:process_info(Pid) of
        undefined ->
            ignore;
        
        Props ->
            error_logger:info_report([{'PROPERTIES:', Props}]),
            Tmp = proplists:get_value(current_function, Props),
            error_logger:info_report([{'CURRENT_FUNCTION:', Tmp}]),
            {M, _F, _A} = proplists:get_value(current_function, Props, erlang),
            error_logger:info_report([{'MODULE:', M}]),
            Attributes = M:module_info(attributes),
            error_logger:info_report([{'ATTRIBUTES:', Attributes}]),
            case proplists:get_value(subscribes, Attributes) of
                undefined ->
                    ignore;
                
                _ ->
                    Dispatcher ! {add, Pid}
            end
    end.
