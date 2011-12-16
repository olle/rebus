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
         publish/2,
	 debug/0]).

-define(SLEEP(Millis), receive after Millis -> ok end).

-record(state, {subscribers :: [pid()],
		monitor :: pid()}).

%%%===================================================================
%%% API
%%%===================================================================

debug() ->
    ?MODULE ! {debug, self()},
    receive
	{ok, State} ->
	    {ok, State}
    after 100 ->
	    {error, "No response"}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Starts the Rebus service.
%% @end
%%--------------------------------------------------------------------
start() ->
    register(?MODULE, spawn(fun rebus/0)),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Stops the Rebus service.
%% @end
%%--------------------------------------------------------------------
stop() ->
    ?MODULE ! {stop, self()},
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
    ?MODULE ! {publish, Topic, Message}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
rebus() ->
    process_flag(sensitive, true),
    Pid = spawn_link(fun monitor/0),
    rebus(#state{monitor = Pid, subscribers = []}).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
rebus(State) ->
    receive
	{debug, From} ->
	    From ! {ok, State},
	    rebus(State);

        {stop, From} ->
            From ! {stopping, State};

        {publish, Topic, Message} ->
            [P ! {Topic, Message} || P <- State#state.subscribers],
            rebus(State);
	
        {subscribe, Process} ->
            error_logger:info_report([dispatch, {add, Process}, {state, State}]),
	    Subscribers = State#state.subscribers,
            rebus(#state{subscribers = [Process | Subscribers]});
	
        Other ->
            error_logger:info_report([other, {message, Other}, {state, State}]),
            rebus(State)
    end.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
monitor() ->
    process_flag(sensitive, true),
    erlang:trace_pattern({'_', '_', '_'}, [{'_', [], []}], [local]),
    erlang:trace(all, true, [procs]),
    erlang:trace(self(), false, [procs]),
    monitor([]).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
monitor(State) ->
    receive
        Spawn = {trace, _, spawn, Pid, _} ->
	    error_logger:info_report([{'SPAWN:', Spawn}]),
            monitor_process(Pid),
            monitor(State);
	
        Other ->
	    error_logger:info_report([{'OTHER:', Other}]),
            monitor(State)
    end.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
monitor_process(Pid) when is_pid(Pid) ->
    case erlang:process_info(Pid) of
        undefined ->
            ignore;
        
        Props ->
            {M, _F, _A} = proplists:get_value(current_function, Props, erlang),
            Attributes = M:module_info(attributes),
            case proplists:get_value(subscribes, Attributes) of
                undefined ->
		    error_logger:info_report([{subscribes, undefined}, {process, Pid}, {module, M}]),
                    ignore;
                
                Topics ->
		    error_logger:info_report([{subscribes, Topics}, {process, Pid}, {module, M}]),
                    ?MODULE ! {subscribe, Pid}
            end
    end.

