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
    ?SLEEP(10), %% try to align monitor and dispatching processes
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
%% Simply publishes the given `message' to all processes that are
%% annotated with the module attribute `subscribes' and specified as
%% an empty list.
%% @end
%%--------------------------------------------------------------------
publish(Message) ->
    ?MODULE ! {publish, Message}.

%%--------------------------------------------------------------------
%% @doc
%% Publishes the given message on the specified `topic' to processes
%% that explicit declare this topic in their `subscribes' module
%% attribute.
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
    spawn_link(fun monitor/0),
    rebus([{subscribers, []}, {subscriptions, []}]).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
rebus(State) ->
    receive
        {debug, From} ->
            error_logger:info_report([debug, {state, State}]),
            From ! {ok, State},
            rebus(State);

        {stop, From} ->
            error_logger:info_report([stop, {state, State}]),
            From ! {stopping, State};

        {publish, Message} ->
            error_logger:info_report([publish, {message, Message}, {state, State}]),
            Subscribers = proplists:get_value(subscribers, State, []),
            [Subscriber ! Message || Subscriber <- Subscribers],
            rebus(State);

        {publish, Topic, Message} ->
            error_logger:info_report([publish, {topic, Topic}, {message, Message}, {state, State}]),
            Subscriptions = proplists:get_value(subscriptions, State, []),
            Subscribers = proplists:get_value(Topic, Subscriptions, []),
            [Subscriber ! {Topic, Message} || Subscriber <- Subscribers],
            rebus(State);

        {subscribe, Process, Topics} ->
            error_logger:info_report([subscribe, {process, Process}, {topics, Topics}, {state, State}]),
            NewState = add_subscriber(Process, Topics, State),
            rebus(NewState);

        _Other ->
            %%error_logger:info_report([other, {message, Other}, {state, State}]),
            rebus(State)
    end.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
add_subscriber(Subscriber, Topics, State) ->
    Subscribers = proplists:get_value(subscribers, State, []),
    NewSubscribers = add_to_subscribers(Subscriber, Subscribers),

    Subscriptions = proplists:get_value(subscriptions, State, []),
    NewSubscriptions = add_to_subscriptions(Subscriber, Topics, Subscriptions),
    [{subscribers, NewSubscribers}, {subscriptions, NewSubscriptions}].

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
add_to_subscribers(Pid, Pids) ->
    (Pids -- [Pid]) ++ [Pid].

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
add_to_subscriptions(_Pid, [], Subscriptions) ->
    Subscriptions;

add_to_subscriptions(Pid, [Topic | Topics], Subscriptions) ->
    NewSubscriptions = add_to_subscription(Pid, Topic, Subscriptions),
    add_to_subscriptions(Pid, Topics, NewSubscriptions).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
add_to_subscription(Pid, Topic, Subscriptions) ->    
    NewSubscriptions = 
        case proplists:get_value(Topic, Subscriptions) of
            undefined ->
                Subscriptions ++ [{Topic, [Pid]}];
	    
            Subscribers ->
                NewSubscription = {Topic, (Subscribers -- [Pid]) ++ [Pid]},
                Rest = proplists:delete(Topic, Subscriptions),
		Rest ++ [NewSubscription]
        end,
    NewSubscriptions.

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

        _Ignored ->
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
            case proplists:get_value(subscribe, Attributes) of
                undefined ->
                    ignore;
                
                [] ->
                    ?MODULE ! {subscribe, Pid, [all]};
                
                Topics ->
                    ?MODULE ! {subscribe, Pid, Topics}
            end
    end.

