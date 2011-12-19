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

-type topic()        :: atom().
-type message()      :: term().
-type subscriber()   :: pid().
-type subscription() :: {topic(), [pid()]}.

-record(state, {
          subscribers    = []        :: [subscriber()],
          subscriptions  = []        :: [subscription()],
          monitor        = undefined :: undefined | pid(),
          starter        = undefined :: undefined | pid()}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the Rebus service, making it ready for pub/sub use.
%% @end
%%--------------------------------------------------------------------
-spec start() ->
                   ok.
start() ->
    register(?MODULE, spawn(fun rebus/0)),
    ?MODULE ! {waiting, self()},
    receive
        ready ->
            ok
    end.

%%--------------------------------------------------------------------
%% @doc
%% Stops the Rebus service, shutting down the pub/sub event bus.
%% @end
%%--------------------------------------------------------------------
-spec stop() ->
                  ok.
stop() ->
    ?MODULE ! {stop, self()},
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Publishes the given `message' to all processes that are annotated
%% with the module attribute `subscribes' and an empty list. This is
%% simple global pub/sub.
%% @end
%%--------------------------------------------------------------------
-spec publish(message()) ->
                     ok.
publish(Message) ->
    ?MODULE ! {publish, Message},
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Publishes the given message on the specified `topic' to processes
%% that explicit declare this topic in their `subscribes' module
%% attribute.
%% @end
%%--------------------------------------------------------------------
-spec publish(topic(), message()) ->
                     ok.
publish(Topic, Message) ->
    ?MODULE ! {publish, Topic, Message},
    ok.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
debug() ->
    ?MODULE ! {debug, self()},
    receive
        {ok, State} ->
            {ok, State}
    after 100 ->
            {error, "No response"}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
-spec rebus() ->
                   ok.
rebus() ->
    process_flag(sensitive, true),
    Monitor = spawn_link(fun monitor/0),    
    rebus(#state{monitor = Monitor}).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------                 
rebus(State) ->
    receive
        {debug, From} ->
            error_logger:info_report([debug, {state, State}]),
            From ! {ok, State},
            rebus(State);

        {waiting, From} ->
            State#state.monitor ! {waiting, self()},
            rebus(#state{starter = From});

        ready ->
            State#state.starter ! ready,
            rebus(State);

        {stop, From} ->
            From ! {stopping, State};

        {publish, Message} ->
            error_logger:info_report([publish, {message, Message}, {state, State}]),
            [Subscriber ! Message || Subscriber <- State#state.subscribers],
            rebus(State);

        {publish, Topic, Message} ->
            error_logger:info_report([publish, {topic, Topic}, {message, Message}, {state, State}]),
            Subscribers = proplists:get_value(Topic, State#state.subscriptions, []),
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
-spec add_subscriber(subscriber(), [topic()], #state{}) ->
                            #state{}.
add_subscriber(Subscriber, Topics, #state{subscribers = Subscribers,
                                          subscriptions = Subscriptions}) ->
    NewSubscribers = add_to_subscribers(Subscriber, Subscribers),
    NewSubscriptions = add_to_subscriptions(Subscriber, Topics, Subscriptions),
    #state{subscribers = NewSubscribers, subscriptions = NewSubscriptions}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
-spec add_to_subscribers(subscriber(), [subscriber()]) ->
                                [subscriber()].
add_to_subscribers(Pid, Pids) ->
    (Pids -- [Pid]) ++ [Pid].

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
-spec add_to_subscriptions(subscriber(), [topic()], [subscription()]) ->
                                  [subscription()].
add_to_subscriptions(_Pid, [], Subscriptions) ->
    Subscriptions;

add_to_subscriptions(Pid, [Topic | Topics], Subscriptions) ->
    NewSubscriptions = add_to_subscription(Pid, Topic, Subscriptions),
    add_to_subscriptions(Pid, Topics, NewSubscriptions).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
-spec add_to_subscription(subscriber(), topic(), [subscription()]) ->
                                 [subscription()].
add_to_subscription(Pid, Topic, Subscriptions) ->    
    case proplists:get_value(Topic, Subscriptions) of
        undefined ->
            Subscriptions ++ [{Topic, [Pid]}];
        
        Subscribers ->
            NewSubscription = {Topic, (Subscribers -- [Pid]) ++ [Pid]},
            Rest = proplists:delete(Topic, Subscriptions),
            Rest ++ [NewSubscription]
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
        {waiting, From} ->
            From ! ready,
            monitor(State);
        
        {trace, _, spawn, Pid, _} ->
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
                    error_logger:info_report([spawned, {module, M}, {process, Pid}, {topics, all}]),
                    ?MODULE ! {subscribe, Pid, [all]};

                Topics ->
                    error_logger:info_report([spawned, {module, M}, {process, Pid}, {topics, Topics}]),
                    ?MODULE ! {subscribe, Pid, Topics}
            end
    end.

