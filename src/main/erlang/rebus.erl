%%%=============================================================================
%%% @author Olle Törnström <olle@studiomediatech.com>
%%% @copyright (C) 2011, Olle Törnström
%%% @doc
%%% Rebus is a plain-old-erlang service that allows for stupid simple internal
%%% Erlang pub/sub messaging - it does not get simpler than this.
%%%
%%% First, a very simple public interface for publishing: `publish/1' and 
%%% `publish/2'. Subscribing is also very easy, just annotate any module with
%%% the attribute `-subscribe' and all it's spawned processes will receive
%%% published messages.
%%%
%%% The Rebus service is built to use two services, one to monitor process
%%% spawning and another to manage message notification. Start the Rebus
%%% service like this:
%%%
%%% ```
%%%   rebus:start().
%%% '''
%%%
%%% It will start the Rebus processes and register the service's internal
%%% interface with the name `rebus'. Pleas note, this is only for internal
%%% messaging and users should always use the public APIs.
%%%
%%% In order to keep some level of control of who's receiveing what, a simple
%%% concept of topics are used by publishers and receivers. So message sending
%%% is either done globally, or with a specified `topic'.
%%%
%%% ```
%%%   rebus:publish(gold, "This is gold leader, stay in formation, over."),
%%%   rebus:publish(red, "This is red leader, prepare to engage."),
%%%   rebus:publish("Look at the size of that thing!").
%%% '''
%%%
%%% Notice the two first messages being published to the topics `gold' and `red'
%%% respectively. Here only processes subscribing to those topics will get
%%% notified. The last, message on the other hand, will be notified to all
%%% processes annotated with the `-subscribe' attribute, even those subscribing
%%% to just an empty list.
%%% @end
%%%=============================================================================
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
          tracer         = undefined :: undefined | pid(),
          starter        = undefined :: undefined | pid()}).

-define(SERVER, ?MODULE).

%%%=============================================================================
%%% API
%%%=============================================================================
%%------------------------------------------------------------------------------
%% @doc
%% Starts the Rebus service, making it ready for pub/sub use.
%% @end
%%------------------------------------------------------------------------------
-spec start() ->
                   ok.
start() ->
    register(?SERVER, spawn(fun rebus/0)),
    ?SERVER ! {waiting, self()},
    receive
        ready ->
            ok
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Stops the Rebus service, shutting down the pub/sub event bus.
%% @end
%%------------------------------------------------------------------------------
-spec stop() ->
                  ok.
stop() ->
    ?SERVER ! {stop, self()},
    receive
	ready ->
	    ok
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Publishes the given `message' to all processes that are annotated
%% with the module attribute `subscribes' and an empty list. This is
%% simple global pub/sub.
%% @end
%%------------------------------------------------------------------------------
-spec publish(message()) ->
                     ok.
publish(Message) ->
    ?SERVER ! {publish, Message},
    ok.

%%------------------------------------------------------------------------------
%% @doc
%% Publishes the given message on the specified `topic' to processes
%% that explicit declare this topic in their `subscribes' module
%% attribute.
%% @end
%%------------------------------------------------------------------------------
-spec publish(topic(), message()) ->
                     ok.
publish(Topic, Message) ->
    ?SERVER ! {publish, Topic, Message},
    ok.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
debug() ->
    ?SERVER ! {debug, self()},
    receive
        {ok, State} ->
            {ok, State}
    after 100 ->
            {error, "No response"}
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec rebus() ->
                   ok.
rebus() ->
    Tracer = spawn_link(fun tracer/0),
    rebus(#state{tracer = Tracer}),
    ok.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
rebus(State) ->
    receive
        {debug, From} ->
            error_logger:info_report([debug, {state, State}]),
            From ! {ok, State},
            rebus(State);

        {waiting, From} ->
            State#state.tracer ! {waiting, self()},
            rebus(State#state{starter = From});

        ready ->
            State#state.starter ! ready,
            rebus(State);

        {stop, From} ->
	    State#state.tracer ! {stop, self()},
	    receive
		ready ->
		    From ! ready
	    end;

        {publish, Message} ->
            error_logger:info_report([publish, 
				      {message, Message}, 
				      {state, State}]),
            [Subscriber ! Message || Subscriber <- State#state.subscribers],
            rebus(State);

        {publish, Topic, Message} ->
            error_logger:info_report([publish, 
				      {topic, Topic}, 
				      {message, Message},
				      {state, State}]),
            Subscribers = proplists:get_value(Topic, State#state.subscriptions, []),
            [Subscriber ! {Topic, Message} || Subscriber <- Subscribers],
            rebus(State);

        {subscribe, Process, Topics} ->
            error_logger:info_report([subscribe, 
				      {process, Process}, 
				      {topics, Topics}, 
				      {state, State}]),
            NewState = add_subscriber(Process, Topics, State),
            rebus(NewState);

	{unsubscribe, Process} ->
	    error_logger:info_report([unsubscribe, 
				      {process, Process}, 
				      {state, State}]),
	    NewState = remove_subscriber(Process, State),
	    rebus(NewState);

	Other ->
            error_logger:info_report([rebus,
				      other,
				      {message, Other},
				      {state, State}]),
            rebus(State)
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec add_subscriber(subscriber(), [topic()], #state{}) ->
                            #state{}.
add_subscriber(Subscriber,
	       Topics,
	       State = #state{subscribers = Subscribers,
			      subscriptions = Subscriptions}) ->
    NewSubscribers = add_to_subscribers(Subscriber, Subscribers),
    NewSubscriptions = add_to_subscriptions(Subscriber, Topics, Subscriptions),
    State#state{subscribers = NewSubscribers, subscriptions = NewSubscriptions}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec add_to_subscribers(subscriber(), [subscriber()]) ->
                                [subscriber()].
add_to_subscribers(Pid, Pids) ->
    (Pids -- [Pid]) ++ [Pid].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec add_to_subscriptions(subscriber(), [topic()], [subscription()]) ->
                                  [subscription()].
add_to_subscriptions(_Pid, [], Subscriptions) ->
    Subscriptions;

add_to_subscriptions(Pid, [Topic | Topics], Subscriptions) ->
    NewSubscriptions = add_to_subscription(Pid, Topic, Subscriptions),
    add_to_subscriptions(Pid, Topics, NewSubscriptions).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
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

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec remove_subscriber(subscriber(), #state{}) ->
			       #state{}.
remove_subscriber(Subscriber, State = #state{subscribers = Subscribers,
					     subscriptions = Subscriptions}) ->
    NewSubscribers = remove_from_subscribers(Subscriber, Subscribers),
    NewSubscriptions = remove_from_subscriptions(Subscriber, Subscriptions),
    State#state{subscribers = NewSubscribers, subscriptions = NewSubscriptions}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec remove_from_subscribers(subscriber(), [subscriber()]) ->
				     [subscriber()].
remove_from_subscribers(Pid, Subscribers) ->
    Subscribers -- [Pid].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec remove_from_subscriptions(subscriber(), [subscription()]) ->
				       [subscription()].
remove_from_subscriptions(Pid, Subscriptions) ->
    lists:foldl(fun({Topic, Subscribers}, Acc) ->
			case Subscribers -- [Pid] of
			    [] ->
				Acc;

			    NewSubscribers ->
				Acc ++ [{Topic, NewSubscribers}]
			end
		end,
		[],
		Subscriptions).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
tracer() ->
    erlang:trace_pattern({'_', '_', '_'}, [{'_', [], []}], [local]),
    erlang:trace(all, true, [procs]),
    erlang:trace(self(), false, [procs]),
    tracer([]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
tracer(State) ->
    receive
        {waiting, From} ->
            From ! ready,
            tracer(State);

	{stop, From} ->
	    From ! ready;

        {trace, _, spawn, Pid, _} ->
	    case get_topics(Pid) of
		undefined ->
		    tracer(State);
		
		Topics ->
		    erlang:monitor(process, Pid),
		    ?SERVER ! {subscribe, Pid, Topics},
		    tracer(State)
	    end;
	
	{'DOWN', _, process, Pid, _} ->
	    ?SERVER ! {unsubscribe, Pid},
	    tracer(State);
	
	Other ->
	    error_logger:info_report([tracer, other, {message, Other}]),
            tracer(State)
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_topics(Pid) when is_pid(Pid) ->
    case erlang:process_info(Pid) of
        undefined ->
            undefined;
	
        Props ->
            {M, _F, _A} = proplists:get_value(current_function, Props, erlang),
            Attributes = M:module_info(attributes),
            case proplists:get_value(subscribe, Attributes) of
                undefined ->
                    undefined;
		
                [] ->
		    [all];
		
                Topics ->
		    Topics
            end
    end.

