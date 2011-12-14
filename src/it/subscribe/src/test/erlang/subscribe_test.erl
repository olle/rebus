%%%-------------------------------------------------------------------
%%% @author Olle Törnström <olle@studiomediatech.com>
%%% @copyright (C) 2011, Olle Törnström
%%% @doc
%%% Tests that ensures that message to named `channels' arrive at the
%%% inbox of processes where the module is annotated, with the given
%%% attribute.
%%% @end
%%%-------------------------------------------------------------------
-module(subscribe_test).

-include_lib("eunit/include/eunit.hrl").

-define(SLEEP(Time), receive after Time -> ok end).

subscribed_topics_arrive_at_inbox_test() ->
    Pid = subscriber:start(),
    rebus:publish(topic1, {message, to, topic1}),
    rebus:publish(topic2, {message, to, topic2}),
    ?SLEEP(500),
    ?assert(lists:member({message, to, topic1}, subscriber:state())).
    
