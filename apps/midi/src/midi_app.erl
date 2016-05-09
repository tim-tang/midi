-module(midi_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case midi_sup:start_link() of
        {ok, Pid} ->
            ok = riak_core:register([{vnode_module, midi_crunch_vnode}]),
            ok = riak_core_node_watcher:service_up(midi_crunch, self()),

            ok = riak_core:register([{vnode_module, midi_stat_vnode}]),
            ok = riak_core_node_watcher:service_up(midi_stat, self()),
            
            %% Ring Event Handler
            ok = riak_core_ring_events:add_guarded_handler(midi_ring_event_handler, []),
            %% Node Event Handler
            ok = riak_core_node_watcher_events:add_guarded_handler(midi_node_event_handler, []),

            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.
