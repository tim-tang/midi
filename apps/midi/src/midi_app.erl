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
            ok = riak_core:register([{vnode_module, midi_vnode}]),
            ok = riak_core_node_watcher:service_up(midi, self()),

            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.
