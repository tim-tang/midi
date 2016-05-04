%% @doc Vnode worker to crunch log.

-module(midi_crunch_worker).
-behaviour (riak_core_vnode_worker).
-export([init_worker/3, handle_work/3]).
-record (state, {index}).

%% ===================================================================
%% Public API
%% ===================================================================

%% @doc Initialize the worker. Currently only the VNode index
%% parameter is used.
init_worker(VNodeIndex, _Args, _Props) ->
    {ok, #state{index=VNodeIndex}}.

%% @doc Perform the asynchronous crunch operation.
handle_work({crunch, CrunchFn, FinishFn}, _Sender, State) ->
    try
        CrunchFn(),
        lager:info("[VNODE WORKER] handle work on partition: ~p~n", [State#state.index]),
        FinishFn()
    catch
        throw:receiver_down -> ok;
        throw:stop_fold -> ok
    end,
    {noreply, State}.
