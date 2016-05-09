-module(midi_coverage_fsm).
-include("midi.hrl").
-behaviour (riak_core_coverage_fsm).

-export([fold/1, fold/3]).

-export([start_link/3, init/2, process_results/2, finish/2]).


-ignore_xref([start_link/4]).

-define(MASTER, midi_stat_vnode_master).
-define(SERVICE, midi_stat). 
-define(PARTIAL_SIZE, 10).

-type merge_fn() ::  fun(([term()]) -> term()).

-record(state, {req_id :: integer(), 
                from :: pid(), 
                r = 1 :: integer(), 
                merge_fn = fun([E | _]) -> E  end :: merge_fn(),
                request, 
                seen = sets:new() :: sets:set(),
                accum=[] :: [binary()]}). 


%%%===================================================================
%%% API
%%%===================================================================
start_link(ReqId, From, Request) ->
    riak_core_coverage_fsm:start_link(?MODULE, {pid, ReqId, From},
                                    [ReqId, From, Request]).

concat(Es, Acc) ->
    Es ++ Acc.

fold(Request) ->
    fold(Request, fun concat/2, []).

fold(Request, FoldFn, Acc0) ->
    ReqID = midi_utils:mk_reqid(),
    midi_coverage_fsm_sup:start_coverage([ReqID, self(), Request]),
    wait(ReqID, FoldFn, Acc0).

wait(ReqID, FoldFn, Acc) ->
    receive
        {ok, ReqID} ->
            ok;
        {partial, ReqID, Result1} ->
            wait(ReqID, FoldFn, FoldFn(Result1, Acc));
        {ok, ReqID, Result1} ->
            {ok, FoldFn(Result1, Acc)}
    after 10000 ->
            {error, timeout}
    end.

%%%===================================================================
%%% States
%%%===================================================================
init(_, [ReqID, From, Request]) ->
    %% NVal - Indicates the replication factor and is used to
    %% accurately create a minimal covering set of VNodes.
    N = application:get_env(midi, n, ?DEFAULT_N),
    R = application:get_env(midi, r, ?DEFAULT_R),
    PrimaryVNodeCoverage = R,
    %% all - full coverage; allup - partial coverage.
    VNodeSelector = allup,
    %% We timeout after 5s
    Timeout = 9000,
    State = #state{r = R, from = From, req_id = ReqID},
    %%PrimaryVNodeCoverage - The number of primary VNodes
    %%from the preference list to use in creating the coverage plan.
    {Request, VNodeSelector, N, PrimaryVNodeCoverage, ?SERVICE, ?MASTER, Timeout, State}.

process_results({{_ReqId, {Partition, Node}}, Data},
                 State=#state{accum=Accum}) ->
     NewAccum = [{Partition, Node, Data}|Accum],
     {done, State#state{accum=NewAccum}};

process_results(Result, State=#state{}) ->
    lager:error("[coverage] Unknown process results call: ~p ~p",
                [Result, State]),
    {done, State}.

finish(clean, State = #state{accum=Accum, req_id = ReqID, from = From}) ->
    From ! {ok, ReqID, Accum},
    {stop, normal, State};

finish({error, Reason}, State = #state{accum=Accum, req_id = ReqID, from = From}) ->
    lager:error("[coverage] Finished with error: ~p", [Reason]),
    From ! {ReqID, {partial, Reason, Accum}},
    {stop, normal, State};

finish(How, State) ->
    lager:error("[coverage] Unknown finish call: ~p ~p", [How, State]),
    {error, failed}.
