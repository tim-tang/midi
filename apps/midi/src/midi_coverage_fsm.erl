-module(midi_coverage_fsm).
-include("midi.hrl").
-behaviour (riak_core_coverage_fsm).

-compile(export_all).

-define(MASTER, midi_stat_vnode_master).
-define(SERVICE, midi_stat). 

-type merge_fn() ::  fun(([term()]) -> term()).

-record(state, {replies = #{} :: #{binary() => pos_integer()},
                seen = sets:new() :: sets:set(),
                r = 1 :: pos_integer(),
                reqid :: integer(),
                from :: pid(),
                reqs :: list(),
                raw :: boolean(),
                merge_fn = fun([E | _]) -> E  end :: merge_fn(),
                completed = [] :: [binary()]}).
 


%%%===================================================================
%%% API
%%%===================================================================
concat(Es, Acc) ->
    Es ++ Acc.

fold(Request) ->
    fold(Request, fun concat/2, []).

fold(Request, FoldFn, Acc0) ->
    ReqID = midi_utils:mk_reqid(),
    midi_coverage_fsm_sup:start_coverage(
      ?MODULE, {self(), ReqID, something_else}, Request#req{id = ReqID}),
    wait(ReqID, FoldFn, Acc0).


%%%===================================================================
%%% States
%%%===================================================================
init({From, ReqID, _}, Request) ->
    N = application:get_env(midi, n, ?DEFAULT_N),
    R = application:get_env(midi, r, ?DEFAULT_R),
    PrimaryVNodeCoverage = R,
    %% all - full coverage; allup - partial coverage.
    VNodeSelector = allup,
    %% We timeout after 5s
    Timeout = 9000,
    State = #state{r = R, from = From, reqid = ReqID},
    {Request, VNodeSelector, N, PrimaryVNodeCoverage, ?SERVICE, ?MASTER, Timeout, State}.


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



process_results({ok, _}, State) ->
    {done, State};

process_results(Result, State) ->
    lager:error("[coverage] Unknown process results call: ~p ~p",
                [Result, State]),
    {done, State}.


finish(clean, State = #state{completed = Completed, reqid = ReqID,
                             from = From}) ->
    From ! {ok, ReqID, Completed},
    {stop, normal, State};

finish({error, E}, State = #state{completed = Completed, reqid = ReqID,
                             from = From}) ->
    lager:error("[coverage] Finished with error: ~p", [E]),
    From ! {ok, ReqID, Completed},
    {stop, normal, State};

finish(How, State) ->
    lager:error("[coverage] Unknown finish call: ~p ~p", [How, State]),
    {error, failed}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================
