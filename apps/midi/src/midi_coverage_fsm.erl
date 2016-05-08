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

%-record(state, {replies = #{} :: #{binary() => pos_integer()},
%                seen = sets:new() :: sets:set(),
%                r = 1 :: pos_integer(),
%                reqid :: integer(),
%                from :: pid(),
%                reqs :: list(),
%                merge_fn = fun([E | _]) -> E  end :: merge_fn(),
%                completed = [] :: [binary()]}).

-record(state, {req_id :: integer(), 
                from :: pid(), 
                r = 1 :: integer(), 
                merge_fn = fun([E | _]) -> E  end :: merge_fn(),
                request, 
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
    N = application:get_env(midi, n, ?DEFAULT_N),
    R = application:get_env(midi, r, ?DEFAULT_R),
    PrimaryVNodeCoverage = R,
    %% all - full coverage; allup - partial coverage.
    VNodeSelector = allup,
    %% We timeout after 5s
    Timeout = 9000,
    State = #state{r = R, from = From, req_id = ReqID},
    {Request, VNodeSelector, N, PrimaryVNodeCoverage, ?SERVICE, ?MASTER, Timeout, State}.

process_results({{_ReqId, {Partition, Node}}, Data},
                 State=#state{accum=Accum}) ->
     NewAccum = [{Partition, Node, Data}|Accum],
     {done, State#state{accum=NewAccum}};

%process_results({Type, _ReqID, _IdxNode, Obj},
%                 State = #state{reqid = ReqID, from = From})
%    when Type =:= partial;
%         Type =:= ok
%         ->
%      State1 = lists:foldl(fun update/2, State, Obj),
%      State2 = case length(State1#state.completed) of
%                   L when L >= ?PARTIAL_SIZE ->
%                       From ! {partial, ReqID, State1#state.completed},
%                       State1#state{completed = []};
%                   _ ->
%                       State1
%               end,
%      %% If we return ok and not done this vnode will be considered
%      %% to keep sending data.
%      %% So we translate the reply type here
%      ReplyType = case Type of
%                      ok -> done;
%                      partial -> ok
%                  end,
%      {ReplyType, State2};

process_results({ok, _}, State) ->
    {done, State};

process_results(Result, State) ->
    lager:error("[coverage] Unknown process results call: ~p ~p",
                [Result, State]),
    {done, State}.


finish(clean, State = #state{accum= Completed, req_id = ReqID,
                             from = From}) ->
    From ! {ok, ReqID, Completed},
    {stop, normal, State};

finish({error, E}, State = #state{accum= Completed, req_id = ReqID,
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
%update(Key, State) when is_binary(Key) ->
%    update({Key, Key}, State);
%
%update(Key, State) when is_atom(Key) ->
%    update({Key, Key}, State);
%
%update({Pts, {Key, V}}, State) when not is_binary(Pts) ->
%    update({Key, {Pts, V}}, State);
%
%update({Key, Value}, State = #state{seen = Seen}) ->
%    case sets:is_element(Key, Seen) of
%        true ->
%            State;
%        false ->
%            update1({Key, Value}, State)
%    end.
%
%update1({Key, Value}, State = #state{r = R, completed = Competed, seen = Seen})
%  when R < 2 ->
%    Seen1 = sets:add_element(Key, Seen),
%    State#state{seen = Seen1, completed = [Value | Competed]};
%
%update1({Key, Value},
%        State = #state{r = R, completed = Competed, seen = Seen,
%                       merge_fn = Merge, replies = Replies}) ->
%    case maps:find(Key, Replies) of
%        error ->
%            Replies1 = maps:put(Key, [Value], Replies),
%            State#state{replies = Replies1};
%        {ok, Vals} when length(Vals) >= R - 1 ->
%            Merged = Merge([Value | Vals]),
%            Seen1 = sets:add_element(Key, Seen),
%            Replies1 = maps:remove(Key, Replies),
%            State#state{seen = Seen1, completed = [Merged | Competed],
%                        replies = Replies1};
%        {ok, Vals} ->
%            Replies1 = maps:put(Key, [Value | Vals], Replies),
%            State#state{replies = Replies1}
%    end.
