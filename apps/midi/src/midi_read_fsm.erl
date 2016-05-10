-module(midi_read_fsm).
-behavior(gen_fsm).
-include("midi.hrl").

%% API
-export([start_link/5, read/3]).

%% Callbacks
-export([init/1, code_change/4, handle_event/3, handle_info/3,
         handle_sync_event/4, terminate/3]).

%% States
-export([prepare/2, execute/2, waiting/2, wait_for_n/2, finalize/2]).

-export([reconcile/1]).

-record(state, {req_id,
                from,
                client,
                stat_name,
                preflist,
                num_r=0,
                replies=[],
                r,
                n,
                timeout=?DEFAULT_TIMEOUT}).

-type idx_node() :: {integer(), node()}.
-type midi_reply() :: {idx_node(), midi_obj() | not_found}.
-define(SERVICE, midi_stat).

%%%===================================================================
%%% API
%%%===================================================================

start_link(ReqID, From, Client, StatName, Opts) ->
    gen_fsm:start_link(?MODULE, [ReqID, From, Client, StatName, Opts], []).

read(Client, StatName, Opts) ->
    ReqID = midi_utils:mk_reqid(),
    midi_read_fsm_sup:start_read_fsm([ReqID, self(), Client, StatName, Opts]),
    receive
        {ReqID, ok} -> 
            ok;
        {ReqID, ok, Val} -> {ok, Val}
    after ?TIMEOUT ->
        {error, timeout}
    end.


%%%===================================================================
%%% States
%%%===================================================================

%% Intiailize state data.
init([ReqId, From, Client, StatName, Opts]) ->
    N = application:get_env(midi, n, ?DEFAULT_N),
    R = application:get_env(midi, r, ?DEFAULT_R),
    
    SD = #state{req_id=ReqId,
                from=From,
                client=Client,
                stat_name=StatName,
                n=N,
                r=R},
    {ok, prepare, SD, 0}.

%% @doc Calculate the Preflist.
prepare(timeout, SD0=#state{client=Client,
                            stat_name=StatName,
                            n=N}) ->
    DocIdx = riak_core_util:chash_key({list_to_binary(Client),
                                       list_to_binary(StatName)}),
    Prelist = riak_core_apl:get_apl(DocIdx, N, ?SERVICE),
    SD = SD0#state{preflist=Prelist},
    {next_state, execute, SD, 0}.

%% @doc Execute the get reqs.
execute(timeout, SD0=#state{req_id=ReqId,
                            stat_name=StatName,
                            preflist=Prelist}) ->
    midi_stat_vnode:get(Prelist, ReqId, StatName),
    {next_state, waiting, SD0}.

%% @doc Wait for R replies and then respond to From (original client
%% that called `midi:get/2').
waiting({ok, ReqID, IdxNode, Obj},
        SD0=#state{from=From, num_r=NumR0, replies=Replies0,
                   r=R, n=N, timeout=Timeout}) ->
    NumR = NumR0 + 1,
    Replies = [{IdxNode, Obj}|Replies0],
    SD = SD0#state{num_r=NumR,replies=Replies},

    case NumR of
        R ->
            Reply = midi_obj:val(merge(Replies)),
            From ! {ReqID, ok, Reply},

            %do read repair
            case NumR of 
                N -> {next_state, finalize, SD, 0};
                _ -> {next_state, wait_for_n, SD, Timeout}
            end;
        _ -> {next_state, waiting, SD}
    end.

wait_for_n({ok, _ReqID, IdxNode, Obj},
             SD0=#state{num_r=NumR, n=N, replies=Replies0, stat_name=_StatName}) when NumR == N-1 ->
    Replies = [{IdxNode, Obj}|Replies0],
    {next_state, finalize, SD0#state{num_r=N, replies=Replies}, 0};

wait_for_n({ok, _ReqID, IdxNode, Obj},
             SD0=#state{num_r=NumR0, replies=Replies0,
                        stat_name=_StatName, timeout=Timeout}) ->
    NumR = NumR0 + 1,
    Replies = [{IdxNode, Obj}|Replies0],
    {next_state, wait_for_n, SD0#state{num_r=NumR, replies=Replies}, Timeout};

%% TODO partial repair?
wait_for_n(timeout, SD) ->
    {stop, timeout, SD}.

finalize(timeout, SD=#state{replies=Replies, stat_name=StatName}) ->
    MObj = merge(Replies),
    case needs_repair(MObj, Replies) of
        true ->
            repair(StatName, MObj, Replies),
            {stop, normal, SD};
        false ->
            {stop, normal, SD}
    end.

handle_info(_Info, _StateName, StateData) ->
    {stop,badmsg,StateData}.

handle_event(_Event, _StateName, StateData) ->
    {stop,badmsg,StateData}.

handle_sync_event(_Event, _From, _StateName, StateData) ->
    {stop,badmsg,StateData}.

code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.

terminate(_Reason, _SN, _SD) ->
    ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @pure
%%
%% @doc Given a list of `Replies' return the merged value.
-spec merge([midi_reply()]) -> midi_obj() | not_found.
merge(Replies) ->
    Objs = [Obj || {_,Obj} <- Replies],
    midi_obj:merge(Objs).

%%
%% @doc Reconcile conflicts among conflicting values.
%% More details refer => https://github.com/tim-tang/try-try-try/tree/master/04-riak-core-conflict-resolution 
-spec reconcile([A::any()]) -> A::any().
reconcile([#incr{}|_]=Vals) ->
    Get = fun(K, L) -> proplists:get_value(K, L, 0) end,
    Counts = [dict:to_list(V#incr.counts) || V <- Vals],
    Nodes = unique(lists:flatten([[Node || {Node,_} <- C] || C <- Counts])),
    MaxCounts = [{Node, lists:max([Get(Node, C) || C <- Counts])}
                 || Node <- Nodes],
    Total = lists:sum([lists:max([Get(Node, C) || C <- Counts])
                       || Node <- Nodes]),
    #incr{total=Total, counts=dict:from_list(MaxCounts)};

reconcile([V|_]=Vals) when element(1, V) == statebox -> statebox:merge(Vals).


%% @pure
%%
%% @doc Given the merged object `MObj' and a list of `Replies'
%% determine if repair is needed.
-spec needs_repair(any(), [midi_reply()]) -> boolean().
needs_repair(MObj, Replies) ->
    Objs = [Obj || {_,Obj} <- Replies],
    lists:any(different(MObj), Objs).

%% @pure
different(A) -> fun(B) -> not midi_obj:equal(A,B) end.

%% @impure
%%
%% @doc Repair any vnodes that do not have the correct object.
-spec repair(string(), midi_obj(), [midi_reply()]) -> io.
repair(_, _, []) -> io;

repair(StatName, MObj, [{IdxNode,Obj}|T]) ->
    case midi_obj:equal(MObj, Obj) of
        true -> repair(StatName, MObj, T);
        false ->
            midi_stat_vnode:repair(IdxNode, StatName, MObj),
            repair(StatName, MObj, T)
    end.

%% pure
%%
%% @doc Given a list return the set of unique values.
-spec unique([A::any()]) -> [A::any()].
unique(L) ->
    sets:to_list(sets:from_list(L)).

