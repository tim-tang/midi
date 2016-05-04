-module(midi_stat_vnode).
-behaviour(riak_core_vnode).
-include("midi.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").
-export([start_vnode/1,
         init/1,
         terminate/2,
         handle_command/3,
         is_empty/1,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         encode_handoff_item/2,
         delete/1,
         handle_coverage/4,
         handle_exit/3]).

-export([
         get/3,
         set/4,
         repair/3,
         incr/3,
         incrby/4,
         append/4,
         sadd/4,
         srem/4
        ]).

-record(state, {partition, stats, node}).

-define(MASTER, midi_stat_vnode_master).

%%%===================================================================
%%% API
%%%===================================================================

start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

get(Preflist, ReqID, StatName) ->
    riak_core_vnode_master:command(Preflist,
                                   {get, ReqID, StatName},
                                   {fsm, undefined, self()},
                                   ?MASTER).

set(Preflist, Identity, StatName, Val) ->
    riak_core_vnode_master:command(Preflist,
                                   {set, Identity, StatName, Val},
                                   {fsm, undefined, self()},
                                   ?MASTER).

%% @doc Attempt to repair -- fire and forget.
repair(IdxNode, StatName, Obj) ->
    riak_core_vnode_master:command(IdxNode,
                                   {repair, undefined, StatName, Obj},
                                   ignore,
                                   ?MASTER).

%% TODO: I have to look at the Sender stuff more closely again
incr(Preflist, Identity, StatName) ->
    riak_core_vnode_master:command(Preflist,
                                   {incrby, Identity, StatName, 1},
                                   {fsm, undefined, self()},
                                   ?MASTER).

incrby(Preflist, Identity, StatName, Val) ->
    riak_core_vnode_master:command(Preflist,
                                   {incrby, Identity, StatName, Val},
                                   {fsm, undefined, self()},
                                   ?MASTER).

append(Preflist, Identity, StatName, Val) ->
    riak_core_vnode_master:command(Preflist,
                                   {append, Identity, StatName, Val},
                                   {fsm, undefined, self()},
                                   ?MASTER).

sadd(Preflist, Identity, StatName, Val) ->
    riak_core_vnode_master:command(Preflist,
                                   {sadd, Identity, StatName, Val},
                                   {fsm, undefined, self()},
                                   ?MASTER).

srem(Preflist, Identity, StatName, Val) ->
    riak_core_vnode_master:command(Preflist,
                                   {srem, Identity, StatName, Val},
                                   {fsm, undefined, self()},
                                   ?MASTER).


%%%===================================================================
%%% Callbacks
%%%===================================================================
init([Partition]) ->
    {ok, #state{partition=Partition, stats=dict:new(), node=node()}}.

handle_command({get, ReqID, StatName}, _Sender,
               #state{stats=Stats, partition=Partition, node=Node}=State) ->
    Reply =
        case dict:find(StatName, Stats) of
            error ->
                not_found;
            {ok, Found} ->
                Found
        end,
    {reply, {ok, ReqID, {Partition,Node}, Reply}, State};

handle_command({set, {ReqID, _}, StatName, Val}, _Sender, #state{stats=Stats0}=State) ->
    Stats = try 
                dict:store(StatName, [Val], Stats0)
            catch
                _:_ -> lager:error("Failed to set value")
            end,
    {reply, {ok, ReqID}, State#state{stats=Stats}};

handle_command({repair, undefined, StatName, Obj}, _Sender, #state{stats=Stats0}=State) ->
    lager:info("repair performed ~p~n", [Obj]),
    Stats = dict:store(StatName, Obj, Stats0),
    {noreply, State#state{stats=Stats}};

handle_command({incrby, {ReqID, Coordinator}, StatName, IncrBy}, _Sender, #state{stats=Stats0}=State) ->
    Obj =
        case dict:find(StatName, Stats0) of
            {ok, #midi_obj{val=#incr{total=T0, counts=C0}}=O} ->
                T = T0 + IncrBy,
                C = dict:update_counter(Coordinator, IncrBy, C0),
                Val = #incr{total=T, counts=C},
                midi_obj:update(Val, Coordinator, O);
            error ->
                Val = #incr{total=IncrBy,
                            counts=dict:from_list([{Coordinator, IncrBy}])},
                VC0 = vclock:fresh(),
                VC = vclock:increment(Coordinator, VC0),
                #midi_obj{val=Val, vclock=VC}
        end,
    Stats = dict:store(StatName, Obj, Stats0),
    {reply, {ok, ReqID}, State#state{stats=Stats}};

handle_command({append, {ReqID, _}, StatName, Val}, _Sender, #state{stats=Stats0}=State) ->
    Stats = try 
                dict:append(StatName, Val, Stats0)
            catch 
                _:_ -> dict:store(StatName, [Val], Stats0)
            end,
    {reply, {ok, ReqID}, State#state{stats=Stats}};

handle_command({sadd, {ReqID, Coordinator}, StatName, Val},
               _Sender, #state{stats=Stats0}=State) ->
    SB = 
        case dict:find(StatName, Stats0) of
            {ok, #midi_obj{val=SB0}=O} ->
                SB1 = statebox:modify({sets, add_element, [Val]}, SB0),
                SB2 = statebox:expire(?STATEBOX_EXPIRE, SB1),
                midi_obj:update(SB2, Coordinator, O);
            error ->
                SB0 = statebox:new(fun sets:new/0),
                SB1 = statebox:modify({sets, add_element, [Val]}, SB0),
                VC0 = vclock:fresh(),
                VC = vclock:increment(Coordinator, VC0),
                #midi_obj{val=SB1, vclock=VC}
        end,
    Stats = dict:store(StatName, SB, Stats0),
    {reply, {ok, ReqID}, State#state{stats=Stats}};

handle_command({srem, {ReqID, Coordinator}, StatName, Val},
               _Sender, #state{stats=Stats0}=State) ->
    SB =
        case dict:find(StatName, Stats0) of
            {ok, #midi_obj{val=SB0}=O} ->
                SB1 = statebox:modify({sets, del_element, [Val]}, SB0),
                SB2 = statebox:expire(?STATEBOX_EXPIRE, SB1),
                midi_obj:update(SB2, Coordinator, O);
            error ->
                SB0 = statebox:new(fun sets:new/0),
                SB1 = statebox:modify({sets, del_element, [Val]}, SB0),
                VC0 = vclock:fresh(),
                VC = vclock:increment(Coordinator, VC0),
                #midi_obj{val=SB1, vclock=VC}
        end,
    Stats = dict:store(StatName, SB, Stats0),
    {reply, {ok, ReqID}, State#state{stats=Stats}}.

%% Active vnodes operate in three states: normal, handoff, and forwarding.
%%
%% In the normal state, vnode commands are passed to handle_command. When
%% a handoff is triggered, handoff_target is set and the vnode
%% is said to be in the handoff state.
%%
%% In the handoff state, vnode commands are passed to handle_handoff_command.
%% However, a vnode may be blocked during handoff (and therefore not servicing
%% commands) if the handoff procedure is blocking (eg. in riak_kv when not
%% using async fold).
%%
%% After handoff, a vnode may move into forwarding state. The forwarding state
%% is a product of the new gossip/membership code and will not occur if the
%% node is running in legacy mode. The forwarding state represents the case
%% where the vnode has already handed its data off to the new owner, but the
%% new owner is not yet listed as the current owner in the ring. This may occur
%% because additional vnodes are still waiting to handoff their data to the
%% new owner, or simply because the ring has yet to converge on the new owner.
%% In the forwarding state, all vnode commands and coverage commands are
%% forwarded to the new owner for processing.
%%
%% The above becomes a bit more complicated when the vnode takes part in resizing
%% the ring, since several transfers with a single vnode as the source are necessary
%% to complete the operation. A vnode will remain in the handoff state, for, potentially,
%% more than one transfer and may be in the handoff state despite there being no active
%% transfers with this vnode as the source. During this time requests that can be forwarded
%% to a partition for which the transfer has already completed, are forwarded. All other
%% requests are passed to handle_handoff_command.
handle_handoff_command(?FOLD_REQ{foldfun=Fun, acc0=Acc0}, _Sender, State) ->
    lager:info("[STAT VNODE HANDOFF] - Handle Accumulation ~p Fn ~p~n.", [Acc0, Fun]),
    Acc = dict:fold(Fun, Acc0, State#state.stats),
    {reply, Acc, State}.

%% @doc 2 types of handoff: ownership/hinted handoff 
handoff_starting(_TargetNode, _State) ->
    lager:info("[STAT VNODE HANDOFF] - Starting On Node => ~p~n", [_TargetNode]),
    {true, _State}.

handoff_cancelled(State) ->
    lager:info("[STAT VNODE HANDOFF] - Cancelled!"),
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    lager:info("[STAT VNODE HANDOFF] - Partition => ~p Node => ~p~n Finished! ", [State#state.partition, _TargetNode]),
    {ok, State}.

%% @doc Handle handoff data, merge handoff data with current vnode/partition data.
handle_handoff_data(Data, #state{stats=Stats0}=State) ->
    lager:info("[STAT VNODE HANDOFF] - Handle Data => ~p~n", [Data]),
    {StatName, HObj} = binary_to_term(Data),
    MObj =
        case dict:find(StatName, Stats0) of
            {ok, Obj} -> midi_obj:merge([Obj,HObj]);
            error -> HObj
        end,
    Stats = dict:store(StatName, MObj, Stats0),
    {reply, ok, State#state{stats=Stats}}.

encode_handoff_item(StatName, Val) ->
    lager:info("[STAT VNODE HANDOFF] - Encoding Data Key => ~p Value => ~p~n", [StatName, Val]),
    term_to_binary({StatName,Val}).

%% @doc check current vnode/partition has data to handoff or not.
%% if there is no handoff data will call handoff_finished/1
%% otherwise riak core will send out the data.
is_empty(State) ->
    case dict:is_empty(State#state.stats) of
        true -> {true, State};
        false -> {false, State}
    end.

%% @doc after handoff_finished will delete unused data on this vnode.
%% stats is dict module. 
%% Attention if you use db to store data, you need to delete it from db.
delete(#state{partition=Partition, stats=Stats}=State) ->
    case dict:is_empty(Stats) of
        true ->
            {ok, State};
        false ->
            lager:info("[STAT VNODE HANDOFF] - Purge Data In Dict, Partition => ~p~n", [Partition]),
            {ok, State#state{stats=dict:new()}}
    end.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
    {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, _State) ->
    {noreply, _State}.

terminate(_Reason, _State) ->
    ok.

