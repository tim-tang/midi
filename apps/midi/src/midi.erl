-module(midi).
-include_lib("riak_core/include/riak_core_vnode.hrl").
-include("midi.hrl").

-export([
         ping/0,
         parse_log/2,
         parse_log/4,
         crunch/2,
         read/2,
         read/3,
         keys/1,
         set/3,
         append/3,
         incr/2,
         incrby/3,
         sadd/3,
         srem/3
        ]).

-export([get_dbg_preflist/2,
         get_dbg_preflist/3,
         dbg_op/5, dbg_op/6]).

%-define(TIMEOUT, 5000).


%%%===================================================================
%%% API
%%%===================================================================

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
    %This function is used to hash a (routing) key. It returns an integer between 0 and 2^160 - 1.
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(os:timestamp())}),
    %DocIdx = riak_core_util:chash_key({<<"TT">>, <<"ping">>}),
    lager:info("Routing Key => ~p~n", [DocIdx]),
    %apl => active preference list.
    %Vnode Type: primary/fallback
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, midi),
    lager:info("Preference List => ~p~n", [PrefList]),
    [{IndexNode, _Type}] = PrefList,
    %{Partitionid, Node} = IndexNode
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, midi_vnode_master).

% @doc Process entries from a file
parse_log(Client, FilePath) ->
  case file:read_file(FilePath) of
    {ok, Data} ->
      LinesData = binary:split(Data, [<<"\n">>], [global]),
      lists:foreach(fun(LineData) -> crunch(Client, binary_to_list(LineData)) end, LinesData),
      lager:info("~p entries processed~n", [length(LinesData)]),
      ok;
    {error, Reason} ->
      lager:error("Read file ~s failed: ~p~n", [FilePath, Reason])
  end.

% @doc Process entries from a file, start with S for N lines
parse_log(Client, FilePath, S, N) ->
  case file:read_file(FilePath) of
    {ok, Data} ->
      LinesData = binary:split(Data, [<<"\n">>], [global]),
      lists:foreach(fun(LineData) -> crunch(Client, binary_to_list(LineData)) end, lists:sublist(LinesData, S, N)),
      ok;
    {error, Reason} ->
      lager:error("Read file ~s failed: ~p~n", [FilePath, Reason])
  end.

%% @doc Process an entry.
crunch(Client, Entry) ->
    midi_crunch_fsm:crunch(Client, Entry).

%% @doc Get a stat's value.
read(Client, StatName) ->
    read(Client, StatName, []).

read(Client, StatName, Opts) ->
    {ok, Val} = midi_read_fsm:read(Client, StatName, Opts),
    midi_utils:pretty_print(Val).

keys(StateName) ->
    midi_coverage_fsm:fold({keys, StateName}).

get_dbg_preflist(Client, StatName) ->
    [get_dbg_preflist(Client, StatName, N) || N <- lists:seq(1,3)].

get_dbg_preflist(Client, StatName, Nth) ->
    DocIdx = riak_core_util:chash_key({list_to_binary(Client),
                                       list_to_binary(StatName)}),
    N = application:get_env(midi, n, ?DEFAULT_N),
    Preflist = riak_core_apl:get_apl(DocIdx, N, midi_stat),
    IdxNode = lists:nth(Nth, Preflist),
    {ok, req_id, _, Val} =
        riak_core_vnode_master:sync_command(IdxNode,
                                            {get, req_id, StatName},
                                            midi_stat_vnode_master),
    {IdxNode, Val}.

%% @doc Set a stat's value, replacing the current value.
set(Client, StatName, Val) ->
    do_write(Client, StatName, set, Val).

%% @doc Append to a stat's value.
append(Client, StatName, Val) ->
    do_write(Client, StatName, append, Val).

%% @doc Increment the stat's value by 1.
incr(Client, StatName) ->
    do_write(Client, StatName, incr).

%% @doc Increment the STATBOX value by Val.
incrby(Client, StatName, Val) ->
    do_write(Client, StatName, incrby, Val).

%% @doc Add a member to the STATBOX set.
sadd(Client, StatName, Val) ->
    do_write(Client, StatName, sadd, Val).

%% @doc Remove a member from the STATBOX set.
srem(Client, StatName, Val) ->
    do_write(Client, StatName, srem, Val).

%% @doc Fake a partitioned `Op' to the given `Nodes' from the given
%% `Coordinator'.  That is, this op will act as if the given nodes are
%% partitioned from the rest of the cluster.  Let the replies fall on
%% the caller's mailbox.
-spec dbg_op(atom(), node(), [node()], string(), string()) -> ok.
dbg_op(Op, Coordinator, Nodes, Client, StatName) ->
    dbg_op(Op, Coordinator, Nodes, Client, StatName, undefined).

-spec dbg_op(atom(), node(), [node()], string(), string(), term()) -> ok.
dbg_op(Op, Coordinator, Nodes, Client, StatName, Val) ->
    ReqID = midi_uitils:mk_reqid(),
    DocIdx = riak_core_util:chash_key({list_to_binary(Client),
                                       list_to_binary(StatName)}),
    N = application:get_env(midi, n, ?DEFAULT_N),
    Preflist = riak_core_apl:get_apl(DocIdx, N, midi_stat),
    P = fun({_Idx,Node}) ->
                lists:member(Node, [Coordinator|Nodes])
        end,
    Targets = lists:filter(P, Preflist),
    case Val of
        undefined ->
            midi_stat_vnode:Op(Targets, {ReqID, Coordinator}, StatName);
        _ ->
            midi_stat_vnode:Op(Targets, {ReqID, Coordinator}, StatName, Val)
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================
do_write(Client, StatName, Op) ->
    midi_write_fsm:write(Client, StatName, Op).

do_write(Client, StatName, Op, Val) ->
    midi_write_fsm:write(Client, StatName, Op, Val).
