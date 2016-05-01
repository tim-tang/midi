-module(midi).
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
         ping/0
        ]).

-ignore_xref([
              ping/0
             ]).

%% Public API

-define(SERVICE, midi).

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
    %This function is used to hash a (routing) key. It returns an integer between 0 and 2^160 - 1.
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(os:timestamp())}),
    %DocIdx = riak_core_util:chash_key({<<"TT">>, <<"ping">>}),
    lager:info("Routing Key => ~p~n", [DocIdx]),
    %apl => active preference list.
    %Vnode Type: primary/fallback
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, ?SERVICE),
    lager:info("Preference List => ~p~n", [PrefList]),
    [{IndexNode, _Type}] = PrefList,
    %{Partitionid, Node} = IndexNode
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, midi_vnode_master).
