%% @doc A vnode to crunch incoming log entries.  Attempt to match each
%% log entry against a registry of regexps.  If a regexp matches then
%% execute its corresponding trigger function passing it the {Client,
%% Entry, Regexp} as well as the resulting match.  The trigger
%% function can then choose to take an action such as update a
%% statistic via the `midi_stat_vnode'.
%%
%% Since this vnode is purely for computation there is no need to
%% worry about handoff.
-module(midi_crunch_vnode).
-behaviour(riak_core_vnode).
-include("midi.hrl").

-export([start_vnode/1,
         init/1,
         terminate/2,
         handle_command/3,
         is_empty/1,
         delete/1,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         encode_handoff_item/2,
         handle_coverage/4,
         handle_exit/3]).

%% match handlers
-export([
         combined_lf/2
        ]).

%% API
-export([crunch/4]).

-record(state, {
          partition,
          node,
          reg            %% registry [regexp => fun]
         }).

-define(MASTER, midi_crunch_vnode_master).
-define(COMBINED_LF, "(\\d+\\.\\d+\\.\\d+\\.\\d+) (.*) (.*) (\\[.*\\]) \"(.*)\" (\\d+) (.*) \"(.*)\" \"(.*)\"").

%%%===================================================================
%%% API
%%%===================================================================

start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

%% @doc Identity = {ReqId, Coordinator}.
crunch(Preflist, Identity, Client, Entry) ->
    riak_core_vnode_master:command(Preflist,
                                   {crunch, Identity, Client, Entry},
                                   {fsm, undefined, self()},
                                   ?MASTER).


%%%===================================================================
%%% Callbacks
%%%===================================================================

%% @doc start 5 vnode worker to parse the log file.
init([Partition]) ->
    WorkerPoolSize = application:get_env(midi, async_workers, 5),
    CrunchWorkerPool = {pool, midi_crunch_worker, WorkerPoolSize, []},
    Reg = [{?COMBINED_LF, fun ?MODULE:combined_lf/2}],
    {ok, #state{reg=Reg}, [CrunchWorkerPool]}.

handle_command({crunch, {ReqID, _}, Client, Entry}, _Sender, #state{reg=Reg}=State) ->
    AsyncCrunchWork = fun() ->
                        lists:foreach(match(Client, Entry), Reg)
                      end,
    FinishFn = fun() ->
                    %riak_core_vnode:reply(_Sender, ok)
                    reply({ok, ReqID}, _Sender)
               end,
    {async, {crunch, AsyncCrunchWork, FinishFn}, _Sender, State}.

handle_handoff_command(_Message, _Sender, State) ->
    {noreply, State}.

handoff_starting(_TargetNode, _State) ->
    {true, _State}.

handoff_cancelled(State) ->
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    {ok, State}.

handle_handoff_data(_Data, State) ->
    {reply, ok, State}.

encode_handoff_item(_ObjectName, _ObjectValue) ->
    <<>>.

is_empty(State) ->
    {true, State}.

delete(State) ->
    {ok, State}.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
    {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, _State) ->
    {noreply, _State}.

terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

match(Client, Entry) ->
    fun({Regexp, Fun}) ->
            case re:run(Entry, Regexp, [{capture, all, list}]) of
                nomatch -> ignore;
                {match, Match} -> Fun({Client, Entry, Regexp}, Match)
            end
    end.

%%%===================================================================
%%% Match Handlers
%%%===================================================================

combined_lf({Client, _Entry, _Regexp}, [_Entry, _Host, _, _User, _Time, Req, Code, BodySize, _Referer, Agent]) ->
    midi:sadd(Client, "agents", Agent),
    midi:incrby(Client, "total_sent", list_to_integer(BodySize)),
    
    case string:equal(Req, "-") of 
        false ->
            [Method, _Resource, _Protocol] = string:tokens(Req, " "),
            midi:incr(Client, Method),
            case Code of
                [$2, _, _] ->
                    midi:incr(Client, "200");
                [$3, _, _] ->
                    midi:incr(Client, "300");
                [$4, _, _] ->
                    midi:incr(Client, "400");
                [$5, _, _] ->
                    midi:incr(Client, "500")
            end,
            midi:incr(Client, "total_reqs");
        true -> lager:info("Invalid logger entry...")
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================
reply(Reply, Sender) ->
    riak_core_vnode:reply(Sender, Reply).
