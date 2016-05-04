%% @doc The coodinator for parse log entries, make sure 
%% each entry parsed when single/mutli nodes failure.

-module(midi_crunch_fsm).
-behaviour(gen_fsm).
-include("midi.hrl").

%% API
-export([start_link/4, crunch/2]).

%% Callbacks
-export([init/1, code_change/4, handle_event/3, handle_info/3,
         handle_sync_event/4, terminate/3]).

%% States
-export([prepare/2, execute/2, waiting/2]).

-record(state, {req_id :: pos_integer(),
                coordinator :: node(),
                from :: pid(),
                client :: string(),
                entry :: any(),
                preflist :: riak_core_apl:preflist2(),
                n :: pos_integer(),
                w :: pos_integer(),
                num_w = 0 :: non_neg_integer()
                }).

-define(SERVICE, midi_crunch).

start_link(ReqID, From, Client, Entry) ->
    gen_fsm:start_link(?MODULE, [ReqID, From, Client, Entry], []).


crunch(Client, Entry) ->
    ReqID = midi_utils:mk_reqid(),
    midi_crunch_fsm_sup:start_crunch_fsm([ReqID, self(), Client, Entry]),
    receive
        {ReqID, ok} -> 
            ok;
        {ReqID, ok, Val} -> {ok, Val}
    after ?TIMEOUT->
        {error, timeout}
    end.


%%%===================================================================
%%% States
%%%===================================================================

%% @doc Initialize the state data.
init([ReqID, From, Client, Entry]) ->
    N = application:get_env(midi, crunch_n, ?DEFAULT_CRUNCH_N),
    SD = #state{req_id=ReqID,
                coordinator=node(),
                from=From,
                client=Client,
                entry=Entry,
                w=?DEFAULT_CRUNCH_W,
                n=N},
    {ok, prepare, SD, 0}.

%% @doc Prepare the write by calculating the _preference list_.
prepare(timeout, SD0=#state{client=Client, n=N}) ->
    DocIdx = riak_core_util:chash_key({list_to_binary(Client),
                                      term_to_binary(os:timestamp())}),
    Preflist = riak_core_apl:get_apl(DocIdx, N, ?SERVICE),
    SD = SD0#state{preflist=Preflist},
    {next_state, execute, SD, 0}.

%% @doc Execute the write request and then go into waiting state to
%% verify it has meets consistency requirements.
execute(timeout, SD0=#state{req_id=ReqID,
                            coordinator=Coordinator,
                            client=Client,
                            entry=Entry,
                            preflist=Preflist}) ->
    midi_crunch_vnode:crunch(Preflist, {ReqID, Coordinator}, Client, Entry),
    {next_state, waiting, SD0}.

%% @doc Wait for N reqs to respond.
waiting({ok, ReqID}, SD0=#state{from=From, w=W, num_w=NumW0}) ->
    NumW = NumW0 + 1,
    SD = SD0#state{num_w=NumW},
    case NumW of 
        W ->
            From ! {ReqID, ok},
            lager:info("[Crunch FSM] - Finished Log Crunch with ReqID => ~p Num_W =>~p~n", [ReqID, NumW]),
            {stop, normal, SD};

        _ -> {next_state, waiting, SD}
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
