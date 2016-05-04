-module(midi_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
    VMaster = {midi_vnode_master,
                  {riak_core_vnode_master, start_link, [midi_vnode]},
                  permanent, 5000, worker, [riak_core_vnode_master]},

    Crunch = {midi_crunch_vnode_master,
             {riak_core_vnode_master, start_link, [midi_crunch_vnode]},
             permanent, 5000, worker, [riak_core_vnode_master]},

    Stat = {midi_stat_vnode_master,
            {riak_core_vnode_master, start_link, [midi_stat_vnode]},
            permanent, 5000, worker, [riak_core_vnode_master]},

    CrunchFsmSup = {midi_crunch_fsm_sup,
                 {midi_crunch_fsm_sup, start_link, []},
                 permanent, infinity, supervisor, [midi_crunch_fsm_sup]},

    WriteFsmSup = {midi_write_fsm_sup,
                 {midi_write_fsm_sup, start_link, []},
                 permanent, infinity, supervisor, [midi_write_fsm_sup]},

    ReadFsmSup = {midi_read_fsm_sup,
               {midi_read_fsm_sup, start_link, []},
               permanent, infinity, supervisor, [midi_read_fsm_sup]},

    {ok,
        {{one_for_one, 5, 10},
          [VMaster, Crunch, Stat, CrunchFsmSup, WriteFsmSup, ReadFsmSup]}}.
