%% @doc coverage supervisor for stat vnode.
-module(midi_coverage_fsm_sup).
-behavior(supervisor).

-export([start_coverage/1,
         start_link/0]).

-export([init/1]).

-ignore_xref([init/1,
              start_link/0,
              start_coverage/1]).

start_coverage(Args) ->
    supervisor:start_child(?MODULE, Args).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    CoverageFsm = {undefined,
               {midi_coverage_fsm, start_link, []},
               temporary, 5000, worker, [midi_coverage_fsm]},
    {ok, {{simple_one_for_one, 10, 10}, [CoverageFsm]}}.
