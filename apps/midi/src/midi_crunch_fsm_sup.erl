-module(midi_crunch_fsm_sup).
-behavior(supervisor).

-export([start_crunch_fsm/1,
         start_link/0]).
-export([init/1]).

start_crunch_fsm(Args) ->
    supervisor:start_child(?MODULE, Args).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    CrunchFsm = {undefined,
                {midi_crunch_fsm, start_link, []},
                temporary, 5000, worker, [midi_crunch_fsm]},
    {ok, {{simple_one_for_one, 10, 10}, [CrunchFsm]}}.
