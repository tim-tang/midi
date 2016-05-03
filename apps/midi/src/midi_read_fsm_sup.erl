-module(midi_read_fsm_sup).
-behavior(supervisor).

-export([start_read_fsm/1,start_link/0]).
-export([init/1]).

start_read_fsm(Args) ->
    supervisor:start_child(?MODULE, Args).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    GetFsm = {undefined,
              {midi_read_fsm, start_link, []},
              temporary, 5000, worker, [midi_read_fsm]},
    {ok, {{simple_one_for_one, 10, 10}, [GetFsm]}}.
