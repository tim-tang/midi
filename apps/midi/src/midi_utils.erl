-module(midi_utils).

%% API
-export([mk_reqid/0]).

mk_reqid() ->
    erlang:system_time(nano_seconds).




