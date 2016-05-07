-module(midi_utils).
-include("midi.hrl").

%% API
-export([mk_reqid/0, pretty_print/1]).

mk_reqid() ->
    erlang:system_time(nano_seconds).

pretty_print(#incr{total=Total}) -> Total;
pretty_print(Val) when element(1, Val) == statebox ->
    pretty_print(statebox:value(Val));
pretty_print(Val) when element(1, Val) == set -> sets:to_list(Val);
pretty_print(Val) -> Val.
