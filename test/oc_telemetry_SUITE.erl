-module(oc_telemetry_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
  [exports_to_view].

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(opencensus_telemetry),
  Config.

end_per_suite(Config) ->
  ok = application:stop(opencensus_telemetry),
  Config.

init_per_testcase(_, Config) ->
    Name = crypto:strong_rand_bytes(16),
    [{name, Name} | Config].

end_per_testcase(_, Config) ->
    Name = ?config(name, Config),
    telemetry:detach(Name).

exports_to_view(Config) ->
  Name = ?config(name, Config),
  EventName = [foo, bar],
  Value = 42,

  {ok, Measure} = oc_telemetry:attach(Name, EventName, "Test", any),
  {ok, View} = oc_stat_view:subscribe(Name, Measure, "Test", [baz],
                                      oc_stat_aggregation_latest),
  telemetry:execute(EventName, Value, #{foo => "a", baz => "b"}),
  ?assertMatch(#{
     data := #{rows := [#{tags := ["b"], value := Value}]},
     name := Name,
     tags := [baz]
    }, oc_stat_view:export(View)),
  oc_stat_view:unsubscribe(View).

registering_twice_returns_error(Config) ->
  Name = ?config(name, Config),

  {ok, _Measure} = oc_telemetry:attach(Name, [foo], "Foo", foo),
  ?assertMatch({error, _}, oc_telemetry:attach(Name, [foo], "Bar", bar)).
