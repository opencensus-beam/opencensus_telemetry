-module(oc_telemetry_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
  [attach_simple, attaching_twice_fails, tracking_simple].

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

attach_simple(Config) ->
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

attaching_twice_fails(Config) ->
  Name = ?config(name, Config),

  {ok, _Measure} = oc_telemetry:attach(Name, [foo], "Foo", foo),
  ?assertMatch({error, _}, oc_telemetry:attach(Name, [foo], "Bar", bar)).

tracking_simple(_Config) ->
  EventName = [foo, bar],
  Value = 42,

  Definition = telemetry_metric_latest(EventName, fun (V) -> V end, []),

  {ok, View} = oc_telemetry:track(Definition),
  telemetry:execute(EventName, Value, #{foo => "a", baz => "b"}),
  ?assertMatch(#{
     data := #{rows := [#{value := Value}]}
    }, oc_stat_view:export(View)),
  oc_stat_view:unsubscribe(View).

telemetry_metric_latest(Name, Meta, Tags) ->
  #{name => Name,
    event_name => Name,
    tags => Tags,
    metadata => Meta,
    unit => unit,
    description => nil,
    '__struct__' => 'Elixir.Telemetry.Metrics.LastValue'}.
