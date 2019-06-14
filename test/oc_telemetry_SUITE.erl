-module(oc_telemetry_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
  [attach_simple, attaching_twice_fails, tracking_simple, distribution,
   oc_views, metrics_to_oc_views].

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

  {ok, Measure} = oc_telemetry:attach(Name, EventName, bar, "Test", any, fun(X) -> X end),
  {ok, View} = oc_stat_view:subscribe(Name, Measure, "Test", [baz],
                                      oc_stat_aggregation_latest),
  telemetry:execute(EventName, #{bar => Value}, #{foo => "a", baz => "b"}),
  ?assertMatch(#{
     data := #{rows := [#{tags := ["b"], value := Value}]},
     name := Name,
     tags := [baz]
    }, oc_stat_view:export(View)),
  oc_stat_view:unsubscribe(View).

attaching_twice_fails(Config) ->
  Name = ?config(name, Config),

  {ok, _Measure} = oc_telemetry:attach(Name, [foo], foo, "Foo", foo, fun(X) -> X end),
  ?assertMatch({error, _}, oc_telemetry:attach(Name, [foo], foo, "Bar", bar, fun(X) -> X end)).

tracking_simple(_Config) ->
  EventName = [foo, bar],
  Value = 42,

  Definition = telemetry_metric_latest(EventName, fun (V) -> V end, []),
  {ok, View} = oc_telemetry:track(Definition),
  telemetry:execute([foo], #{value => Value,
                             bar => Value}, #{foo => "a", baz => "b"}),
  ?assertMatch(#{
     data := #{rows := [#{value := Value}]}
    }, oc_stat_view:export(View)),
  oc_stat_view:unsubscribe(View).

distribution(_Config) ->
    M = 'Elixir.Telemetry.Metrics':distribution(<<"http.request.duration">>,
                                                [{buckets, [100, 200, 300]},
                                                 {tags, [controller, action]}]),
    {ok, View} = oc_telemetry:track(M),

    C = 'Elixir.Telemetry.Metrics':counter(<<"http.request.requests">>,
                                           %% I don't like this but haven't figured out a better way
                                           [{measurement, <<"http/request/duration">>},
                                            {tags, [controller, action]}]),
    {ok, View2} = oc_telemetry:track(C),


    telemetry:execute([http, request], #{duration => 100}, #{}),
    telemetry:execute([http, request], #{duration => 300}, #{}),
    ?assertMatch(#{
                   data := #{rows := [#{value := 2}]}
                  }, oc_stat_view:export(View2)),
    ?assertMatch(#{
                   data := #{rows := [#{value := #{buckets :=
                                                       [{100,1},{200,0},{300,1},{infinity,0}]}}]}
                  }, oc_stat_view:export(View)),
    oc_stat_view:unsubscribe(View),
    oc_stat_view:unsubscribe(View2).

oc_views(_Config) ->
    Measurements = [{[http, request], [{duration, 'http/request/latency', millisecond}]}],
    oc_telemetry:track(Measurements),

    {ok, CountView} = oc_stat_view:subscribe(#{name => "http/request/count",
                                               measure => 'http/request/latency',
                                               description => "number of requests received",
                                               tags => [],
                                               aggregation => oc_stat_aggregation_count}),

    telemetry:execute([http, request], #{duration => 100}, #{}),
    telemetry:execute([http, request], #{duration => 300}, #{}),
    ?assertMatch(#{
                   data := #{rows := [#{value := 2}]}
                  }, oc_stat_view:export(CountView)),
    oc_stat_view:unsubscribe(CountView).

metrics_to_oc_views(_Config) ->
    Measurements = [{[http, request], [{duration, 'http/request/latency', millisecond}]}],
    oc_telemetry:track(Measurements),

    Count = telemetry_metric_count(<<"http.request.count">>, 'http/request/latency'),
    [CountView] = oc_telemetry:subscribe_views([Count]),

    telemetry:execute([http, request], #{duration => 100}, #{}),
    telemetry:execute([http, request], #{duration => 300}, #{}),
    ?assertMatch(#{
                   data := #{rows := [#{value := 2}]}
                  }, oc_stat_view:export(CountView)),
    oc_stat_view:unsubscribe(CountView).

telemetry_metric_latest(Name, TagValues, Tags) ->
    'Elixir.Telemetry.Metrics':last_value(Name, [{tags, Tags}, {tag_values, TagValues}]).

telemetry_metric_count(Name, Measurement) ->
    'Elixir.Telemetry.Metrics':counter(Name, [{measurement, Measurement}]).
