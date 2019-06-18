# Opencensus.Telemetry

[![CircleCI](https://circleci.com/gh/opencensus-beam/opencensus_telemetry.svg?style=svg)](https://circleci.com/gh/opencensus-beam/opencensus_telemetry)
[![codecov](https://codecov.io/gh/opencensus-beam/opencensus_telemetry/branch/master/graph/badge.svg)](https://codecov.io/gh/opencensus-beam/opencensus_telemetry)

Integration between [`telemetry`][telemetry] and [`opencensus`][oc].

## Installation

Rebar3:

```erlang
{deps, [{opencensus_telemetry, "~> 0.1.0"}]}.
```


Mix:

```elixir
def deps do
  [
    {:opencensus_telemetry, "~> 0.1.0"}
  ]
end
```

## Usage

`oc_telemetry:track/1` will create `telemetry` handlers for events that use `oc_stat` to record measurements. Each `telemetry` event receives a map of keys to measurement values. The list passed to `track` defines what the relationship between an event, a key in the values map and an Opencensus measumrent is.

In the example:

```erlang
Measurements = [{[http, request], [{duration, 'http/request/latency', millisecond}]}],
oc_telemetry:track(Measurements),
```

And 

```elixir
measurements = [{[:http, :request], [{:duration, :'http/request/latency', :millisecond}]}]
:oc_telemetry.track(measurements)
```

`[http, request]` is the event, `duration` the key to get the measurement from the event's values and `http/request/latency` is the OpenCensus measurement's name to record the value to.

A `Telemetry.Metric` struct can be used to define and subscribe to an OpenCensus View. For example a View for counting the number of HTTP requests based on the number of latency measurements taken would be:

```erlang
'Elixir.Telemetry.Metrics':counter(<<"http.requests.count">>, [{measurement, 'latency'}]).
```

Or in Elixir:

```elixir
Telemetry.Metrics.counter("http.requests.count", measurement: :'latency').
```

To just add a measure on an event `oc_telemetry:attach/5` can be used:

```erlang
oc_telemetry:attach("measure/name", [event, name], key, "Description", unit).
```

Or in Elixir:

```elixir
:oc_telemetry.attach("measure/name", [:event, :name], :key, "Description", :unit)
```

## License

See [LICENSE](LICENSE) file.

[telemetry]: https://github.com/beam-telemetry/telemetry
[oc]: https://github.com/census-instrumentation/opencensus-erlang
