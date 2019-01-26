# Opencensus.Telemetry

[![CircleCI](https://circleci.com/gh/opencensus-beam/opencensus_telemetry.svg?style=svg)](https://circleci.com/gh/opencensus-beam/opencensus_telemetry)
[![codecov](https://codecov.io/gh/opencensus-beam/opencensus_telemetry/branch/master/graph/badge.svg)](https://codecov.io/gh/opencensus-beam/opencensus_telemetry)

Integration between [`telemetry`][telemetry] and [`opencensus`][oc].

## Installation

Rebar3:

```erlang
{deps, [{opencensus_telemetry, "~> 0.1.0"}]}.


Mix:

```elixir
def deps do
  [
    {:opencensus_telemetry, "~> 0.1.0"}
  ]
end
```

## Usage

To add measure on event just run:

```erlang
oc_telemetry:attach("measure/name", [event, name], "Description", unit).
```

Or in Elixir:

```elixir
:oc_telemetry.attach("measure/name", [:event, :name], "Description", :unit)
```

## License

See [LICENSE](LICENSE) file.

[telemetry]: https://github.com/beam-telemetry/telemetry
[oc]: https://github.com/census-instrumentation/opencensus-erlang
