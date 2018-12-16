# Opencensus.Telemetry

[![CircleCI](https://circleci.com/gh/opencensus-beam/opencensus_telemetry.svg?style=svg)](https://circleci.com/gh/opencensus-beam/opencensus_telemetry)
[![codecov](https://codecov.io/gh/opencensus-beam/opencensus_telemetry/branch/master/graph/badge.svg)](https://codecov.io/gh/opencensus-beam/opencensus_telemetry)

Integration between [`telemetry`][telemetry] and [`opencensus`][oc].

## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `telemetry` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:opencensus_telemetry, "~> 0.1.0"}
  ]
end
```

## License

See [LICENSE](LICENSE) file.

[telemetry]: https://github.com/beam-telemetry/telemetry
[oc]: https://github.com/census-instrumentation/opencensus-erlang
