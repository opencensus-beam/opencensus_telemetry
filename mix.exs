defmodule Opencensus.Telemetry.MixProject do
  use Mix.Project

  def project do
    [
      app: :opencensus_telemetry,
      version: "0.1.0",
      elixir: "~> 1.5",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      test_coverage: [tool: ExCoveralls],
      preferred_cli_env: [coveralls: :test, "coveralls.html": :test, "coveralls.json": :test]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:telemetry, "~> 0.2.0"},
      {:opencensus, ">= 0.6.0 and <= 0.8.0"},

      # Documentation
      {:ex_doc, ">= 0.0.0", only: [:doc]},

      # Testing
      {:excoveralls, "~> 0.10.3", only: [:test]},
      {:dialyxir, ">= 0.0.0", runtime: false},
      {:junit_formatter, ">= 0.0.0", only: [:test]}
    ]
  end
end
