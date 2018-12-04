defmodule Opencensus.Telemetry.MixProject do
  use Mix.Project

  def project do
    [
      app: :opencensus_telemetry,
      version: "0.1.0",
      elixir: "~> 1.7",
      start_permanent: Mix.env() == :prod,
      deps: deps()
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
      {:opencensus, "~> 0.6.0"},
      {:ex_doc, ">= 0.0.0", only: [:dev, :doc]}
    ]
  end
end
