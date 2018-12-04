defmodule Opencensus.Telemetry do
  @moduledoc """
  Simple wrapper for integrating `Telemetry` with Opencensus metrics.

  It will ignore unknown metrics.

  ## Options

  Each option accepts one of the 3 possible values:

  - `{module, function}` that point to unary function that will be called with
    `Telemetry.event_name`
  - unary function
  - raw value

  And options are:

  - `:name` - should return name of the measurement as a string, a charlist,
    or an atom, defaults to event name values joined with `"/"`
  - `:tags` - should return measurement tags as a map, defaults to rejecting all
    non-binary fields of the `metadata` argument of `Telemetry.execute/3`

  ## Example

      iex> :oc_stat_measure.new("foo/bar", "Test", :quus)
      iex> {:ok, view} = :oc_stat_view.subscribe(
      ...>   :foo_bar,
      ...>   "foo/bar",
      ...>   "Test",
      ...>   [:baz],
      ...>   :oc_stat_aggregation_latest
      ...> )
      iex> Opencensus.Telemetry.attach("foo", [:foo, :bar])
      :ok
      iex> Telemetry.execute([:foo, :bar], 42, %{baz: "qux"})
      iex> :oc_stat_view.export(view)
      %{
        ctags: %{},
        data: %{rows: [%{tags: ["qux"], value: 42}], type: :latest},
        description: "Test",
        name: :foo_bar,
        tags: [:baz]
      }

  ## Erlang

  For convenience of Erlang users, there also exists `:oc_telemetry` module that
  provides identical interface to Elixir module.
  """

  defstruct [:name, :description, :unit, :tags]

  @type generator(type) :: {module(), atom()} | (Telemetry.event_name() -> type) | type
  @type option ::
          {:name, generator(:oc_stat_measure.name())}
          | {:tags, generator(:oc_tags.tags())}
  @type options :: [option]

  @doc """
  See `Telemetry.attach/5`.
  """
  @spec attach(Telemetry.handler_id(), Telemetry.event_name(), options()) ::
          :ok | {:error, :already_registered}
  def attach(handler_id, event_name, config \\ []),
    do: attach_many(handler_id, [event_name], config)

  @doc """
  See `Telemetry.attach_many/5`
  """
  @spec attach_many(Telemetry.handler_id(), [Telemetry.event_name()], options()) ::
          :ok | {:error, :already_registered}
  def attach_many(handler_id, event_names, config \\ []) do
    name = Keyword.get(config, :name, fn event_name -> Enum.join(event_name, "/") end)
    tags = Keyword.get(config, :tags, &default_tags/1)

    options = %__MODULE__{
      name: name,
      tags: tags
    }

    Telemetry.attach_many(handler_id, event_names, __MODULE__, :handle_event, options)
  end

  @doc false
  def handle_event(event_name, value, metadata, %__MODULE__{} = config) do
    name = generate(config.name, event_name)

    :ok = :oc_stat.record(generate(config.tags, metadata), name, value)
  end

  defp generate({m, f}, name), do: apply(m, f, [name])
  defp generate(func, name) when is_function(func, 1), do: func.(name)
  defp generate(value, _), do: value

  defp default_tags(tags) do
    for {key, value} <- tags,
        is_atom(value) or is_binary(value),
        into: %{},
        do: {key, to_string(value)}
  end
end
