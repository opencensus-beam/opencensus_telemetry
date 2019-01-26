-module(oc_telemetry).

% ------------------------------------------------------------------------------

% Public Exports
-export([attach/4, track/1]).

% ------------------------------------------------------------------------------

-record(options, {name, filter}).

% ------------------------------------------------------------------------------

%% @doc Creates measurement and attach it to the telemetry dispatcher
%%
%% On success it returns `{ok, Measurement}' where `Measurement' is newly
%% created OpenCensus measurement that can be used for creating views.
%%
%% When there is already attached listener for given name then it will return
%% `{error, already_registered}'.

-spec attach(oc_stat_measure:name(),
             telemetry:event_name(),
             oc_stat_measure:description(),
             oc_stat_measure:unit()) -> {ok, oc_stat_measure:measure()}
                                        | {error, already_registered}.

attach(Name, EventName, Description, Unit) ->
  attach(Name, EventName, Description, Unit, fun (Data) -> Data end).

%% @doc Creates measurement, view, and attaches measurement to the telemetry
%%
%% On success it returns `{ok, View}' where `View' is newly created OpenCensus
%% view.
%%
%% When there is already attached listener for given name then it will return
%% `{error, already_registered}'.

-spec track(Metric::map()) -> {ok, oc_stat_view:view()}
                              | {error, term()}.

track(#{'__struct__' := Type,
        name := NormalizedName,
        event_name := EventName,
        metadata := Metadata,
        tags := Tags,
        description := Desc,
        unit := Unit} = Data) ->
  Name = build_name(NormalizedName),
  Description = Desc,

  case attach(Name, EventName, Description, Unit, Metadata) of
    {ok, Measure} ->
      Aggregation = aggregation(Type, Data),

      oc_stat_view:subscribe(Name, Measure, Description, Tags, Aggregation);
    Error -> Error
  end.

% ------------------------------------------------------------------------------

attach(Name, EventName, Description, Unit, Metadata) ->
    Measure = oc_stat_measure:new(Name, Description, Unit),
    Options = #options{name = Name, filter = Metadata},

    case telemetry:attach(Name, EventName, fun handle_event/4, Options) of
      ok -> {ok, Measure};
      Error -> Error
    end.

% Handle events send by `telemetry' application
handle_event(_EventName, Value, Metadata,
             #options{name = Name, filter = Filter}) ->
  Filtered = Filter(Metadata),

  ok = oc_stat:record(Filtered, Name, Value).

build_name(NormalizedName) ->
  Stringified = [atom_to_list(Atom) || Atom <- NormalizedName],
  Joined = lists:join($/, Stringified),

  list_to_binary(Joined).

aggregation('Elixir.Telemetry.Metrics.Counter', _) ->
  oc_stat_aggregation_count;
aggregation('Elixir.Telemetry.Metrics.Sum', _) ->
  oc_stat_aggregation_sum;
aggregation('Elixir.Telemetry.Metrics.LastValue', _) ->
  oc_stat_aggregation_latest;
aggregation('Elixir.Telemetry.Metrics.Distribution', #{buckets := Buckets}) ->
  {oc_stat_aggregation_distribution, [{buckets, Buckets}]}.

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

build_name_test() ->
  ?assertEqual(<<"foo/bar">>, build_name([foo, bar])).

-endif.
