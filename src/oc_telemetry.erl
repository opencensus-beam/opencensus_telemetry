-module(oc_telemetry).

%% -----------------------------------------------------------------------------

%% Public Exports
-export([attach/5,
         attach/6,
         track/1,
         subscribe_views/1]).

%% -----------------------------------------------------------------------------

-record(oc_measurement, {name :: atom(),
                         value_fun :: fun((maps:map()) -> number() | undefined),
                         tag_values :: fun((maps:map()) -> maps:map())}).

%% @doc Creates measurement and attach it to the telemetry dispatcher
%%
%% On success it returns `{ok, Measurement}' where `Measurement' is newly
%% created OpenCensus measurement that can be used for creating views.
%%
%% When there is already attached listener for given name then it will return
%% `{error, already_registered}'.
-spec attach(oc_stat_measure:name(),
             telemetry:event_name(),
             term() | fun((telemetry:event_measurements()) -> number()),
             oc_stat_measure:description(),
             oc_stat_measure:unit()) -> {ok, oc_stat_measure:measure()}
                                            | {error, already_registered}.
attach(Name, EventName, Measurement, Description, Unit) ->
    attach(Name, EventName, Measurement, Description, Unit, fun(T) -> T end).

-spec subscribe_views(Metrics::[map()]) -> [oc_stat_view:view()].
subscribe_views(Metrics) ->
    [begin
         Name = build_name(NormalizedName),
         Measure = binary_to_atom(build_name(EventName ++ [Measurement]), utf8),
         Aggregation = aggregation(Type, Data),
         {ok, View} = oc_stat_view:subscribe(Name, Measure, Description,
                                             Tags, Aggregation),
         View
     end || #{'__struct__' := Type,
              name := NormalizedName,
              event_name := EventName,
              measurement := Measurement,
              tags := Tags,
              description := Description}=Data <- Metrics].

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
        measurement := Measurement,
        tags := Tags,
        tag_values := TagValues,
        description := Description,
        unit := Unit} = Data) ->
    Name = build_name(NormalizedName),
    case attach(Name, EventName, Measurement, Description, Unit, TagValues) of
        {ok, Measure} ->
            Aggregation = aggregation(Type, Data),
            oc_stat_view:subscribe(Name, Measure, Description,
                                   Tags, Aggregation);
        Error ->
            Error
    end;
track(Events) ->
    [track_(Event) || Event <- Events].

track_({EventName, Measurements}) ->
    Measures = measures(Measurements),
    telemetry:attach(build_name(EventName), EventName,
                     fun handle_event/4, Measures).

measures([]) -> [];
measures([{EventKey, Measurement, Unit} | Rest]) ->
    oc_stat_measure:new(Measurement, <<>>, Unit),
    [{EventKey, Measurement} | measures(Rest)].

attach(Name, EventName, Measurement, Description, Unit, TagValues)
  when is_atom(Measurement) ; is_binary(Measurement) ->
    case oc_stat_measure:exists(Measurement) of
        false ->
            attach_(Name, EventName, Measurement, Description, Unit, TagValues);
        _ ->
            {ok, Measurement}
    end;
attach(Name, EventName, Measurement, Description, Unit, TagValues) ->
    attach_(Name, EventName, Measurement, Description, Unit, TagValues).

attach_(Name, EventName, Measurement, Description, Unit, TagValues) ->
    Measure = oc_stat_measure:new(Name, Description, Unit),
    OCMeasurement = oc_measurement(Name, Measurement, TagValues),
    case telemetry:attach(Name, EventName,
                          fun handle_event/4, OCMeasurement) of
        ok ->
            {ok, Measure};
        Error ->
            Error
    end.

oc_measurement(Name, Measurement, TagValues) when is_atom(Measurement) ->
    #oc_measurement{name=Name,
                    value_fun=fun(Values) ->
                                  maps:get(Measurement, Values, undefined)
                              end,
                    tag_values=TagValues};
oc_measurement(Name, Measurement, TagValues) when is_function(Measurement) ->
    #oc_measurement{name=Name,
                    value_fun=Measurement,
                    tag_values=TagValues}.

%% Handle events send by `telemetry' application
handle_event(_EventName, Values, Tags, #oc_measurement{name=Name,
                                                       value_fun=ValueFun,
                                                       tag_values=TagValues}) ->
    case ValueFun(Values) of
        undefined ->
            ok;
        Value ->
            ok = oc_stat:record(TagValues(Tags), Name, Value)
    end;
handle_event(_EventName, Values, Tags, Measurements) ->
    lists:foreach(fun({Key, Name}) ->
                          case maps:get(Key, Values, undefined) of
                              undefined ->
                                  ok;
                              Value ->
                                  ok = oc_stat:record(Tags, Name, Value)
                          end
                  end, Measurements).

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
