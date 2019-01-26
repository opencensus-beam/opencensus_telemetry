-module(oc_telemetry).

% ------------------------------------------------------------------------------

% Public Exports
-export([attach/4]).

% ------------------------------------------------------------------------------

-record(options, {name}).

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
    Measure = oc_stat_measure:new(Name, Description, Unit),
    Options = #options{name = Name},

    case telemetry:attach(Name, EventName, fun handle_event/4, Options) of
      ok -> {ok, Measure};
      Error -> Error
    end.

% ------------------------------------------------------------------------------

% Handle events send by `telemetry' application
handle_event(_EventName, Value, Metadata,
             #options{name = Name}) ->
    ok = oc_stat:record(Metadata, Name, Value).
