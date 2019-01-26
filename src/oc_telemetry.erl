-module(oc_telemetry).

% ------------------------------------------------------------------------------

% Public Exports
-export([attach/4]).

% ------------------------------------------------------------------------------

-record(options, {name}).

% ------------------------------------------------------------------------------

-spec attach(oc_stat_measure:name(),
             telemetry:event_name(), oc_stat_measure:description(),
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
