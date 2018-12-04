-module(oc_telemetry).

-export([attach/2, attach/3,
         attach_many/2, attach_many/3]).

attach(HandlerId, EventName) ->
    'Elixir.Opencensus.Telemetry':attach(HandlerId, EventName).
attach(HandlerId, EventName, Opts) ->
    'Elixir.Opencensus.Telemetry':attach(HandlerId, EventName, Opts).

attach_many(HandlerId, EventName) ->
    'Elixir.Opencensus.Telemetry':attach_many(HandlerId, EventName).
attach_many(HandlerId, EventName, Opts) ->
    'Elixir.Opencensus.Telemetry':attach_many(HandlerId, EventName, Opts).
