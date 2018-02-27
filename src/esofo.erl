-module(esofo).

-export([start_sofo_sup/1]).


start_sofo_sup(WorkerModule) ->
    esofo_sup:start_sofo_sup(WorkerModule, #{}).
