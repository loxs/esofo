-module(esofo).

-export([start_sofo_sup/1]).


-spec start_sofo_sup(atom()) -> term().
start_sofo_sup(WorkerModule) when is_atom(WorkerModule) ->
    esofo_sup:start_sofo_sup(WorkerModule, #{}).
