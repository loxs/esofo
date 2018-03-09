-module(db_handler_tester).

-export([start_db_handlers/2]).

start_db_handlers(StartFrom, Number) ->
    spawn(fun() ->
                  start_db_handlers(StartFrom, StartFrom+Number, StartFrom)
          end).

start_db_handlers(StartFrom, End, Current) when End > Current ->
    db_handler:start_user(Current),
    case Current rem 10000 of
        0 ->
            lager:info("started db_handler ~p", [Current]),
            timer:sleep(100);
        _ -> pass
    end,
    start_db_handlers(StartFrom, End, Current+1);
start_db_handlers(_, _, _) ->
    ok.
