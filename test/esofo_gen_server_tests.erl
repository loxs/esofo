-module(esofo_gen_server_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

gen_server_global_test_() ->
    gen_server(esofo_gen_server_global).

gen_server_gproc_test_() ->
    gen_server(esofo_gen_server_gproc).

gen_server(Module) ->
    ModStr = erlang:atom_to_list(Module),
    {setup,
     fun() ->
             ok = application:start(gproc),
             ok = application:start(esofo),
             {ok, _} = esofo:start_sofo_sup(Module)
     end,
     fun(_) ->
             ok = application:stop(esofo),
             ok = application:stop(gproc)
     end,
     fun(_) ->
     {inparallel, [
                   {"hibernation " ++ ModStr, ?_test(hibernation(Module))}
                  , {"shutdown " ++ ModStr, ?_test(shutdown(Module))}
                  , {"test_init " ++ ModStr, ?_test(test_init(Module))}
                  , {"test_call " ++ ModStr, ?_test(test_call(Module))}
                  , {"test_cast " ++ ModStr, ?_test(test_cast(Module))}
                  , {"test_info " ++ ModStr, ?_test(test_info(Module))}
                  ]}
     end}.


test_init(Module) ->
    ID = test_init,
    {ok, Pid} = esofo_gen_server:start({Module, ID},
                                       #{init_action => hibernate}, #{}),
    assure_hibernated(Pid),
    ok = esofo_gen_server:stop({Module, ID}),

    {ok, _Pid1} = esofo_gen_server:start({Module, ID},
                                       #{init_action => timeout}, #{}),
    ok = esofo_gen_server:stop({Module, ID}),

    {error, normal} = esofo_gen_server:start({Module, ID},
                                       #{init_action => stop}, #{}),

    {ok, undefined} = esofo_gen_server:start({Module, ID},
                                        #{init_action => ignore}, #{}).


test_call(Module) ->
    ID = esofo_gen_server_call,
    _ = start_worker(Module, ID, infinity, infinity),
    replied = esofo_gen_server:call({Module, ID}, test_reply),
    hibernated = esofo_gen_server:call({Module, ID}, test_reply_hibernate),
    {ok, Pid} = esofo_gen_server:find({Module, ID}),
    assure_hibernated(Pid),
    timeout = esofo_gen_server:call({Module, ID}, test_reply_timeout),

    noreply = esofo_gen_server:call({Module, ID}, test_noreply),
    noreply_hibernate = esofo_gen_server:call(
                               {Module, ID}, test_noreply_hibernate),
    assure_hibernated(Pid),
    noreply_timeout = esofo_gen_server:call(
                             {Module, ID}, test_noreply_timeout),

    stopped = esofo_gen_server:call({Module, ID}, test_stop_reply),
    timer:sleep(100),
    false = erlang:is_process_alive(Pid),
    Pid1 = start_worker(Module, ID, infinity, infinity),
    stopped = esofo_gen_server:call({Module, ID}, test_stop_noreply),
    timer:sleep(100),
    false = erlang:is_process_alive(Pid1).

test_cast(Module) ->
    ID = esofo_gen_server_cast,
    _ = start_worker(Module, ID, infinity, infinity),
    {ok, Pid} = esofo_gen_server:find({Module, ID}),

    ok = esofo_gen_server:cast({Module, ID}, cast_noreply),
    true = erlang:is_process_alive(Pid),

    ok = esofo_gen_server:cast({Module, ID}, cast_noreply_hibernate),
    timer:sleep(timer:seconds(1)),
    assure_hibernated(Pid),

    ok = esofo_gen_server:cast({Module, ID}, cast_noreply_timeout),
    true = erlang:is_process_alive(Pid),

    ok = esofo_gen_server:cast({Module, ID}, cast_noreply_stop),
    timer:sleep(timer:seconds(1)),
    false = erlang:is_process_alive(Pid).

test_info(Module) ->
    ID = esofo_gen_server_info,
    _ = start_worker(Module, ID, infinity, infinity),
    {ok, Pid} = esofo_gen_server:find({Module, ID}),

    Pid ! info_noreply,
    true = erlang:is_process_alive(Pid),

    Pid ! info_noreply_hibernate,
    timer:sleep(timer:seconds(1)),
    assure_hibernated(Pid),

    Pid ! info_noreply_timeout,
    true = erlang:is_process_alive(Pid),

    Pid ! info_noreply_stop,
    timer:sleep(timer:seconds(1)),
    false = erlang:is_process_alive(Pid).


hibernation(Module) ->
    Pid = start_worker(Module, hibernation_tester, 1, infinity),
    timer:sleep(timer:seconds(3)),
    assure_hibernated(Pid).

assure_hibernated(Pid) ->
    {current_function, {erlang, hibernate, 3}} = erlang:process_info(
                                                   Pid, current_function).
shutdown(Module) ->
    Pid = start_worker(Module, shutdown_tester, infinity, 1),
    timer:sleep(timer:seconds(3)),
    false = erlang:is_process_alive(Pid).

start_worker(Module, ID, Hibernate, Shutdown) ->
    {ok, Pid} = esofo_gen_server:start({Module, ID}, #{},
                                       #{hibernate => Hibernate,
                                         shutdown => Shutdown}),
    Pid.

-endif.
