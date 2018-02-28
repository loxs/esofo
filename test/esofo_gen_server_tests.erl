-module(esofo_gen_server_tests).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
          id = 0
         }).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(#{id := ID, args := #{init_action := hibernate}}) ->
    {ok, #state{id = ID}, hibernate};
init(#{id := ID, args := #{init_action := timeout}}) ->
    {ok, #state{id = ID}, 0};
init(#{id := _ID, args := #{init_action := stop}}) ->
    {stop, normal};
init(#{id := _ID, args := #{init_action := ignore}}) ->
    ignore;
init(#{id := ID, args := _Args}) ->
    {ok, #state{id = ID}}.

handle_call(test_reply, _From, State) ->
    {reply, replied, State};
handle_call(test_reply_hibernate, _From, State) ->
    {reply, hibernated, State, hibernate};
handle_call(test_reply_timeout, _From, State) ->
    {reply, timeout, State, 1};

handle_call(test_noreply, From, State) ->
    gen_server:reply(From, noreply),
    {noreply, State};
handle_call(test_noreply_hibernate, From, State) ->
    gen_server:reply(From, noreply_hibernate),
    {noreply, State, hibernate};
handle_call(test_noreply_timeout, From, State) ->
    gen_server:reply(From, noreply_timeout),
    {noreply, State, 1};

handle_call(test_stop_reply, _From, State) ->
    {stop, normal, stopped, State};
handle_call(test_stop_noreply, From, State) ->
    gen_server:reply(From, stopped),
    {stop, normal, State};

handle_call(Request, _From, State) ->
    {reply, {unknown_call, Request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{id = _ID}) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

top_level_test_() ->
    {setup,
     fun() ->
             ok = application:start(esofo),
             esofo:start_sofo_sup(?MODULE)
     end,
     fun(_) -> ok = application:stop(esofo) end,
     fun(_) ->
     {inparallel, [
                   {"hibernation", ?_test(hibernation())}
                   , {"shutdown", ?_test(shutdown())}
                   , {"init", ?_test(test_init())}
                   , {"test_call", ?_test(test_call())}
                  ]}
     end}.


test_init() ->
    ID = test_init,
    {ok, Pid} = esofo_gen_server:start({?MODULE, ID},
                                       #{init_action => hibernate}, #{}),
    assure_hibernated(Pid),
    ok = esofo_gen_server:stop({?MODULE, ID}),

    {ok, _Pid1} = esofo_gen_server:start({?MODULE, ID},
                                       #{init_action => timeout}, #{}),
    ok = esofo_gen_server:stop({?MODULE, ID}),

    {error, normal} = esofo_gen_server:start({?MODULE, ID},
                                       #{init_action => stop}, #{}),

    {ok, undefined} = esofo_gen_server:start({?MODULE, ID},
                                        #{init_action => ignore}, #{}).


test_call() ->
    ID = esofo_gen_server_call,
    _ = start_worker(ID, infinity, infinity),
    replied = esofo_gen_server:call({?MODULE, ID}, test_reply),
    hibernated = esofo_gen_server:call({?MODULE, ID}, test_reply_hibernate),
    {ok, Pid} = esofo_gen_server:find({?MODULE, ID}),
    assure_hibernated(Pid),
    timeout = esofo_gen_server:call({?MODULE, ID}, test_reply_timeout),

    noreply = esofo_gen_server:call({?MODULE, ID}, test_noreply),
    noreply_hibernate = esofo_gen_server:call(
                               {?MODULE, ID}, test_noreply_hibernate),
    assure_hibernated(Pid),
    noreply_timeout = esofo_gen_server:call(
                             {?MODULE, ID}, test_noreply_timeout),

    stopped = esofo_gen_server:call({?MODULE, ID}, test_stop_reply),
    false = erlang:is_process_alive(Pid),
    Pid1 = start_worker(ID, infinity, infinity),
    stopped = esofo_gen_server:call({?MODULE, ID}, test_stop_noreply),
    false = erlang:is_process_alive(Pid1).

hibernation() ->
    Pid = start_worker(hibernation_tester, 1, infinity),
    timer:sleep(timer:seconds(3)),
    assure_hibernated(Pid).

assure_hibernated(Pid) ->
    {current_function, {erlang, hibernate, 3}} = erlang:process_info(
                                                   Pid, current_function).
shutdown() ->
    Pid = start_worker(shutdown_tester, infinity, 1),
    timer:sleep(timer:seconds(3)),
    false = erlang:is_process_alive(Pid).

start_worker(ID, Hibernate, Shutdown) ->
    {ok, Pid} = esofo_gen_server:start({?MODULE, ID}, #{},
                                       #{hibernate => Hibernate,
                                         shutdown => Shutdown}),
    Pid.

-endif.
