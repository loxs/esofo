-module(esofo_gen_server_gproc).

-export([esofo_registry/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
          id = 0
         }).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
esofo_registry() ->
    gproc.

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

handle_cast(cast_noreply, State) ->
    {noreply, State};
handle_cast(cast_noreply_hibernate, State) ->
    {noreply, State, hibernate};
handle_cast(cast_noreply_timeout, State) ->
    {noreply, State, 5};
handle_cast(cast_noreply_stop, State) ->
    {stop, normal, State}.

handle_info(info_noreply, State) ->
    {noreply, State};
handle_info(info_noreply_hibernate, State) ->
    {noreply, State, hibernate};
handle_info(info_noreply_timeout, State) ->
    {noreply, State, timeout};
handle_info(info_noreply_stop, State) ->
    {stop, normal, State}.

terminate(_Reason, #state{id = _ID}) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
