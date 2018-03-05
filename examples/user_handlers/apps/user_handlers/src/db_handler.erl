-module(db_handler).

-behaviour(gen_server).

%% API
-export([esofo_registry/0,
         start_user/1,
         get_user_data/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
         id = 0
         }).

%%%===================================================================
%%% API
%%%===================================================================
esofo_registry() ->
    gproc.

start_user(ID) ->
    esofo_gen_server:start({?MODULE, ID},
                           #{arg_a => "A", arg_b => "B"},
                           #{hibernate => timer:seconds(10),
                             shutdown => timer:minutes(30)}).

get_user_data(ID) ->
    esofo_gen_server:call({?MODULE, ID}, get_user_data).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(#{id := ID, args := _Args}) ->
    %% lager:info("starting db_handler with id ~p and args ~p", [ID, Args]),
    {ok, #state{id = ID}}.

handle_call(noreply, From, State) ->
    gen_server:reply(From, noreply_reply),
    {noreply, State};
handle_call(stop, _From, State) ->
    {stop, normal, State};
handle_call(stop_reply, _From, State) ->
    {stop, normal, stopped, State};
handle_call(Request, _From, State) ->
    lager:error("Unknown call to ~s: ~p", [?MODULE, Request]),
    {reply, unknown_call, State}.

handle_cast(hibernate, State) ->
    lager:info("Will hibernate"),
    {noreply, State, hibernate};
handle_cast(Msg, State) ->
    lager:error("Unknown cast to ~s: ~p", [?MODULE, Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    lager:error("Unknown info received by ~s: ~p", [?MODULE, Info]),
    {noreply, State}.

terminate(_Reason, #state{id = _ID}) ->
    %% lager:info("Terminating db_handler ~p", [ID]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
