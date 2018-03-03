-module(esofo_gen_server).

-behaviour(gen_server).

%% API
-export([
         start/1,
         start/3,
         find/1,
         call/2,
         call/3,
         cast/2,
         stop/1,
         stop/3
        ]).

%% gen_server callbacks
-export([start_link/4,
         init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================
start({WorkerModule, ID}) ->
    start({WorkerModule, ID}, #{}, #{}).

-spec start({atom(), term()}, map(), map()) ->
                   {ok, pid()} | {error, {already_started, pid()}}.
start({WorkerModule, ID}, Args, GivenOptions)
  when is_atom(WorkerModule) andalso is_map(Args) andalso is_map(GivenOptions) ->
    DefaultOptions = #{hibernate => 0, shutdown => infinity},
    Options = maps:merge(DefaultOptions, GivenOptions),
    esofo_sofo_sup:start_child(WorkerModule, ID, Args, Options).

find({WorkerModule, ID}) ->
    case global:whereis_name({?MODULE, WorkerModule, ID}) of
        undefined ->
            {error, not_found};
        Pid when is_pid(Pid) ->
            {ok, Pid}
    end.

-spec call({atom, term()}, term()) -> term().
call({WorkerModule, ID}, Message) ->
    gen_server:call(name({WorkerModule, ID}), Message).

-spec call({atom, term()}, term(), pos_integer()) -> term().
call({WorkerModule, ID}, Message, Timeout) ->
    gen_server:call(name({WorkerModule, ID}), Message, Timeout).

-spec cast({atom, term()}, term()) -> ok.
cast({WorkerModule, ID}, Message) ->
    gen_server:cast(name({WorkerModule, ID}), Message).

-spec stop({atom, term()}) -> ok.
stop({WorkerModule, ID}) ->
    gen_server:stop(name({WorkerModule, ID})).

-spec stop({atom, term()}, term(), pos_integer()) -> ok.
stop({WorkerModule, ID}, Reason, Timeout) ->
    gen_server:stop(name({WorkerModule, ID}), Reason, Timeout).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
-record(state, {
          worker_module :: atom(),
          id :: term(),
          worker_state :: term(),
          hibernate_after = 0 :: pos_integer() | infinity,
          hibernate_timer :: timer:tref() | undefined,
          shutdown_after = infinity :: pos_integer() | infinity,
          shutdown_timer :: timer:tref() | undefined
         }).

start_link(WorkerModule, ID, WorkerArgs, Options) ->
    gen_server:start_link(name({WorkerModule, ID}),
                          ?MODULE, [WorkerModule, ID, WorkerArgs, Options], []).

init([WorkerModule, ID, WorkerArgs, Options]) ->
    #{hibernate := Hibernate, shutdown := Shutdown} = Options,
    State0 = #state{worker_module = WorkerModule,
                    id = ID,
                    hibernate_after = Hibernate,
                    shutdown_after = Shutdown },
    State = set_timers(State0),
    case WorkerModule:init(#{id => ID, args => WorkerArgs}) of
        {ok, WorkerState} ->
            {ok, State#state{worker_state = WorkerState}};
        {ok, WorkerState, hibernate} ->
            {ok, State#state{worker_state = WorkerState}, hibernate};
        {ok, WorkerState, Timeout}
          when is_integer(Timeout) andalso Timeout >= 0 ->
            {ok, State#state{worker_state = WorkerState}, Timeout};
        {stop, Reason} -> {stop, Reason};
        ignore -> ignore
    end.

handle_call(Request, From, #state{worker_module=WM,
                                  worker_state=WState0}=State0) ->
    State = set_timers(State0),
    case WM:handle_call(Request, From, WState0) of
        {reply, Reply, WState}  ->
            {reply, Reply, State#state{worker_state=WState}};
        {reply, Reply, WState, hibernate} ->
            {reply, Reply, State#state{worker_state=WState}, hibernate};
        {reply, Reply, WState, Timeout}
          when is_integer(Timeout) andalso Timeout >= 0 ->
            {reply, Reply, State#state{worker_state=WState}, Timeout};

        {noreply, WState}  ->
            {noreply, State#state{worker_state=WState}};
        {noreply, WState, hibernate} ->
            {noreply, State#state{worker_state=WState}, hibernate};
        {noreply, WState, Timeout}
          when is_integer(Timeout) andalso Timeout >= 0 ->
            {noreply, State#state{worker_state=WState}, Timeout};

        {stop, Reason, WState}->
            {stop, Reason, State#state{worker_state=WState}};
        {stop, Reason, Reply, WState}->
            {stop, Reason, Reply, State#state{worker_state=WState}}

    end.

handle_cast(Msg, #state{worker_state=WState0, worker_module=WM}=State0) ->
    State = set_timers(State0),
    case WM:handle_cast(Msg, WState0) of
        {noreply, WState} ->
            {noreply, State#state{worker_state=WState}};
        {noreply, WState, hibernate} ->
            {noreply, State#state{worker_state=WState}, hibernate};
        {noreply, WState, Timeout} ->
            {noreply, State#state{worker_state=WState}, Timeout};
        {stop, Reason, WState} ->
            {stop, Reason, State#state{worker_state=WState}}
    end.

handle_info({?MODULE, hibernate}, State) ->
    {noreply, State, hibernate};
handle_info({?MODULE, shutdown}, State) ->
    {message_queue_len, MQL} = erlang:process_info(self(), message_queue_len),
    case MQL of 0 -> {stop, normal, State};
        _ -> {noreply, State}
    end;
handle_info(Info, #state{worker_state=WState0, worker_module=WM}=State0) ->
    State = set_timers(State0),
    case WM:handle_info(Info, WState0) of
        {noreply, WState} ->
            {noreply, State#state{worker_state=WState}};
        {noreply, WState, hibernate} ->
            {noreply, State#state{worker_state=WState}, hibernate};
        {noreply, WState, Timeout} ->
            {noreply, State#state{worker_state=WState}, Timeout};
        {stop, Reason, WState} ->
            {stop, Reason, State#state{worker_state=WState}}
    end.

terminate(Reason, #state{worker_state=WState, worker_module=WM}) ->
    WM:terminate(Reason, WState).

code_change(OldVsn, #state{worker_state=WState0, worker_module=WM}=State, Extra) ->
    {ok, WState} = WM:terminate(OldVsn, WState0, Extra),
    {ok, State#state{worker_state = WState}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
name({WorkerModule, ID}) ->
    {global, {?MODULE, WorkerModule, ID}}.

-spec set_timers(#state{}) -> #state{}.
set_timers(#state{hibernate_after = HAfter, hibernate_timer = OldHTimer,
                  shutdown_after = SAfter, shutdown_timer = OldSTimer }=S) ->
    if OldHTimer =/= undefined -> timer:cancel(OldHTimer); true -> pass end,
    if OldSTimer =/= undefined -> timer:cancel(OldSTimer); true -> pass end,

    HTimer = case HAfter of
                 infinity ->
                     undefined;
                 _ ->
                     {ok, HTmr} = timer:send_after(HAfter, {?MODULE, hibernate}),
                     HTmr
             end,
    STimer = case SAfter of
                 infinity ->
                     undefined;
                 _ ->
                     {ok, STmr} = timer:send_after(SAfter, {?MODULE, shutdown}),
                     STmr
             end,
    S#state{hibernate_timer = HTimer, shutdown_timer = STimer}.
