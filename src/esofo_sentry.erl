-module(esofo_sentry).

-behaviour(gen_server).

-include("esofo.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% API
-export([all_by_last_activity/0]).
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
          %% As percents of EVM system limit
          process_count_limit = 90 :: pos_integer()
         }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ets:new(?ETS_LAST_ACTIVITY, [named_table, public, ordered_set, {keypos, 2},
                                 {write_concurrency, true},
                                 {read_concurrency, true}]),
    erlang:send_after(timer:seconds(1), self(), enforce_limits),
    {ok, #state{}}.

handle_call(Request, _From, State) ->
    error_logger:error_msg("Unknown call to ~p: ~p~n", [?SERVER, Request]),
    {reply, {unknown_call, Request}, State}.

handle_cast(Msg, State) ->
    error_logger:error_msg("Unknown cast to ~p: ~p~n", [?SERVER, Msg]),
    {noreply, State}.

handle_info(enforce_limits, #state{}=State) ->
    enforce_limits(State),
    erlang:send_after(timer:seconds(1), self(), enforce_limits),
    {noreply, State};
handle_info(Info, State) ->
    error_logger:error_msg("Unknown info to ~p: ~p~n", [?SERVER, Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
enforce_limits(#state{process_count_limit = ProcCountLimit}) ->
    ProcessLimit = erlang:system_info(process_limit),
    CurPrCount = erlang:system_info(process_count),
    AllowedPrCount = trunc((ProcessLimit / 100) * ProcCountLimit),
    case CurPrCount > AllowedPrCount of
        true ->
            Overflow = CurPrCount - AllowedPrCount,
            CountToKill = Overflow * 2,
            Before = erlang:system_time(millisecond),
            Cursor = all_by_last_activity(),
            kill_inactive(Cursor, CountToKill, 0),
            qlc:delete_cursor(Cursor),
            After = erlang:system_time(millisecond),
            Seconds = (After - Before) / 1000,
            error_logger:info_msg("esofo_sentri killed ~p workers in ~p seconds",
                                  [CountToKill, Seconds]),
            erlang:garbage_collect();
        false ->
            pass
    end,
    ok.


-spec all_by_last_activity() -> qlc:query_cursor().
all_by_last_activity() ->
    Q = qlc:sort(qlc:q([ X || X <- ets:table(?ETS_LAST_ACTIVITY)])),
    qlc:cursor(qlc:e(Q)).

-spec kill_inactive(qlc:query_cursor(), integer(), integer()) -> ok.
kill_inactive(Cursor, CountToKill, Killed)
  when CountToKill >= Killed ->
    [{_Timestamp, Pid}] = qlc:next_answers(Cursor, 1),
    case erlang:is_process_alive(Pid) of
        false ->
            ets:delete(?ETS_LAST_ACTIVITY, Pid);
        true ->
            Pid ! {esofo_gen_server, shutdown}
    end,
    kill_inactive(Cursor, CountToKill, Killed+1);
kill_inactive(_, _, _) ->
    ok.
