%%%-------------------------------------------------------------------
%% @doc esofo top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(esofo_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
        start_sofo_sup/2,
        stop_sofo_sup/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_sofo_sup(WorkerModule, Options) ->
    ChildSpec = #{id => {esofo_sofo_sup, WorkerModule},
                  start => {esofo_sofo_sup, start_link, [WorkerModule, Options]},
                  restart => permanent,
                  shutdown => infinity,
                  type => supervisor},
    supervisor:start_child(?SERVER, ChildSpec).

stop_sofo_sup(WorkerModule) ->
   supervisor:terminate_child(?SERVER, {esofo_sofo_sup, WorkerModule}).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},

    Sentry = #{id => esofo_sentry,
               start => {esofo_sentry, start_link, []},
               restart => permanent,
               shutdown => 5000,
               type => worker },
    {ok, { SupFlags, [Sentry]} }.

%%====================================================================
%% Internal functions
%%====================================================================
