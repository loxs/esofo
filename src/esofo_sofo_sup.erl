%%%-------------------------------------------------------------------
%% @doc esofo top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(esofo_sofo_sup).

-behaviour(supervisor).

%% API
-export([start_link/2,
         start_child/4]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

start_link(WorkerModule, SupOptions) ->
    supervisor:start_link({via, global, {node(), ?MODULE, WorkerModule}}, ?MODULE,
                          [WorkerModule, SupOptions]).

start_child(WorkerModule, ID, WorkerArgs, EsofoGSOptions)
  when is_atom(WorkerModule) andalso is_map(WorkerArgs)
       andalso is_map(EsofoGSOptions) ->
    SupPid = case global:whereis_name({node(), ?MODULE, WorkerModule}) of
        undefined ->
            {ok, Pid} = esofo_sup:start_sofo_sup(WorkerModule, #{}),
            Pid;
        Pid when is_pid(Pid) ->
            Pid
    end,
    supervisor:start_child(SupPid, [ID, WorkerArgs, EsofoGSOptions]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([WorkerModule, _Options]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 0,
                 period => 1},
    ChildSpecs = #{id => WorkerModule,
                   type => worker,
                   start => {esofo_gen_server, start_link, [WorkerModule]},
                   restart => transient,
                   shutdown => 5000},
    {ok, {SupFlags, [ChildSpecs]} }.

%%====================================================================
%% Internal functions
%%====================================================================
