
-module(jpush_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Config) ->
      supervisor:start_link({local, ?MODULE}, ?MODULE, [Config]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Config]) ->
    Jpush = {jpush_server, {jpush_server, start_link, [Config]}, permanent, 5000, worker, [jpush_server]},
    {ok, { {one_for_one, 5, 10}, [Jpush]} }.

