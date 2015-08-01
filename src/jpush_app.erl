-module(jpush_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Config} = application:get_env(jpush, config),
    jpush_sup:start_link(Config).

stop(_State) ->
    ok.
