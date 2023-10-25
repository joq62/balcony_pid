%%%-------------------------------------------------------------------
%% @doc console public API
%% @end
%%%-------------------------------------------------------------------

-module(pid_controller_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    pid_controller_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
