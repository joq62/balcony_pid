%%%-------------------------------------------------------------------
%% @doc control public API
%% @end
%%%-------------------------------------------------------------------

-module(balcony_pid_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    balcony_pid_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
