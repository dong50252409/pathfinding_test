%%%-------------------------------------------------------------------
%% @doc pathfinding_test public API
%% @end
%%%-------------------------------------------------------------------

-module(pathfinding_test_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    pathfinding_test_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
