-module(astar_test).

%% API
-export([test/1]).
test(Options) ->
    fun(Maze) ->
        Width = size(element(1, Maze)),
        High = size(Maze),
        ValidFun =
            fun({X, Y}) ->
                X > 0 andalso X =< Width andalso Y > 0 andalso Y =< High
                    andalso element(X, element(Y, Maze)) =/= 0
            end,
        case astar:search({1, 1}, {Width, High}, ValidFun, Options) of
            {max, FullPath} ->
                FullPath;
            _Other ->
                []
        end
    end.
