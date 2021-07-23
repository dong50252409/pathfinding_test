-module(jps_tests).

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
        case jps:search({1, 1}, {Width, High}, ValidFun, Options) of
            {jump_points, Path} ->
                {full_path, FullPath} = jps:get_full_path(Path);
            _Other ->
                FullPath = []
        end,
        proplists:get_value(is_output, Options, false) andalso pathfinding_test:draw_maze(FullPath, Maze)
    end.

