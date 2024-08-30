-module(jps_test).

%% API
-compile(export_all).
-compile(nowarn_export_all).

test(Options) ->
    fun(Mazes) ->
        lists:foreach(
            fun(Maze) ->
                Width = size(element(1, Maze)),
                High = size(Maze),
                ValidFun =
                    fun({X, Y}) ->
                    X > 0
                    andalso X =< Width
                    andalso Y > 0
                    andalso Y =< High
                    andalso element(X, element(Y, Maze)) =/= 0
                    end,
                StarPos = proplists:get_value(start_pos, Options, {3, 3}),
                EndPos = proplists:get_value(end_pos, Options, {Width - 3, High - 3}),
                case jps:search(StarPos, EndPos, ValidFun, Options) of
                    {ok, FullPath} -> FullPath;
                    _Other ->
                        io:format("Pathfinding Failed Reason:~w~n", [_Other]),
                        []
                end
            end, Mazes)
    end.

