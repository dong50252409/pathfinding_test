-module(gbfs_test).

%% API
-export([test/1, test_fast/1]).

test(Options) ->
    fun(Maze) ->
        Width = size(element(1, Maze)),
        High = size(Maze),
        ValidFun =
            fun({X, Y}) ->
                X > 0 andalso X =< Width andalso Y > 0 andalso Y =< High andalso element(X, element(Y, Maze)) =/= 0
            end,
        StarPos = proplists:get_value(star_pos, Options, {3, 3}),
        EndPos = proplists:get_value(end_pos, Options, {Width - 3, High - 3}),
        case gbfs:search(StarPos, EndPos, ValidFun, Options) of
            {ok, FullPath} ->
                FullPath;
            _Other ->
                io:format("Pathfinding Failed Reason:~w~n", [_Other]),
                []
        end
    end.

test_fast(Options) ->
    fun(Maze) ->
        Width = size(element(1, Maze)),
        High = size(Maze),
        ValidFun =
            fun({X, Y}) ->
                X > 0 andalso X =< Width andalso Y > 0 andalso Y =< High andalso element(X, element(Y, Maze)) =/= 0
            end,
        StarPos = proplists:get_value(star_pos, Options, {3, 3}),
        EndPos = proplists:get_value(end_pos, Options, {Width - 3, High - 3}),
        case gbfs_fast:search(StarPos, EndPos, ValidFun, Options) of
            {ok, FullPath} ->
                FullPath;
            _Other ->
                io:format("Pathfinding Failed Reason:~w~n", [_Other]),
                []
        end
    end.
