-module(pathfinding_test).


%% TEST_API
-export([t1/5]).

%% API
-export([gen_mazes/4, test/3, test/6, test/5]).

t1(MazeMod, Multi, Width, High, Times) when Times > 0 ->
    test(MazeMod, Width, High, 10, 10, false),
    t1(MazeMod, Multi, Width * Multi, High * Multi, Times - 1);
t1(MazeMod, Multi, Width, High, Times) ->
    t1(MazeMod, Multi, Width, High, Times - 1).

gen_mazes(MazeMod, Width, High, Times) ->
    Mazes = [MazeMod:create_maze(Width, High) || _ <- lists:seq(1, Times)],
    Str = io_lib:format("~w.", [Mazes]),
    Str1 = string:replace(Str, <<"},">>, <<"},\n">>, all),
    file:write_file(io_lib:format("~w.data", [MazeMod]), Str1).

test(Filename, Times, IsOutput) when is_list(Filename);is_binary(Filename) ->
    {ok, [Mazes]} = file:consult(Filename),
    Width = size(element(1, hd(Mazes))),
    High = size(hd(Mazes)),
    Mazes1 = lists:zip(lists:seq(1, length(Mazes)), Mazes),
    test(Mazes1, Width, High, Times, IsOutput).

test(MazeMod, Width, High, Num, Times, IsOutput) when is_atom(MazeMod) ->
    Mazes = [{N, MazeMod:create_maze(Width, High)} || N <- lists:seq(1, Num)],
    test(Mazes, Width, High, Times, IsOutput).

test(Mazes, Width, High, Times, IsOutput) ->
    JPSFun = jps_test:test([{max_limit, 16#FFFFFFFF}]),
    Fun1 =
        fun({N, Maze}) ->
            FullPath = JPSFun(Maze),
            try_output("jps.output", N, FullPath, Maze, IsOutput)
        end,

    io:format("JPS Width:~w, High:~wn", [Width, High]),
    tc:t(lists, foreach, [Fun1, Mazes], Times),

    AStarFun = astar_test:test([{max_limit, 16#FFFFFFFF}]),
    Fun2 =
        fun({N, Maze}) ->
            FullPath = AStarFun(Maze),
            try_output("astar.output", N, FullPath, Maze, IsOutput)
        end,

    io:format("AStar Width:~w, High:~wn", [Width, High]),
    tc:t(lists, foreach, [Fun2, Mazes], Times).

try_output(Filename, Num, FullPath, Maze, true) ->
    draw_maze(Num, Filename, FullPath, Maze);
try_output(_Filename, _Num, _FullPath, _Maze, false) ->
    ok.

draw_maze(Num, Filename, Path, Maze) ->
    Width = size(element(1, Maze)),
    {ok, IO} = file:open(Filename, [append]),
    Maze1 = draw_maze_path(Path, Maze),
    Border = io_lib:format("~s~n", [lists:duplicate(Width + 2, $X)]),
    Str = draw_maze(tuple_to_list(Maze1)),
    io:format(IO, "Num:~w Len:~w~n ~s~n", [Num, length(Path), [Border, Str, Border]]),
    file:close(IO).

draw_maze([Width | T]) ->
    [io_lib:format("X~sX~n", [lists:map(fun draw_maze_1/1, tuple_to_list(Width))]) | draw_maze(T)];
draw_maze([]) ->
    [].

draw_maze_1(0) ->
    $X;
draw_maze_1(1) ->
    " ";
draw_maze_1(E) ->
    E.

draw_maze_path([{X, Y} | T], Maze) ->
    Maze1 = setelement(Y, Maze, setelement(X, element(Y, Maze), $o)),
    draw_maze_path(T, Maze1);
draw_maze_path(_, Maze) ->
    Maze.
