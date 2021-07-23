-module(pathfinding_test).

%% API
-export([gen_mazes/4, test/1, draw_maze/2]).

gen_mazes(MazeMod, Width, Col, Times) ->
    Mazes = [MazeMod:create_maze(Width, Col) || _ <- lists:seq(1, Times)],
    file:write_file("mazes.data", term_to_binary(Mazes)).

test(Fun) ->
    {ok, Data} = file:read_file("mazes.data"),
    Mazes = binary_to_term(Data),
    tc:t(lists, foreach, [Fun, Mazes], 1).

draw_maze(Path, Maze) ->
    Width = size(element(1, Maze)),
    {ok, IO} = file:open("draw_mazes.out", [write]),
    Maze1 = draw_maze_path(Path, Maze),
    Border = io_lib:format("~s~n", [lists:duplicate(Width + 2, $X)]),
    Str = draw_maze(tuple_to_list(Maze1)),
    io:format(IO, "~s~n", [[Border, Str, Border]]),
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
