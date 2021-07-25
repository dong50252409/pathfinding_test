-module(pathfinding_test).

%% API
-export([gen_mazes/4, test/1, draw_maze/2, t/0]).

gen_mazes(MazeMod, Width, High, Times) ->
    Mazes = [MazeMod:create_maze(Width, High) || _ <- lists:seq(1, Times)],
    Str = io_lib:format("{mazes,~w}.", [Mazes]),
    Str1 = string:replace(Str, <<"},">>, <<"},\n">>, all),
    file:write_file("mazes.data", Str1).

test(Fun) ->
    {ok, Data} = file:consult("mazes.data"),
    Mazes = proplists:get_value(mazes, Data, []),
    tc:t(lists, foreach, [Fun, Mazes], 1).

t() ->
    {ok, Data} = file:consult("mazes.data"),
    [Maze] = proplists:get_value(mazes, Data, []),
    Width = size(element(1, Maze)),
    High = size(Maze),
    Fun = fun(X, Y) -> element(X, element(Y, Maze)) =:= 0 end,
    [io:format("this.setWalkableAt(~w, ~w, false);~n", [X-1, Y-1]) || X <- lists:seq(1, Width), Y <- lists:seq(1, High), Fun(X, Y)].


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
