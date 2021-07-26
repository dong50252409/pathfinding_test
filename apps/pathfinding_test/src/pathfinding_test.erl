-module(pathfinding_test).

%% API
-export([gen_mazes/4, test/1, test/2]).

gen_mazes(MazeMod, Width, High, Times) ->
    Mazes = [MazeMod:create_maze(Width, High) || _ <- lists:seq(1, Times)],
    Str = io_lib:format("{mazes,~w}.", [Mazes]),
    Str1 = string:replace(Str, <<"},">>, <<"},\n">>, all),
    file:write_file("mazes.data", Str1).

test(PFFun) ->
    test(PFFun, false).
test(PFFun, IsOutput) ->
    {module, Mod} = erlang:fun_info(PFFun, module),
    Fun =
        fun(Maze, {AccFullPath, Num}) ->
            FullPath = PFFun(Maze),
            case IsOutput of
                true ->
                    draw_maze(Num, atom_to_list(Mod) ++ ".output", FullPath, Maze);
                false ->
                    ok
            end,
            {[{Num, FullPath} | AccFullPath], Num + 1}
        end,
    {ok, Data} = file:consult("mazes.data"),
    Mazes = proplists:get_value(mazes, Data, []),
    tc:t(lists, foldr, [Fun, {[], 1}, Mazes], 1).

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
