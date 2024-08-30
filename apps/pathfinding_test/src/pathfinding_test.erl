-module(pathfinding_test).

%% API
-export([gen_mazes/4, benchmark/2, benchmark/3, visual_run/2]).

%% @doc 生成迷宫
gen_mazes(MazeMod, Width, High, Times) ->
    Pid = self(),
    PidList = [spawn(fun() -> Pid ! {self(), MazeMod:create_maze(Width, High)} end) || _ <- lists:seq(1, Times)],
    Mazes = gen_mazes_1(PidList),
    Str = io_lib:format("~w.", [Mazes]),
    Str1 = string:replace(Str, <<"},">>, <<"},\n">>, all),
    Filename = io_lib:format("~w.data", [MazeMod]),
    file:write_file(Filename, Str1).

gen_mazes_1([]) ->
    [];
gen_mazes_1(PidList) ->
    receive
        {Pid, Maze} ->
            [Maze | gen_mazes_1(lists:delete(Pid, PidList))]
    end.

%% @doc 运行测试
benchmark(Filename, FunList) when is_list(Filename);is_binary(Filename) ->
    benchmark(Filename, FunList, #{}).
benchmark(Filename, FunList, RunOptions) when is_list(Filename);is_binary(Filename) ->
    {ok, [Mazes]} = file:consult(Filename),
    Codes = [fun() -> Fun(Mazes) end || Fun <- FunList],
    Reports = erlperf:compare(Codes, RunOptions#{report => full}),
    io:format("~s~n", [erlperf_cli:format(Reports, #{viewport_width => 120, format => basic})]).

%% @doc 运行寻路算法，并生成可视化结果
visual_run(Filename, FunList) ->
    {ok, [Mazes]} = file:consult(Filename),
    Width = size(erlang:element(1, hd(Mazes))),
    High = erlang:size(erlang:hd(Mazes)),
    lists:foreach(
        fun({N, Maze}) ->
            lists:foreach(
                fun(Fun) ->
                    FullPath = Fun(Maze),
                    {_, Mod} = erlang:fun_info(Fun, module),
                    OutFilename = io_lib:format("~w.output",[Mod]),
                    draw_maze(N, Maze, FullPath, OutFilename),
                    io:format("Mod:~w N:~w Width:~w, High:~w~n", [Mod, N, Width, High])
                end, FunList)
        end, lists:enumerate(Mazes)).


draw_maze(Num, Maze, Path, Filename) ->
    Width = size(element(1, Maze)),
    {ok, IO} = file:open(Filename, [append]),
    Maze1 = draw_maze_path(Path, Maze),
    Border = io_lib:format("~s~n", [lists:duplicate(Width + 2, $X)]),
    Str = draw_maze(tuple_to_list(Maze1)),
    io:format(IO, "Num:~w Len:~w~n~s~n", [Num, length(Path), [Border, Str, Border]]),
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
