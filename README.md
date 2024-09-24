pathfinding_test
=====

寻路性能测试Demo

编译
-----

    $ rebar3 compile

使用
----
    1> Options = [].
    2> F1 = pathfinding_test:gen_test_fun(fun gbfs:search/4, Options).      % gbfs寻路算法
    3> F2 = pathfinding_test:gen_test_fun(astar, search, Options).          % astar寻路算法
    4> F3 = fun(Maze) -> {W, H, F} = pathfinding_test:get_maze_info(Maze), {_, L} = jps:search({3, 3},{W - 3, H - 3}, F, Options), {ok, L} end. % jps寻路算法
    5> pathfinding_test:gen_mazes(island_maze, 100, 100, 1).           % 生成一张100 X 100地图，还可以生成迷宫类型的地图，也可以一次生成多张
    6> pathfinding_test:benchmark("island_maze.data", [F1, F2, F3]).   % 运行测试
    Code                                                                                      ||        QPS       Time   Rel
    [fun gbfs:search/4,[]]                                                                     1       1734     575 us  100%
    [{{1,6},[],{eval,#Fun<shell.21.78299858>},{value,#Fun<shell.5.78299858>},#{},[{cla         1        610    1632 us   35%
    [astar,search,[]]                                                                          1         41   24500 us    2%
    7> pathfinding_test:visual_run("island_maze.data", [F1, F2, F3]).  % 生成可视化地图

生成迷宫地图图片
----
    python maze_generate.py maze_path      