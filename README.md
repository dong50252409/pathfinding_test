pathfinding_test
=====

寻路性能测试Demo

编译
-----

    $ rebar3 compile

使用
----
    F1 = astar_test:test([]).
    F2 = jps_test:test([]).
    F3 = gbfs_test:test([]).
    %% 生成一个迷宫地图
    %% 100 X 100 200 X 200 400 X 400 800 X 800种尺寸
    pathfinding_test:gen_maze(island_maze, 100, 100, 1),
    pathfinding_test:benchmark("island_maze.data", [F1, F2, F3]).

生成迷宫地图图片
----
    python maze_generate.py maze_path      