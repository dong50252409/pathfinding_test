pathfinding_test
=====

寻路性能测试Demo

编译
-----

    $ rebar3 compile

使用
----
    AStar =  astar_test:test([]).
    JPS = jps_test:test().
    %% 测试深度优先迷宫地图的寻路性能
    %% 100 X 100 200 X 200 400 X 400 800 X 800种尺寸
    pathfinding_test:t1(dfs_maze, 2, 100, 100, 4).

生成迷宫地图图片
----
    python maze_generate.py maze_path      