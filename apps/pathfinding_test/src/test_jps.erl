%%%-------------------------------------------------------------------
%%% @author DY
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. 7月 2021 14:40
%%%-------------------------------------------------------------------
-module(test_jps).
-author("DY").

%% API
-export([gen_world_maps/3, test/3]).

gen_world_maps(Row, Col, Times) ->
    WorldMaps = [prime_maze:create_maze({1, 1}, {Row, Col}, Row, Col) || _ <- lists:seq(1, Times)],
    file:write_file("world_maps.data", term_to_binary(WorldMaps)).

test(Row, Col, IsShow) ->
    {ok, Data} = file:read_file("world_maps.data"),
    WorldMaps = binary_to_term(Data),
    {ok, IO} = file:open("world_maps.out", [write]),
    put(io, IO),
    {Time, _Value} = timer:tc(fun() -> test_1({1, 1}, {Row, Col}, Row, Col, WorldMaps, IsShow) end),
    file:close(IO),
    Time.

test_1(Start, End, Row, Col, [WorldMap | T], IsShow) ->
    Fun =
        fun({X, Y}) ->
            X > 0 andalso X =< Row andalso Y > 0 andalso Y =< Col
                andalso element(X, element(Y, WorldMap)) =/= $X
        end,
    case jps:search(Start, End, Fun, []) of
        {jump_points, Path} ->
            {full_path, FullPath} = jps:get_full_path(Path);
        _Other ->
            FullPath = []
    end,
    case IsShow of
        true ->
            draw_map(Row, FullPath, WorldMap);
        false ->
            ok
    end,
    test_1(Start, End, Row, Col, T, IsShow);
test_1(_Start, _End, _Row, _Col, [], _IsShow) ->
    ok.

draw_map(Width, Path, WorldMap) ->
    WorldMap1 = draw_map_path(Path, WorldMap),
    Border = io_lib:format("~s~n", [lists:duplicate(Width + 2, $X)]),
    Str = draw_map(tuple_to_list(WorldMap1)),
    io:format(get(io), "~s~n", [[Border, Str, Border]]).

draw_map([Row | T]) ->
    Fun = fun(0) -> $X;(1) -> " ";(E) -> E end,
    [io_lib:format("X~sX~n", [lists:map(Fun, tuple_to_list(Row))]) | draw_map(T)];
draw_map([]) ->
    [].

draw_map_path([{X, Y} | T], WorldMap) ->
    WorldMap1 = setelement(Y, WorldMap, setelement(X, element(Y, WorldMap), $o)),
    draw_map_path(T, WorldMap1);
draw_map_path(_, WorldMap) ->
    WorldMap.
