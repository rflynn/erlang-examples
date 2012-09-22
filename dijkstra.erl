% perform Dijkstra's algorithm over a directed graph

-module(dijkstra).
-author("Ryan Flynn parseerror@gmail.com www.parseerror.com github.com/rflynn").
-export(
    [
        dijkstra/2,
        test/0, tests/0
    ]).

% given a starting point and a weighted graph,
% calculate path cost to all reachable nodes
dijkstra(Node, G) ->
    dijkstra(Node, G,
        dict:from_list([{Node,0}]),
        dict:fetch_keys(G)).

dijkstra(_, _, Dist, []) ->
    lists:sort(fun({_,X},{_,Y}) -> X < Y end, dict:to_list(Dist));
dijkstra(Node, G, Dist, Unvisit=[U|_]) ->
    {Closest,_} =
        dict:fold(fun(K,V,Acc) -> closest_min(K,V,Acc) end, {U,inf},
            dict:filter(fun(K,_) -> lists:member(K, Unvisit) end,
                dict:fetch(Node, G))),
    dijkstra(Closest, G,
        distances(Node, G,
            dict:fetch_keys(dict:fetch(Node, G)), Dist),
        lists:delete(Closest, Unvisit)).

distances(_, _, [], Dist) -> Dist;
distances(Node, G, [N|Neighbors], Dist) ->
    distances(Node, G, Neighbors,
        dict:store(N,
            min(fetch_or(N, Dist, inf),
                addinf(fetch_or(Node, Dist, inf),
                    dict:fetch(N, dict:fetch(Node, G)))), Dist)).

% dict:filter callback: choose {K,V} where min(V); respect 'inf'inity
closest_min(_,inf,Acc)              -> Acc;
closest_min(K,V,{_,inf})            -> {K,V};
closest_min(K,V,{_,VA}) when V < VA -> {K,V};
closest_min(_,_,Acc)                -> Acc.

fetch_or(Key, Dict, Rcf) ->
    try dict:fetch(Key, Dict)
    catch _:_ -> Rcf
    end.

addinf(inf, _) -> inf;
addinf(_, inf) -> inf;
addinf(X, Y)   -> X + Y.

test() ->
    {tests_results, Res} = tests(),
    Failed = lists:filter(fun({_In,Out,Exp}) -> Out =/= Exp end, Res),
    case Failed of
        [] -> ok;
        _ -> {error, Failed}
    end.

tests() ->

    Node =
        [
            {a, []}
        ],
    NodeTest =
        {
            [a, Node],
            [{a,0}]
        },

    Loop =
        [
            {a, [{a,1}]}
        ],
    LoopTest =
        {
            [a, Loop],
            [{a,0}]
        },

    % Given multiple paths from 'a' to 'd' choose the cheapest one
    Diamond =
        [
            {a, [{b,2},{c,1}]},
            {b, [{d,1}]},
            {c, [{d,1}]},
            {d, []}
        ],
    DiamondTest =
        {
            [a, Diamond],
            [{a,0},{c,1},{d,2},{b,2}]
        },

    % Ensure that the order of a's paths does not affect its outcome
    Diamond2 =
        [
            {a, [{c,1},{b,2}]},
            {b, [{d,1}]},
            {c, [{d,1}]},
            {d, []}
        ],
    Diamond2Test =
        {
            [a, Diamond2],
            [{a,0},{c,1},{d,2},{b,2}]
        },

    RealWorld =
        [
            {win1,      [{cloud,50},{win2,10},{usbdrive,20},{dropbox,20}]},
            {win2,      [{win1,10},{usbdrive,9}]},
            {cloud,     [{sss,1},{dropbox,10}]},
            {web,       [{cloud,50}]},
            {sss,       []},
            {dropbox,   []},
            {usbdrive,  []}
        ],

    RealWorldWin1Test =
        {
            [win1, RealWorld],
            [{win1,0},
             {win2,10},
             {usbdrive,19},
             {dropbox,20},
             {cloud,50},
             {sss,51}]
        },

    RealWorldWin2Test =
        {
            [win2, RealWorld],
            [{win2,0},
             {usbdrive,9},
             {win1,10},
             {dropbox,30},
             {cloud,60},
             {sss,61}]
        },

    RyanFlynnWebTest =
        {
            [web, RealWorld],
            [{web,0},
             {cloud,50},
             {sss,51},
             {dropbox,60},
             {usbdrive,inf},
             {win2,inf},
             {win1,inf}]
        },

    Tests =
        [
            NodeTest,
            LoopTest,
            DiamondTest,
            Diamond2Test,
            RealWorldWin1Test,
            RealWorldWin2Test,
            RyanFlynnWebTest
        ],

    {tests_results,
        [ {[N,G],
             dijkstra(N,
                dict:from_list([{K,dict:from_list(V)} || {K,V} <- G])),
             Exp}
            || {[N,G],Exp} <- Tests]}.

