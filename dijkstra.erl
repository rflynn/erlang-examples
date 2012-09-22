
-module(dijkstra).
-export(
    [
        dijkstra/2,
        test/0, tests/0
    ]).

% given a starting point and a weighted graph calculate path cost to all nodes
dijkstra(Node, G) ->
    dijkstra(Node, G,
        dict:from_list([{Node,0}]),
        sets:from_list(dict:fetch_keys(G)),
        dict:fetch_keys(G)).

dijkstra(_Node, _G, Dist, _UnvisitedSet, []) ->
    lists:sort(
        fun({_,X},{_,Y}) -> X < Y end,
        dict:to_list(Dist));
dijkstra(Node, G, Dist, UnvisitedSet, Unvisit=[U|_]) ->
    Closest = closest(dict:fetch(Node, G), UnvisitedSet, U),
    dijkstra(Closest, G,
        distances(Node, G,
            dict:fetch_keys(fetch_or(Node, G, dict:new())), Dist),
        sets:del_element(Closest, UnvisitedSet),
        lists:delete(Closest, Unvisit)).

distances(_Node, _G, [], Dist) -> Dist;
distances(Node, G, [N|Neighbors], Dist) ->
    distances(Node, G, Neighbors,
        dict:store(N,
            min(fetch_or(N, Dist, inf),
                addinf(fetch_or(Node, Dist, inf),
                    dict:fetch(N, dict:fetch(Node, G)))),
            Dist)).

closest(SubG, UnvisSet, Else) ->
    SubGUnvis = dict:filter(fun(K,_) -> sets:is_element(K, UnvisSet) end, SubG),
    {Closest,_Dist} =
        dict:fold(fun(K,V,Acc) -> closest_min(K,V,Acc) end,
            {Else,inf}, SubGUnvis),
    Closest.

% dict:filter callback: choose {K,V} where min(V); respect 'inf'inity
closest_min(_,inf,Acc)              -> Acc;
closest_min(K,V,{_,inf})            -> {K,V};
closest_min(K,V,{_,VA}) when V < VA -> {K,V};
closest_min(_,_,Acc)                -> Acc.

fetch_or(Key, Dict, Default) ->
    try dict:fetch(Key, Dict)
    catch _:_ -> Default
    end.

addinf(inf, _) -> inf;
addinf(_, inf) -> inf;
addinf(X, Y)   -> X + Y.

test() ->
    {tests_results, Res} = tests(),
    Failed = lists:filter(fun({_In,Out,Exp}) -> Out =/= Exp end, Res),
    case Failed of
        [] -> ok;
        _ -> Failed
    end.

tests() ->
    Tests =
        [
            {
                [win1,
                    [
                        {win1,      [{cloud,500},{win2,10},{usbdrive,20}]},
                        {win2,      [{win1,10}]},
                        {cloud,     [{hss,1},{dropbox,10}]},
                        {web,       [{cloud,500}]},
                        {hss,       []},
                        {dropbox,   []},
                        {usbdrive,  []}
                    ]
                ],
                []
            },
            {
                [a,
                    [
                        {a, [{b,2},{c,1}]},
                        {b, [{d,1}]},
                        {c, [{d,1}]},
                        {d, []}
                    ]
                ],
                [{a,0},{c,1},{d,2},{b,2}]
            }
        ],

    {tests_results,
        [
            {[N,G],
             dijkstra(N,
                dict:from_list([{K,dict:from_list(V)} || {K,V} <- G])),
             Exp}
            || {[N,G],Exp} <- Tests]}.

