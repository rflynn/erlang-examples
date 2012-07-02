#!/usr/bin/env escript
%% -*- erlang -*-

% figure out which year's calendars are identical in structure
% so we can recycle them
% inspiration: http://garyc.me/calendars/

year_key(Year) ->
    FirstWeekday = calendar:day_of_the_week({Year,1,1}),
    IsLeapYear = calendar:is_leap_year(Year),
    {FirstWeekday, IsLeapYear}.

% fold [{K,V}] append Vs on matching K
append_val({K,V}, [{K,V2}|Rest]) -> [{K,[V|V2]}|Rest];
append_val({K,V}, Merged) -> [{K,[V]}|Merged].

main(_) ->
    AllYears = lists:sort([{year_key(Year),Year} || Year <- lists:seq(1900,2100)]),
    MergedByKey =
        lists:foldl(fun(X,Y) -> append_val(X,Y) end,
            [hd(AllYears)], tl(AllYears)),
    io:format("~p~n", [MergedByKey]).

