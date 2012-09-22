#!/usr/bin/env escript
%% -*- erlang -*-

-author("Ryan Flynn parseerror@gmail.com www.parseerror.com github.com/rflynn").

% figure out which year's calendars are identical in structure so we can recycle them
% inspiration: http://garyc.me/calendars/
% NOTE: shortcoming: lunar cycle

main(_) ->
    AllYears = [
        {{calendar:day_of_the_week(Year,1,1),
          calendar:is_leap_year(Year)},Year}
            || Year <- lists:seq(1900,2100)],
    MergedByKey =
        lists:foldl(
            fun({K,V},O) -> orddict:append(K,V,O) end,
            orddict:new(), AllYears),
    io:format("~p~n", [MergedByKey]).

