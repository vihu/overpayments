-module(overpayment).

-export([answer/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

answer(Input, Target) ->
    InputList = maps:to_list(Input),

    %% reverse sort by node value
    RevSorted = [Head | Rest] = lists:reverse(lists:keysort(2, InputList)),
    io:format("RevSorted: ~p~n", [RevSorted]),

    %% Calculate overage (delta)
    Delta = lists:sum(maps:values(Input)) - Target,
    io:format("Delta: ~p~n", [Delta]),

    %% straight up subtract delta from top guy
    {Name, Val} = Head,
    Ans0 = [{Name, Val - Delta} | Rest],
    io:format("Ans0: ~p~n", [Ans0]),

    [{FirstName, FirstVal}, {SecondName, SecondVal} | Rest] = Ans0,
    InitDeltaFwd = SecondVal - FirstVal,
    Ans = answer(Ans0,
                 [{FirstName, FirstVal},
                  {SecondName, SecondVal - InitDeltaFwd}],
                 InitDeltaFwd),

    maps:from_list(Ans).

answer(List, Acc, DF) when DF =< 0 ->
    %% Nothing to forward
    Acc;
answer(List, [{FirstName, FirstVal}, {SecondName, SecondVal} | Rest]=Acc , DF) ->

-ifdef(TEST).

example_test() ->
    Input1 = #{a => 20, b => 110, c => 110, d => 110, e => 30},
    ?assertEqual(#{a => 20, b => 50, c => 50, d => 50, e => 30}, answer(Input1, 200)),
    Input2 = #{a => 20, b => 90, c => 110, d => 30},
    ?assertEqual(#{a => 20, b => 75, c => 75, d => 30}, answer(Input2, 200)).

-endif.
