-module(time).
-export([fun_time/1, fun_time_erl/1]).

fun_time(F) ->
    {Time, Result} = timer:tc(fun() -> F end),
    io:format("Время выполнения ~p микросекунд ~n", [Time]),
    Result.

fun_time_erl(F) ->
    T1 = erlang:monotonic_time(),
    Result = F,
    T2 = erlang:monotonic_time(),
    DT = erlang:convert_time_unit(T2 - T1, native, microsecond),
    io:format("Время выполения: ~p микросекунд ~n", [DT]),
    Result.