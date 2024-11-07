-module(time).
-export([fac/1, fact/1, run1/1, run2/1, run3/1, run4/1, run5/1]).

run1(N) ->
    io:format("Выполнениe функции ~p ~n", ["fac()"]),
    fun_time(fac(N)),
    io:format("Выполнениe функции ~p ~n", ["fact()"]),
    fun_time(fact(N)).

run3(N) ->
    io:format("Выполнениe функции ~p ~n", ["fac()"]),
    fun_time(fac(N)).

run4(N) ->
    io:format("Выполнениe функции ~p ~n", ["fact()"]),
    fun_time(fact(N)).

run5(N) ->
    run3(N),
    run4(N).

run2(N) ->
    io:format("Выполнениe функции ~p ~n", ["fac()"]),
    fun_time_erl(fac(N)),
    io:format("Выполнениe функции ~p ~n", ["fact()"]),
    fun_time_erl(fact(N)).

fun_time(F) ->
    {Time, _} = timer:tc(fun() -> F end),
    io:format("Время выполнения ~p микросекунд ~n", [Time]).

fun_time_erl(F) ->
    T1 = erlang:monotonic_time(),
    Result = F,
    T2 = erlang:monotonic_time(),
    DT = erlang:convert_time_unit(T2 - T1, native, microsecond),
    io:format("Время выполения: ~p микросекунд ~n", [DT]),
    Result.

-spec fac(integer()) -> integer().
fac(N) ->
    case N of
        0 -> 1;
        1 -> 1;
        _ -> Acc = N*fac(N-1),
            Acc
    end.

-spec fact(integer()) -> fun().
fact(N) -> fact(N, 1).

-spec fact(integer(), integer()) -> integer().
fact(N, Acc) ->
    case N of
        1 -> Acc;
        _ -> fact(N-1, Acc*N)
    end.