-module(sort_time).

-export([run/0, sort/1, list_generate/2, random/0, random_symbol/0]).

fun_time(F) ->
    {Time, Result} = timer:tc(fun() -> F end),
    io:format("Время выполнения ~p микросекунд ~n", [Time]),
    Result.

-spec sort(list()) -> list().

sort(List) ->
    case List of
        [] -> [];
        [_] -> List;
        [Head | Tail] ->
            sort([X || X <- Tail, X < Head]) ++ [Head] ++ sort([X || X <- Tail, X >= Head])
    end.

-spec list_generate(list(), integer()) -> list().
list_generate(List, Counter) ->
    case Counter of
        20000 -> List;
        _ -> list_generate([random() | List], Counter + 1)
    end.

-spec random() -> list().
random() ->
    [random_symbol() || _ <- lists:seq(1, 15)].

random_symbol() ->
    Char = rand:uniform(62) - 1,
    case Char of
        N when N < 10 -> 48 + N;
        N when N < 36 -> 55 + N;
        N -> 61 + N
    end.

run() ->
    List = list_generate([], 0),
    fun_time(lists:sort(List)),
    fun_time(sort(List)).

