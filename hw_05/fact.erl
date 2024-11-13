-module(fact).
-export([fac/1, fact/1]).

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