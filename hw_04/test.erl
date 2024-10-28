-module(test).
-export([run/0]).

run() ->
    case q:test()  of
        ok -> init:stop(0);
        _ -> init:stop(1)
    end,
    case stack:test()  of
        ok -> init:stop(0);
        _ -> init:stop(1)
    end.