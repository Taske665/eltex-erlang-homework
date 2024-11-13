-module(test).
-export([run/0]).

run() ->
    case sort:test()  of
        ok -> init:stop(0);
        _ -> init:stop(1)
    end.