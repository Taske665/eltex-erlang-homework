-module(test).
-export([run/0]).

run() ->
    case tcp_server_test:test()  of
        ok -> init:stop(0);
        _ -> init:stop(1)
    end.