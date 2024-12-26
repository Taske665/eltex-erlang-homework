-module(tcp_server_example).
-export([start/0]).

start() ->
    case gen_tcp:listen(1234, [binary, {active, false}, {reuseaddr, true}]) of
        {ok, LSocket} -> 
            spawn(fun() -> accept_loop(LSocket) end);
        {error, Reason} ->
            io:format("Ошибка: ~p ~n", [Reason])
    end.

accept_loop(LSocket) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    spawn(fun() -> handle_client(Socket) end),
    accept_loop(LSocket).

handle_client(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            io:format("Получено: ~p ~n", [Data]),
            gen_tcp:send(Socket, "Hello from server!");
        {error, closed} ->
            io:format("Клиент закрыл соединение ~n")
        end.