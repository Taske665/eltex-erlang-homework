-module(tcp_client_example).
-export([start/0, send_request/1]).

start() ->
    A = "GET / HTTP/1.0\r\nAccept: text/html\r\nHost: localhost:1234\r\nUser-Agent: ApacheBench/2.3\r\n\r\n",
    S = "POST / HTTP/1.0\r\nAccept: application/json\r\nHost: localhost:1234\r\nUser-Agent: ApacheBench/2.3\r\n\r\n",
    D = "DELETE / HTTP/1.0\r\nAccept: text/html\r\nHost: localhost:1234\r\nUser-Agent: ApacheBench/2.3\r\n\r\n",
    F = "GET / HTTP/1.0\r\nAccept: wrong_type\r\nHost: localhost:1234\r\nUser-Agent: ApacheBench/2.3\r\n\r\n",
    G = "CHECK / HTTP/1.0\r\nAccept: text/html\r\nHost: localhost:1234\r\nUser-Agent: ApacheBench/2.3\r\n\r\n",
    H = "CHECK / HTTP/1.0\r\nHost: localhost\r\nAccept: text/html\r\nHost: localhost:1234\r\nUser-Agent: ApacheBench/2.3\r\n\r\n",
    send(A),
    send(S),
    send(D),
    send(F),
    send(G),
    send(H).
    
send(Request) ->
    case gen_tcp:connect("localhost", 1234, [binary, {active, false}]) of
        {ok, Socket} -> 
            gen_tcp:send(Socket, Request),
            case gen_tcp:recv(Socket, 0) of
                {ok, Data} ->
                    io:format("Получено:~n ~s ~n", [binary:bin_to_list(Data)]);
                {error, Reason} ->
                    io:format("Ошибка: ~p ~n", [Reason])
            end,
            gen_tcp:close(Socket);
        {error, Reason} ->
            io:format("Ошибка: ~p ~n", [Reason])
    end.

send_request(Request) ->
    case gen_tcp:connect("localhost", 1234, [binary, {active, false}]) of
        {ok, Socket} -> 
            gen_tcp:send(Socket, Request),
            case gen_tcp:recv(Socket, 0) of
                {ok, Data} ->
                    gen_tcp:close(Socket),
                    Data;
                {error, Reason} ->
                    gen_tcp:close(Socket),
                    Reason
            end;
        {error, Reason} ->
            io:format("Ошибка: ~p ~n", [Reason])
    end.
