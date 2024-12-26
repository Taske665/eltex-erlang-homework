-module(tcp_server).
-export([start/0]).

start() ->
    case gen_tcp:listen(1234, [binary, {active, false}, {reuseaddr, true}]) of
        {ok, LSocket} ->
            request_log:start(),
            spawn(fun() -> accept_loop(LSocket) end);
        {error, Reason} ->
            io:format("Ошибка при получении данных: ~p ~n", [Reason])
    end.

accept_loop(LSocket) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    spawn(fun() -> handle_client(Socket) end),
    accept_loop(LSocket).

handle_client(Socket) ->
    {ok, {IP, _Port}} = inet:peername(Socket),
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            io:format("Получено:~n ~s ~n", [Data]),
            Respond = handle_http_req(Data, IP),
            gen_tcp:send(Socket, Respond),
            gen_tcp:close(Socket);
        {error, Reason} ->
            io:fwrite("Ошибка:  ~p ~n", [Reason])
        end.

handle_http_req(Data, IP) ->
    StringList = binary:split(Data, <<"\r\n">>, [global]),
    [MethodLine | _] = StringList,
    [Method | _] = binary:split(MethodLine, <<" ">>),
    ContentType = content_type(StringList),
    case ContentType of
        <<"text/html">> ->
            RespCode = handle_method(Method),
            RespType = <<"Content-Type: text/html\r\n">>,
            RespBody = <<"<div> <span>method:</span> <span>", Method/binary, "</span> </div>\r\n\r\n">>;
        <<"application/json">> ->
            RespCode = handle_method(Method),
            RespType = <<"Content-Type: application/json\r\n">>,
            RespBody = <<"{'method':'", Method/binary, "'}\r\n\r\n">>;
        _ ->
            RespCode = <<"400 Bad Request">>,
            RespType = <<"Content-Type: wrong content-type\r\n">>,
            RespBody = <<"ERROR\r\n\r\n">>
    end,
    ContentLength = term_to_binary(byte_size(RespBody)),
    request_log:log_request(IP, 
                            binary:bin_to_list(Method), 
                            binary:bin_to_list(ContentType), 
                            binary:bin_to_list(RespCode)),
    <<"HTTP/1.1 ", RespCode/binary, "\r\n", RespType/binary, "Content-Length: ", ContentLength/binary, "\r\n\r\n", RespBody/binary>>.

handle_method(Method) ->
    case Method of
        <<"GET">> ->
            ResponseCode = <<"200 OK">>;
        <<"POST">> ->
            ResponseCode = <<"201 Created">>;
        <<"DELETE">> ->
            ResponseCode = <<"204 No content">>;
        _ ->
            ResponseCode = <<"501 Not Implemented">>
        end,
    ResponseCode.

content_type(StringList) ->
        case StringList of
            [] -> <<"No content type found">>;
            [SingleElement] -> 
                [Accept, ContentType | _] = binary:split(SingleElement, <<" ">>, [global]),
                case Accept of
                    <<"Accept:">> -> ContentType;
                    _ -> <<"No content type found">>
                end;
            [Head | Tail]->
                [Accept, ContentType | _] = binary:split(Head, <<" ">>, [global]),
                case Accept of
                    <<"Accept:">> -> ContentType;
                    _ -> content_type(Tail)
                end
        end.