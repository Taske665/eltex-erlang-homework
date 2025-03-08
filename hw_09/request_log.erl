-module(request_log).
-export([start/0, log_request/4]).

-record(request_log, {id, ip, method, content_type, response_code}).

start() ->
    mnesia:start(),
    mnesia:create_table(request_log, [{attributes, record_info(fields, request_log)}]).

log_request(IP, Method, ContentType, ResponseCode) ->
    case mnesia:transaction(fun() ->
        mnesia:lock(request_log, write),
        Count = mnesia:table_info(request_log, size),
        mnesia:write(#request_log{id = Count + 1, ip = IP, method = Method, content_type = ContentType, response_code = ResponseCode})
        end) of
            {aborted, Reason} -> io:fwrite("Ошибка транзакции ~p~n", [Reason]);
            {atomic, _} -> ok
    end.