-module(mymess_send_later).
-export([init/0, save_msg/2, send_msg/1]).

-record(send_later_msg, {username, msg_list}).

%% Инициализация Mnesia и создание таблицы для хранения сообщений для оффлайн пользователей
init() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
        case mnesia:create_table(send_later_msg, [{attributes, record_info(fields, send_later_msg)}, {type, set}, {disc_copies, [node()]}]) of
            {atomic, ok} -> 
                io:format("{mymess_send_later, init/0} Mnesia table send_later_msg created~n", []),
                ok;
            {aborted, {already_exists, send_later_msg}} -> 
                io:format("{mymess_send_later, init/0} Mnesia table send_later_msg already exit~n", []),
                ok;
            {aborted, Reason} -> 
                io:format("{mymess_send_later, init/0} Mnesia table send_later_msg error: ~p~n", [Reason]),
                {error, Reason}
        end.

%%Сохранение сообщения для офлайн пользователя
save_msg(Username, Message) ->
    F = fun() ->
        case mnesia:read(send_later_msg, Username) of
            [] ->
                mnesia:write(#send_later_msg{username = Username, msg_list = [Message]});
            [{send_later_msg, Username, Msg_list}] ->
                mnesia:write(#send_later_msg{username = Username, msg_list = Msg_list ++ [Message]});
            _ ->
            io:format("{mymess_send_later, save_msg/3} Mnesia error in save_msg/3~n", []),
            mnesia:abort(invalid_record)
        end
        end,
    mnesia:transaction(F).

%%Проверка наличия сохранненных сообщений для пользователя
send_msg(Username) ->
    F = fun() -> mnesia:read(send_later_msg, Username) end,
    case mnesia:transaction(F) of
        {atomic, []} -> not_found;
        {atomic, [{send_later_msg, Username, Msg_list}]} ->
            %io:format("{mymess_send_later, send_msg/1} Msg_list is: ~p~n", [Msg_list]),
            send_to_msg_server(Username, Msg_list);
            _ ->
                io:format("{mymess_send_later, send_msg/1} Mnesia error in send_msg/1~n", [])
    end.

%%Поочередная отправка сохранненых сообщений пользователю и последующее удаление их из БД
send_to_msg_server(Username, Msg_list) ->
    case Msg_list of
        [Message] ->
            %io:format("{mymess_send_later, send_to_msg_server/2}: {send, ~p, ~p}~n", [Username, Message]),
            msg_server ! {send, Username, Message},
            F = fun() -> 
                mnesia:delete({send_later_msg, Username}) 
                end,
                mnesia:transaction(F);
        [Message | Rest_messages] ->
            %io:format("{mymess_send_later, send_to_msg_server/2}: {send, ~p, ~p}~n", [Username, Message]),
            msg_server ! {send, Username, Message},
            send_to_msg_server(Username, Rest_messages);
        _ -> ok
    end.