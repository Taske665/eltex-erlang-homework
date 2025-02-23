-module(mymess_msg_server).
-export([start/0]).

%% Запуск сервера
start() ->
    %% Запускаем процесс сервера
    Pid = spawn(fun() -> loop() end),
    register(msg_server, Pid),
    io:format("{mymess_msg_server, start/0} Server started with PID ~p~n", [Pid]).

%% Основной цикл сервера сообщений
loop() ->
    receive
        {send, MsgType, To, Message} ->
%%            io:format("MsgType, To, Message format in msg server: ~p / ~p / ~p~n", [MsgType, To, Message]),
            case MsgType of
                _ ->
                    case mymess_online_user_storage:find_user(To) of
                        {ok, Pid} ->
                            io:format("{mymess_msg_server, loop/0} Sending message to ~p: ~p~n", [To, Message]),
%%                            io:format("Is Message binary: ~p / Is From binary: ~p / Is From list: ~p~n", [is_binary(Message), is_binary(From), is_list(From)]),
                            Pid ! {send, Message}; %Pid ! {send, jsx:encode(#{<<"from">> => From, <<"message">> => Message})};
                        not_found ->
                            case mymess_user_storage:find_user(binary_to_list(To)) of
                                ok -> 
                                    io:format("{mymess_msg_server, loop/0} User ~p is offline~n", [To]),
                                    mymess_send_later:save_msg(MsgType, To, Message);
                                Some_other_reply -> io:format("{mymess_msg_server, loop/0} User ~p not found. Some other reply is: ~p~n", [To, Some_other_reply])
                            end
                    end,
                    loop()
 %%               _ -> 
 %%                   io:format("Only user-to-user message is currently supported ~n"),
 %%                   loop()
            end;
        _ ->
            loop()
    end.