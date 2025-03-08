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
        {send, To, Message} ->
            case mymess_online_user_storage:find_user(To) of
                {ok, Pid} ->
                    io:format("{mymess_msg_server, loop/0} Sending message to ~p: ~p~n", [To, Message]),
                    Pid ! {send, Message};
                not_found ->
                    case mymess_user_storage:find_user(To) of
                        ok -> 
                            io:format("{mymess_msg_server, loop/0} User ~p is offline~n", [To]),
                            mymess_send_later:save_msg(To, Message);
                        Some_other_reply -> io:format("{mymess_msg_server, loop/0} User ~p not found. Some other reply is: ~p~n", [To, Some_other_reply])
                    end
            end,
            loop();
        _ ->
            loop()
    end.