-module(mymess_ws_handler).
-behaviour(cowboy_websocket).

-export([init/2, websocket_init/3, websocket_handle/2, websocket_info/2, terminate/3]).

%% Парсим строку запроса на предмет токена
init(Req0, State) ->
        Qs = cowboy_req:parse_qs(Req0),
    TokenParam = proplists:get_value(<<"token">>, Qs),

    case TokenParam of
        undefined ->
            %% Без токена соединение закрывается
            Req1 = cowboy_req:reply(401, #{}, <<"Missing or invalid token">>, Req0),
            {shutdown, Req1, State};
        TokenValue ->
            %% Проверка токена
            case verify_token(binary_to_list(TokenValue)) of
                {ok, Username} ->
                    %% Если токен валиден, то устанавливается соединение по WS
                    io:format("{mymess_ws_handler, init/0} WebSocket connected for user: ~p~n", [Username]),
                    Pid = maps:get(pid, Req0, undefined),
                    mymess_online_user_storage:register_user(binary_to_list(Username), Pid),
                    spawn(fun() -> mymess_send_later:send_msg(binary_to_list(Username)) end),
                    {cowboy_websocket, Req0, #{username => Username}};
                {error, Reason} ->
                    %% Если токен не валиден, то соединение закрывается
                    Msg = io_lib:format("Invalid token: ~p", [Reason]),
                    Req1 = cowboy_req:reply(401, #{}, list_to_binary(Msg), Req0),
                    {shutdown, Req1, State}
            end
    end.

%% Инициализация WebSocket соединения
websocket_init(Req, _Opts, State) ->
    {cowboy_websocket, Req, State}.

%% Обработка сообщений от клиента
websocket_handle({text, Msg}, State) ->
    try jsx:decode(Msg, [return_maps]) of
        Json ->
            io:format("JSON is: ~p~n", [Json]),
            handle_json_message(Json, State)
    catch 
        _:_ ->
            io:format("Invalid JSON~n", []),
            {reply, {text, <<"{\"error\": \"Invalid JSON\"}">>}, State}
    end;

websocket_handle(_Data, State) ->
    {ok, State}.

%% Обработка JSON-сообщений
%% Клиент отправил сообщение для другого пользователя

handle_json_message(#{<<"msg_type">> := <<"user">>, <<"to">> := To, <<"message">> := MsgText}, State) ->
    %% Передаем имя получателя и текст сообщения серверу
    From = maps:get(username, State, undefined),
    Message = <<From/binary, <<": ">>/binary, MsgText/binary>>,
    io:format("{mymess_ws_handler, handle_json_message/2} Message is: ~p~n", [Message]),
    send2server(binary_to_list(To), Message, State),
    {ok, State};

%% Клиент отправил запрос создания группового чата
handle_json_message(#{<<"msg_type">> := <<"create_room">>, <<"roomname">> := Roomname}, State) ->
    Creator = maps:get(username, State, undefined),
    case mymess_rooms_storage:add_room(binary_to_list(Roomname), binary_to_list(Creator)) of
        {aborted, already_exists} ->
            {reply, {text, <<"Roomname already taken">>}, State};
        {atomic, _} -> 
            {reply, {text, <<"Room successfully created">>}, State}
    end;

%% Клиент добавляет собеседника в групповой чат
handle_json_message(#{<<"msg_type">> := <<"add_user2room">>, <<"roomname">> := Roomname, <<"user">> := Username}, State) ->
    case mymess_rooms_storage:add_user2room(binary_to_list(Roomname), binary_to_list(Username)) of
        not_found -> {reply, {text, <<"User is not found">>}, State};
        {aborted, already_exists} ->
            {reply, {text, <<"User is already in room">>}, State};
        {aborted, not_found} ->
            {reply, {text, <<"Room doesn't exist">>}, State};
        {atomic, _} -> 
            {reply, {text, <<"User successfully added to room">>}, State}
    end;

%% Клиент отправил сообщение групповой чат
handle_json_message(#{<<"msg_type">> := <<"room">>, <<"to">> := Roomname, <<"message">> := MsgText}, State) ->
    From = maps:get(username, State, undefined),
    Message = <<From/binary, <<" in ">>/binary, Roomname/binary, <<": ">>/binary, MsgText/binary>>,
    io:format("{mymess_ws_handler, handle_json_message/2} Message is: ~p~n", [Message]),
    Userslist = mymess_rooms_storage:get_userslist(binary_to_list(Roomname)),
    send2room(binary_to_list(From), Userslist, Message, State);

handle_json_message(_Json, State) ->
    io:format("{mymess_ws_handler, handle_json_message/2} Unknown JSON format received~n"),
    {reply, {text, <<"{\"error\": \"Unknown message format\"}">>}, State}.

%% Обработка сообщений из других процессов (например, отправка данных клиенту)
websocket_info({send, Msg}, State) ->
    io:format("{mymess_ws_handler, websocket_info/2} Handler has got message: ~p, PID_info: ~p~n", [Msg, self()]),
    {reply, {text, Msg}, State};

websocket_info(_Info, State) ->
    io:format("{mymess_ws_handler, websocket_info/2} Unknown message format for ws_handler"),
    {ok, State}.

%% Завершение соединения
terminate(_Reason, _Req, State) ->
    case is_map(State) of
        true -> 
            Username = maps:get(username, State),
            io:format("{mymess_ws_handler, terminate/3} WebSocket connection closed for user ~p~n", [Username]),
            mymess_online_user_storage:remove_user(binary_to_list(Username)),
            ok;
        _ -> ok
    end.

%% Проверка валидности токена
verify_token(Token) ->
    try
        %% Декодирование токена
        Decoded = base64:decode(Token),
        %% Извление Username и Password
        [Username, Password] = binary:split(Decoded, <<":">>),
        %% Проверка аутентификации
        case mymess_user_storage:authenticate(binary_to_list(Username), binary_to_list(Password)) of
            true ->
                {ok, Username};
            false ->
                {error, invalid_credentials}
        end
    catch
        _:_ ->
            {error, invalid_token}
    end.

%% Отправка сообщения на маршрутизацию серверу сообщений
send2server(To, Message, State) ->
    msg_server ! {send, To, Message},
    {ok, State}.

%%Отправка сообщения для всех участников группового чата на маршрутизацию
send2room(From, Userslist, Message, State) ->
    case Userslist of
        [] -> {ok, State};
        [User] -> 
            case User =:= From of
                true -> 
                    {ok, State};
                false -> 
                    send2server(User, Message, State)
            end;
        [User | Rest_users] ->
            case User =:= From of
                true -> 
                    send2room(From, Rest_users, Message, State);
                false -> 
                    send2server(User, Message, State),
                    send2room(From, Rest_users, Message, State)
            end
    end.