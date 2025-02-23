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
                    mymess_online_user_storage:register_user(Username, Pid),
                    spawn(fun() -> mymess_send_later:send_msg(Username) end),
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
    try
        Json = jsx:decode(Msg, [return_maps]),
        handle_json_message(Json, State)
    catch
        _:_ ->
            io:format("Invalid JSON received: ~p~n", [Msg]),
            {reply, {text, <<"{\"error\": \"Invalid JSON\"}">>}, State}
    end;

websocket_handle(_Data, State) ->
    {ok, State}.

%% Обработка JSON-сообщений

handle_json_message(#{<<"msg_type">> := MsgType, <<"to">> := To, <<"message">> := MsgText}, State) ->
    %% Клиент отправил сообщение для другого пользователя
    io:format("{mymess_ws_handler, handle_json_message/2} Message from ~p to ~p: ~p~n", [State, To, MsgText]),
    %% Передаем имя получателя и текст сообщения серверу
    From = maps:get(username, State, undefined),
%%    io:format("From: ~p / Is From map: ~p / Is From binary: ~p / Is From list: ~p~n", [From, is_map(From), is_binary(From), is_list(From)]),
    X = <<": ">>,
    Message = <<From/binary, X/binary, MsgText/binary>>,
    io:format("{mymess_ws_handler, handle_json_message/2} Message is: ~p~n", [Message]),
    msg_server ! {send, MsgType, To, Message},
    {ok, State};

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
            mymess_online_user_storage:remove_user(Username),
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