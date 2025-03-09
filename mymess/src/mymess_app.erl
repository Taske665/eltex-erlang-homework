-module(mymess_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("Starting mymess application...~n"),
    %%Запуск супервизора приложения
    mymess_sup:start_link(),
    %%Проверка запуска библиотек
    application:ensure_all_started(cowboy),
    application:ensure_all_started(sasl),
    %%Запуск БД mnesia зарегестрированных пользователей
    mymess_user_storage:init(),
    %%Запуск БД mnesia существующих групповых чатов
    mymess_rooms_storage:init(),
    %%Запуск БД ETS пользователей онлайн
    mymess_online_user_storage:init(),
    %%Запуск БД mnesia для сохранения сообщений оффлайн пользователям
    mymess_send_later:init(),
    %%Запуск модуля маршрутизации сообщений
    mymess_msg_server:start(),
    %%Настройка маршрутов cowboy
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/reg", mymess_registration_handler, []},
            {"/auth", mymess_auth_handler, []},
            {"/ws", mymess_ws_handler, []}
        ]}
    ]),
    io:format("Dispatch table created. Starting Cowboy server...~n"),
    %%Запуск cowboy веб-сервера
    {ok, _} = cowboy:start_clear(http, [{port, 1234}], #{
        env => #{dispatch => Dispatch}
    }),
    io:format("Cowboy server started on port 1234.~n"),
    {ok, self()}.

stop(_State) ->
    ok.