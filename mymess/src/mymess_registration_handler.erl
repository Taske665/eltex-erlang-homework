-module(mymess_registration_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, _State) ->
    %%Парсим запрос клиента
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    %%Decode JSON body
    Params = jsx:decode(Body, [return_maps]),
    Username = maps:get(<<"username">>, Params, undefined),
    Password = maps:get(<<"password">>, Params, undefined),
    %%Проверка наличия пары имя пользователя/пароль в запросе
    case {Username, Password} of
        {undefined, _} -> respond(Req1, 400, <<"Missing username">>);
        {_, undefined} -> respond(Req1, 400, <<"Missing password">>);
        _ ->
            %%Проверка свободно ли указанное имя пользователя
            case mymess_user_storage:find_user(binary_to_list(Username)) of
                not_found ->
                    %%Создание записи в хранилище пользователей
                    case mymess_user_storage:add_user(binary_to_list(Username), binary_to_list(Password)) of
                        true -> respond(Req1, 201, <<"User registered">>);
                        false -> respond(Req1, 500, <<"Registration failed">>)
                    end;
                _ -> respond(Req1, 400, <<"Username is already taken">>)
            end
    end.

respond(Req, Code, Message) ->
    cowboy_req:reply(Code, #{}, Message, Req).