-module(mymess_auth_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, _State) ->
    %%Парсим запрос клиента
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    %%Decode JSON body
    Params = jsx:decode(Body, [return_maps]),
    Username = maps:get(<<"username">>, Params, undefined),
    Password = maps:get(<<"password">>, Params, undefined),
    %%Проверка на наличие в запросе имени пользователя и пароля
    case {Username, Password} of
        {undefined, _} -> 
            respond(Req1, 400, <<"Missing username">>);
        {_, undefined} -> 
            respond(Req1, 400, <<"Missing password">>);
        _ ->
            %%Проверка корректности пары имя пользователя/пароль
            case mymess_user_storage:authenticate(binary_to_list(Username), binary_to_list(Password)) of
    		    true ->
                    %%Считаваем ключ из переменных окружения
                    {ok, Secret} = application:get_env(mymess, jwt_secret),
                    %io:format("Secret is: ~p~n", [Secret]),
                    %% JSON Web Key
                    JWK = #{
                        <<"kty">> => <<"oct">>,
                        <<"k">> => jose_base64url:encode(list_to_binary(Secret))
                        },
                    %% JSON Web Token
                    JWT = #{
                        <<"username">> => Username,
                        <<"password">> => Password,
                        <<"iss">> => <<"mymess_auth">>
                    },
                    %% JSON Web Signature
                    JWS = #{
                        <<"alg">> => <<"HS256">>
                        },
                    Signed = jose_jwt:sign(JWK, JWS, JWT),
                    %io:format("Signed is: ~p~n", [Signed]),
                    CompactSigned = jose_jws:compact(Signed),
                    %io:format("Signed is: ~p~n", [CompactSigned]),
                    %%Вычленяем токен из кортежа, возвраженного jose
                    {_, Token} = CompactSigned,
                    %io:format("Token is: ~p~n", [Token]),
                    %io:format("Verify is: ~p~n", [jose_jwt:verify(JWK, CompactSigned)]),
                    %%Отправка ответа с токеном клиенту
                    respond(Req1, 200, jsx:encode(#{token => Token}));
		        false ->
		            respond(Req1, 401, <<"Wrong username/password">>)
	        end		
    end.

respond(Req, Code, Message) ->
    cowboy_req:reply(Code, #{}, Message, Req).


