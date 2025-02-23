-module(mymess_user_storage).
-export([add_user/2, authenticate/2, find_user/1, init/0, delete_user/1]).

-record(user, {username, password}).

%% Инициализация Mnesia и создание таблицы
init() ->
    case code:priv_dir(mymess) of
        {error, bad_name} -> 
            io:format("error, \"Application mymess not found\"~n", []),
            {error, "Application mymess not found"};
        PrivDir ->
            MnesiaDir = filename:join(PrivDir, "mnesia"),
            filelib:ensure_path(MnesiaDir),
            io:format("~ts~n", [unicode:characters_to_list(unicode:characters_to_binary(MnesiaDir), utf8)]),
            application:set_env(mnesia, dir, MnesiaDir),
            mnesia:create_schema([node()]),
            mnesia:start(),
            case mnesia:create_table(user, [{attributes, record_info(fields, user)}, {type, set}, {disc_copies, [node()]}]) of
                {atomic, ok} -> 
                    io:format("{mymess_user_storage, init/0} Mnesia table user created~n", []),
                    ok;
                {aborted, {already_exists, user}} -> 
                    io:format("{mymess_user_storage, init/0} Mnesia table user already exit~n", []),
                    ok;
                {aborted, Reason} -> 
                    io:format("{mymess_user_storage, init/0} Mnesia table user error: ~p~n", [Reason]),
                    {error, Reason}
            end
    end.

%% Добавление пользователя
add_user(Username, Password) ->
    io:format("{mymess_user_storage, add_user/2} Username format in mnesia: ~p~n", [Username]),
    F = fun() -> mnesia:write(#user{username = Username, password = Password}) end,
    case mnesia:transaction(F) of
        {atomic, ok} ->
            true;
        {aborted, _} -> false
    end.

%% Удаление пользователя
delete_user(Username) ->
    F = fun() -> mnesia:delete({user, Username}) end,
    case mnesia:transaction(F) of
        {atomic, ok} ->
            true;
        {aborted, _} -> false
    end.

%% Аутентификация пользователя
authenticate(Username, Password) ->
    F = fun() -> 
        mnesia:read(user, Username) end,
    case mnesia:transaction(F) of
        {atomic, [{user, Username, Password}]} -> true;
        _ -> false
    end.

%% Поиск пользователя
find_user(Username) ->
    F = fun() ->
        mnesia:read(user, Username)
        end,
    case Result = mnesia:transaction(F) of
        {atomic, []} -> 
            io:format("{mymess_user_storage, find_user/1} Mnesia find_user result is: ~p~n", [Result]),
            not_found;
        _ -> 
            io:format("{mymess_user_storage, find_user/1} Mnesia find_user result is: ~p~n", [Result]),
            ok
    end.