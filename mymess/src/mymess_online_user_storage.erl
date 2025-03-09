-module(mymess_online_user_storage).
-export([init/0, register_user/2, find_user/1, remove_user/1]).

-define(USER_DB, online_user_db).

%% Инициализация ETS таблицы для регистрации пользователей онлайн
init() ->
    case ets:info(?USER_DB) of
        undefined -> 
            ets:new(?USER_DB, [named_table, set, public]),
            io:format("{mymess_online_user_storage, init/0} Online user storage successfully started~n", []);
        _ ->
            io:format("{mymess_online_user_storage, init/0} Online user storage already started~n", []),
            ok
    end.

%% Регистрация пользователя в ETS
register_user(Username, Pid) ->
    case ets:insert(?USER_DB, {Username, Pid}) of
        true -> 
            io:format("{mymess_online_user_storage, register_user/1} User ~p successfully added to online user storage with PID ~p~n", [Username, Pid]);
        false -> 
            io:format("{mymess_online_user_storage, register_user/0} ETS error for user ~p~n", [Username])
    end.

%% Поиск пользователя в ETS
find_user(Username) ->
    case ets:lookup(?USER_DB, Username) of
        [{_, Pid}] -> 
            io:format("{mymess_online_user_storage, find_user/1} User ~p has PID ~p~n", [Username, Pid]),
            {ok, Pid};
        [] ->
            io:format("{mymess_online_user_storage, find_user/1} User ~p not found ~n", [Username]),
            not_found
    end.

%% Удаление пользователя из ETS
remove_user(Username) ->
    case ets:lookup(?USER_DB, Username) of
        [] -> 
            io:format("{mymess_online_user_storage, remove_user/1} Removing user error", []),
            not_found;
        [{Username, Pid}] -> 
            ets:delete(?USER_DB, Username),
            io:format("{mymess_online_user_storage, remove_user/1} User ~p with PID ~p removed from online users~n", [Username, Pid]),
            ok
    end.