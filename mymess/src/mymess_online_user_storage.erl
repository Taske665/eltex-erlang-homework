-module(mymess_online_user_storage).
-export([init/0, register_user/2, find_user/1, remove_user/1]).

-define(USER_DB, online_user_db).

%% Инициализация ETS таблицы
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
%%            case mymess_send_later:send_msg(Username) of
%%                not_found ->
%%                    io:format("No saved messages for ~p~n", [Username]);
%%                ok ->
%%                    io:format("Saved messages send to ~p~n", [Username]);
%%                _ ->
%%                    ok
%%            end;
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

%% Поиск пользователя по Pid
%%find_by_pid(Pid) ->
%%    case ets:match(?USER_DB, {"$1", Pid}) of
%%        [] -> 
%%            io:format("User with PID ~p not found~n", [Pid]),
%%            not_found;
%%        [[Username]] -> 
%%            {ok, Username}
%%    end.

%% Удаление пользователя по Pid из ETS
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