-module(mymess_rooms_storage).
-export([add_room/2, add_user2room/2, get_userslist/1, init/0, clear/0]).

-record(room, {roomname, userslist}).

%% Инициализация Mnesia и создание таблицы
init() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
        case mnesia:create_table(room, [{attributes, record_info(fields, room)}, {type, set}, {disc_copies, [node()]}]) of
            {atomic, ok} -> 
                io:format("{mymess_rooms_storage, init/0} Mnesia table room created~n", []),
                ok;
            {aborted, {already_exists, room}} -> 
                io:format("{mymess_rooms_storage, init/0} Mnesia table room already exit~n", []),
                ok;
            {aborted, Reason} -> 
                io:format("{mymess_rooms_storage, init/0} Mnesia table room error: ~p~n", [Reason]),
                {error, Reason}
        end.

add_room(Roomname, Creator) ->
    F = fun() ->
        case mnesia:read(room, Roomname) of
            [{room, Roomname, _Userslist}] ->
                io:format("Roomname ~p is already taken.~n", [Roomname]),
                mnesia:abort(already_exists);
            [] ->
                io:format("Room ~p created.~n", [Roomname]),
                mnesia:write(#room{roomname = Roomname, userslist = [Creator]})
        end
    end,
    mnesia:transaction(F).

add_user2room(Roomname, Username) ->
    case mymess_user_storage:find_user(Username) of
        not_found -> not_found;
        ok ->
        F = fun() ->
            case mnesia:read(room, Roomname) of
                [] ->
                    io:format("Room ~p doesn't exist.~n", [Roomname]),
                    mnesia:abort(not_found);
                [{room, Roomname, Userslist}] ->
                    case user_check(Username, Userslist) of
                        true ->
                            io:format("User ~p is already in room.~n", [Username]),
                            mnesia:abort(already_exists);
                        false ->
                            io:format("User ~p added to room ~p. ~n", [Username, Roomname]),
                            mnesia:write(#room{roomname = Roomname, userslist = [Username | Userslist]})
                    end
            end
        end,
        mnesia:transaction(F)
    end.

user_check(Username, Userslist) ->
    case Userslist of
        [] -> false;
        [User] -> 
            case User =:= Username of
                true -> true;
                false -> false
            end;
        [User | Rest_users] ->
            case User =:= Username of
                true -> true;
                false -> user_check(Username, Rest_users)
            end
    end.


get_userslist(Roomname) ->
    F = fun() -> mnesia:read(room, Roomname) end,
    case mnesia:transaction(F) of
            {atomic, []} ->
                not_found;
            {atomic, [{room, Roomname, Userslist}]} ->
                io:format("Userslist for room ~p: ~p~n", [Roomname, Userslist]),
                Userslist;
            {aborted, Reason} ->
                {error, Reason}
    end.

clear() ->
    case mnesia:clear_table(room) of
        {atomic, ok} ->
            io:format("All rooms have been deleted.~n"),
            ok;
        {aborted, Reason} ->
            io:format("Error clearing rooms: ~p~n", [Reason]),
            {error, Reason}
    end.
