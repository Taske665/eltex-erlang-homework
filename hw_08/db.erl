-module(db).
-export([create_table/0, init/0]).

start() ->
    odbc:start(),
    {ok, Ref} = odbc:connect("DSN=taske-665", []),
    Ref.

stop(Ref) ->
    odbc:disconnect(Ref).

create_table() ->
    Ref = start(),
    odbc:sql_query(Ref,
                    "CREATE TABLE students (id INT PRIMARY KEY, 
                                            last_name VARCHAR(20), 
                                            first_name VARCHAR(20), 
                                            age INT, 
                                            email VARCHAR(30))"),
    stop(Ref).

init() ->
    io:fwrite("Выберите действие: ~n
                1. Добавить ~n
                2. Удалить ~n
                3. Редактировать ~n
                4. Найти ~n",
                []),
    Action = io:fread("> ", "~d"),
    case Action of
        {ok, [1]} -> ins();
        {ok, [2]} -> del();
        {ok, [3]} -> upd();
        {ok, [4]} -> find();
        _ -> 
            io:fwrite("Ошибка выбора действия ~n", []),
            init()
    end.

ins() ->
    Ref = start(),
    {ok, [Id]} = io:fread("Введите ID: ", "~d"),
    {ok, [Last_name]} = io:fread("Введите фамилию: ", "~s"),
    {ok, [First_name]} = io:fread("Введите имя: ", "~s"),
    {ok, [Age]} = io:fread("Введите возраст: ", "~d"),
    {ok, [Email]} = io:fread("Введите email: ", "~s"),
    Result = odbc:param_query(Ref, "INSERT INTO students (id, last_name, first_name, age, email) 
                            VALUES (?, ?, ?, ?, ?)", 
                            [{sql_integer, [Id]}, 
                            {{sql_varchar, 20}, [Last_name]}, 
                            {{sql_varchar, 20}, [First_name]}, 
                            {sql_integer, [Age]}, 
                            {{sql_varchar, 30}, [Email]}
                            ]),
    stop(Ref),
    case Result of
        {updated, _} ->
            io:fwrite("Запись успешно добавлена. ~n");
        Reason ->
            io:fwrite("Ошибка: ~p ~n", [Reason])
    end.

find() ->
    Ref = start(),
    {ok, [Last_name]} = io:fread("Введите фамилию: ", "~s"),
    Result = odbc:param_query(Ref, 
                            "SELECT * FROM students 
                            WHERE last_name = ?",
                            [{{sql_varchar, 20}, [Last_name]}
                            ]),
    stop(Ref),
    case Result of
        {_, _, []} ->
            io:fwrite("Не найдено. ~n", []);
        {selected, _, Row} ->
            io:fwrite("Найдено: ~p ~n", [Row]);
        Reason ->
            io:fwrite("Ошибка: ~p ~n", [Reason])
    end.

upd() ->
    Ref = start(),
    {ok, [Id]} = io:fread("Введите Id обновляемой записи: ", "~d"),
    {ok, [Email]} = io:fread("Введите новый email: ", "~s"),
    Result = odbc:param_query(Ref,
                            "UPDATE students SET email =? WHERE id = ?",
                            [{{sql_varchar, 20}, [Email]},
                            {sql_integer, [Id]}
                            ]),
    stop(Ref),
    case Result of
        {updated, _} -> io:fwrite("Успешно обновлено. ~n", []);
        Reason -> 
            io:fwrite("Ошибка при обновлении записи: ~p ~n", [Reason])
    end.

del() ->
    Ref = start(),
    {ok, [Id]} = io:fread("Введите Id удаляемой записи: ", "~d"),
    Result = odbc:param_query(Ref,
                            "DELETE FROM students WHERE id = ?",
                            [{sql_integer, [Id]}
                            ]),
    stop(Ref),
    case Result of
        {updated, _} -> io:fwrite("Запись с id ~p успешно удалена. ~n", [Id]);
        Reason -> 
            io:fwrite("Ошибка при удалении записи: ~p ~n", [Reason]),
            init()
    end.