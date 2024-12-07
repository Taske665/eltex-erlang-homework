-module(db_eunit).
-export([ins/5, del/1, upd/2, find/1]).

start() ->
    odbc:start(),
    {ok, Ref} = odbc:connect("DSN=taske-665", []),
    Ref.

stop(Ref) ->
    odbc:disconnect(Ref).

ins(Id, Last_name, First_name, Age, Email) ->
    Ref = start(),
    Result = odbc:param_query(Ref, "INSERT INTO students (id, last_name, first_name, age, email) 
                            VALUES (?, ?, ?, ?, ?)", 
                            [{sql_integer, [Id]}, 
                            {{sql_varchar, 20}, [Last_name]}, 
                            {{sql_varchar, 20}, [First_name]}, 
                            {sql_integer, [Age]}, 
                            {{sql_varchar, 30}, [Email]}
                            ]),
    stop(Ref),
    Result.

find(Last_name) ->
    Ref = start(),
    Result = odbc:param_query(Ref, 
                            "SELECT * FROM students 
                            WHERE last_name = ?",
                            [{{sql_varchar, 20}, [Last_name]}
                            ]),
    stop(Ref),
    Result.

upd(Id, Email) ->
    Ref = start(),
    Result = odbc:param_query(Ref,
                            "UPDATE students SET email =? WHERE id = ?",
                            [{{sql_varchar, 20}, [Email]},
                            {sql_integer, [Id]}
                            ]),
    stop(Ref),
    Result.

del(Id) ->
    Ref = start(),
    Result = odbc:param_query(Ref,
                            "DELETE FROM students WHERE id = ?",
                            [{sql_integer, [Id]}
                            ]),
    stop(Ref),
    Result.