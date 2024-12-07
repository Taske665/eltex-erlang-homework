-module(db_example).
-export([start/0, create_table/0, insert_data/0, retrieve_data/0, stop/1]).

%% Start the ODBC connection
start() ->
    odbc:start(),
%% {ok, Ref} = odbc:connect("DSN=taske-665;UID=taske-665;PWD=Sacred", []),
    {ok, Ref} = odbc:connect("DSN=taske-665", []),
    Ref.

%% Create a table
create_table() ->
    Ref = start(),
    odbc:sql_query(Ref, "CREATE TABLE prepods (id INT PRIMARY KEY, name VARCHAR(50))"),
    stop(Ref).

%% Insert data into the table
insert_data() ->
    Ref = start(),
    odbc:param_query(Ref, "INSERT INTO prepods (id, name) VALUES (?, ?)", [{sql_integer, [2,3,4]}, {{sql_varchar,20}, ["Alice", "Bob", "Pete"]}]),
    stop(Ref).

%% Retrieve data from the table
retrieve_data() ->
    Ref = start(),
    {selected, _, Rows} = odbc:sql_query(Ref, "SELECT * FROM prepods"),
    io:format("Users: ~p~n", [Rows]),
    stop(Ref).

%% Stop the ODBC connection
stop(Ref) ->
    odbc:disconnect(Ref).