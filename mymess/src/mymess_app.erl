-module(mymess_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("Starting mymess application...~n"),
    mymess_sup:start_link(),
    application:ensure_all_started(cowboy),
    application:ensure_all_started(sasl),
    mymess_user_storage:init(),
    mymess_online_user_storage:init(),
    mymess_send_later:init(),
    mymess_msg_server:start(),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/reg", mymess_registration_handler, []},
            {"/auth", mymess_auth_handler, []},
            {"/ws", mymess_ws_handler, []}
        ]}
    ]),
    io:format("Dispatch table created. Starting Cowboy server...~n"),
    {ok, _} = cowboy:start_clear(http, [{port, 1234}], #{
        env => #{dispatch => Dispatch}
    }),
    io:format("Cowboy server started on port 1234.~n"),
    {ok, self()}.

stop(_State) ->
    ok.