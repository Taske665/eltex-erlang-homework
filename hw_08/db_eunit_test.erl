-module(db_eunit_test).
-include_lib("eunit/include/eunit.hrl").

db_test() ->
    db_eunit:del(5),
    ?assertEqual({updated, 1}, db_eunit:ins(5, "Trump", "Donald", 70, "trump@mail.ru")),
    ?assertEqual({selected, ["id","last_name","first_name","age","email"], [{5, "Trump", "Donald", 70, "trump@mail.ru"}]}, db_eunit:find("Trump")),
    ?assertEqual({updated, 1}, db_eunit:upd(5, "maga@mail.ru")),
    ?assertEqual({selected, ["id","last_name","first_name","age","email"], [{5, "Trump", "Donald", 70, "maga@mail.ru"}]}, db_eunit:find("Trump")),
    ?assertEqual({updated, 1}, db_eunit:del(5)),
    ?assertEqual({selected, ["id","last_name","first_name","age","email"], []}, db_eunit:find("Trump")),
    ok.