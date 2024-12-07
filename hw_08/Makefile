test: test.beam db_eunit_test.beam db_eunit.beam
	erl -s test run

test.beam: test.erl
	erlc test.erl

db_eunit_test.beam: db_eunit_test.erl
	erlc db_eunit_test.erl

db_eunit.beam: db_eunit.erl
	erlc db_eunit.erl
