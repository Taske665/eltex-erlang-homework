test: test.beam q.beam stack.beam
	erl -s test run

test.beam: test.erl
	erlc test.erl

q.beam: q.erl
	erlc q.erl

stack.beam: stack.erl
	erls stack.erl