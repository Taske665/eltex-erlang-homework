test: test.beam tcp_client_example.beam tcp_server.beam request_log.beam tcp_server_test.beam
	erl -s test run

test.beam: test.erl
	erlc test.erl

tcp_client_example.beam: tcp_client_example.erl
	erlc tcp_client_example.erl

tcp_server.beam: tcp_server.erl
	erlc tcp_server.erl

request_log.beam: request_log.erl
	erlc request_log.erl

tcp_server_test.beam: tcp_server_test.erl
	erlc tcp_server_test.erl