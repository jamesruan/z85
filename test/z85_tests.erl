-module(z85_tests).
-include_lib("eunit/include/eunit.hrl").


helloworld_test() ->
	{ok, Pid} = z85:start_link(),
	D = <<16#86, 16#4F, 16#D2, 16#6F, 16#B5, 16#59, 16#F7, 16#5B>>,
	E = <<"HelloWorld">>,
	{ok, E} = gen_server:call(Pid, {encode, D}),
	{ok, D} = gen_server:call(Pid, {decode, E}),
	gen_server:stop(Pid).

padding_test() ->
	{ok, Pid} = z85:start_link(),
	D = <<"Hello, World!">>,
	{ok, E} = gen_server:call(Pid, {encode, D}),
	{ok, D} = gen_server:call(Pid, {decode, E}),
	gen_server:stop(Pid).
