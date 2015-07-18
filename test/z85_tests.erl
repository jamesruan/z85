-module(z85_tests).
-include_lib("eunit/include/eunit.hrl").

helloworld_test() ->
	application:start(z85),
	D = <<16#86, 16#4F, 16#D2, 16#6F, 16#B5, 16#59, 16#F7, 16#5B>>,
	E = <<"HelloWorld">>,
	{ok, E} = gen_server:call(z85, {encode, D}),
	{ok, D} = gen_server:call(z85, {decode, E}),
	application:stop(z85).

random_test() ->
	application:start(z85),
	D = crypto:rand_bytes(16),
	{ok, E} = gen_server:call(z85, {encode, D}),
	{ok, D} = gen_server:call(z85, {decode, E}),
	application:stop(z85).

padding_test() ->
	application:start(z85),
	D = crypto:rand_bytes(17),
	{ok, E} = gen_server:call(z85, {encode, D}),
	{ok, D} = gen_server:call(z85, {decode, E}),
	application:stop(z85).
