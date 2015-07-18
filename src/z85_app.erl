-module(z85_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	z85:start_link().

stop(_State) ->
	gen_server:stop(whereis(z85)).
