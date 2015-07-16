-module(z85).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
	L = "0123456789" ++ 
    	    "abcdefghij" ++
    	    "klmnopqrst" ++
    	    "uvwxyzABCD" ++
    	    "EFGHIJKLMN" ++
    	    "OPQRSTUVWX" ++
    	    "YZ.-:+=^!/" ++
    	    "*?&<>()[]{" ++
    	    "}@%$#",
	Encode_table = lists:zip(lists:seq(0,84), L),
	Decode_table = lists:zip(L, lists:seq(0,84)),
	{ok, {Encode_table, Decode_table}}.

handle_call(Request, _From, State) ->
	case Request of
	{encode, Data} ->
		{reply, encode(Data, State), State};
	{decode, Data} ->
		{reply, decode(Data, State), State};
	O ->
		{reply, {error, io_lib:format("Unknown call -p", O)}, State}
	end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
	gen_server:start_link(?MODULE, [], []).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec encode(A :: binary(), Table :: tuple()) -> {ok, binary()}.
encode(A, T) ->
	S = erlang:byte_size(A) div 4,
	R = erlang:byte_size(A) rem 4,
	case R of 
	0 ->
		{ok, do_encode(A, T)};
	R ->
		L = S * 5 + 5 - (4-R),
		<<Z85:L/binary, _/binary>> = do_encode(padding_bin(A, 4-R), T),
		{ok, Z85}
	end.

-spec decode(A :: binary(), Table :: tuple()) -> {ok, binary()}.
decode(A, T) ->
	S = erlang:byte_size(A) div 5,
	R = erlang:byte_size(A) rem 5,
	case R of 
	0 ->
		{ok, do_decode(A, T)};
	R ->
		L = S * 4 + 4 - (5-R),
		<<B:L/binary, _/binary>> = do_decode(padding_z85(A, 5-R), T),
		{ok, B}
	end.

-spec padding_bin(A :: binary(), L :: integer()) -> binary().
padding_bin(A, L) ->
	<<A/binary, 0:(L*8)>>.

-spec padding_z85(A :: binary(), L :: integer()) -> binary().
padding_z85(A, L) ->
	P = << <<"#">> || _ <- lists:seq(1, L) >>,
	<<A/binary, P/binary>>.

-spec do_encode(A :: binary(), Table :: tuple()) -> binary().
do_encode(A, Table) ->
	{Encode_table, _Decode_table} = Table,
	L32 = [I || <<I:32/unsigned-big>> <= A],
	Lr85 = binlist_to_r85(L32),
	erlang:list_to_binary(lists:map(fun (X) -> proplists:get_value(X, Encode_table) end, Lr85)).

-spec do_decode(A :: binary(), Table :: tuple()) -> binary().
do_decode(A, Table) ->
	{_Encode_table, Decode_table} = Table,
	L = lists:map(fun (X) -> proplists:get_value(X, Decode_table) end, erlang:binary_to_list(A)),
	Lc5 = split_by(5, L),
	Lr = r85list_to_bin(Lc5),
	erlang:list_to_binary(Lr).

-spec split_by(N :: integer(), L :: [integer()]) -> [[integer()]].
split_by(N, L) ->
	split_by(N, L, []).

-spec split_by(N :: integer(), L :: [integer()], R :: [[integer()]]) -> [[integer()]].
split_by(N, L, R) ->
	{H, T} = lists:split(N, L),
	case T of
	[] ->
		R ++ [H];
	_ ->
		split_by(N, T, R ++ [H])
	end.

-spec binlist_to_r85(L :: [integer()]) -> [integer()].
% [ i32() ] -> [ z85() ]
binlist_to_r85(L) ->
	Lr85 = lists:map(fun (I) -> i_to_r85(I) end, L), 
	lists:foldl(fun(I, Acc) -> Acc ++ I end, [], Lr85).

-spec i_to_r85(I :: integer()) -> [integer()].
i_to_r85(I) ->
	i_to_r85(I, []). 

-spec i_to_r85(I :: integer(), L :: [integer()]) -> [integer()].
i_to_r85(I, L) ->
	A = I div 85,
	B = I rem 85,
	case A of
	0 ->
		[B] ++ L;
	_ ->
		i_to_r85(A, [B] ++ L) 
	end.

-spec r85list_to_bin(L :: [[integer()]]) -> [integer()].
r85list_to_bin(L) ->
	Lbin = lists:map(fun (I) -> r85_to_bin(I) end, L), 
	lists:foldl(fun(I, Acc) -> Acc ++ I end, [], Lbin).

-spec r85_to_bin(L :: [integer()]) -> [integer()].
r85_to_bin(L) ->
	I = lists:foldl(fun(X, Acc) -> X + Acc*85 end, 0, L),
	erlang:binary_to_list(<<I:32/unsigned-big>>).
