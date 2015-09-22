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
	{ok, Pid} = gen_server:start_link(?MODULE, [], []),
	register(z85, Pid),
	{ok, Pid}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec encode(A :: binary(), Table :: tuple()) -> {ok, binary()}.
encode(A, T) ->
	{Encode_table, _Decode_table} = T,
	S = erlang:byte_size(A) div 4,
	R = erlang:byte_size(A) rem 4,
	case R of 
	0 ->
		{ok, do_encode(A, Encode_table)};
	R ->
		L = S * 5 + 5 - (4-R),
		<<Z85:L/binary, _/binary>> = do_encode(padding_bin(A, 4-R), Encode_table),
		{ok, Z85}
	end.

-spec decode(A :: binary(), Table :: tuple()) -> {ok, binary()}.
decode(A, T) ->
	{_Encode_table, Decode_table} = T,
	S = erlang:byte_size(A) div 5,
	R = erlang:byte_size(A) rem 5,
	case R of 
	0 ->
		{ok, do_decode(A, Decode_table)};
	R ->
		L = S * 4 + 4 - (5-R),
		<<B:L/binary, _/binary>> = do_decode(padding_z85(A, 5-R), Decode_table),
		{ok, B}
	end.

-spec padding_bin(A :: binary(), L :: integer()) -> binary().
padding_bin(A, L) ->
	P = rep(<<0>>, L), % padding with smallest value 0 is not optional
	<<A/binary, P/binary>>.

-spec padding_z85(A :: binary(), L :: integer()) -> binary().
padding_z85(A, L) ->
	P = rep(<<"#">>, L), % padding with biggest value 84 is not optional
	<<A/binary, P/binary>>.

-spec rep(A :: binary(), L :: integer()) -> binary().
rep(A, L) ->
	<< <<X>> || <<X>> <- lists:duplicate(L, A) >>.

-spec do_encode(A :: binary(), Table :: [tuple()]) -> binary().
% encode 4 bytes padded data
do_encode(A, Table) ->
	L32 = [I || <<I:32/unsigned-big>> <= A],
	Lr85 = binlist_to_r85(L32),
	erlang:list_to_binary(lists:map(fun (X) -> proplists:get_value(X, Table) end, Lr85)).

-spec do_decode(A :: binary(), Table :: [tuple()]) -> binary().
% decode 5 bytes padded data
do_decode(A, Table) ->
	L = lists:map(fun (X) -> proplists:get_value(X, Table) end, erlang:binary_to_list(A)),
	Lc5 = split_by(5, L),
	Lr = r85list_to_bin(Lc5),
	erlang:list_to_binary(Lr).

-spec split_by(N :: integer(), L :: [integer()]) -> [[integer()]].
% split list into sublists of length N
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
% [ i32() ] -> [ r85() ]
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
		if length(L) < 4 ->
			i_to_r85(A, [B] ++ L);
		true ->
			[B] ++ L
		end;
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
