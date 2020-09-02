-module(interpreter).

-export([bin2num/1, eval/1, eval/3, num2bin/1,
	 simple_eval/1]).

eval(S) -> eval(S, [], []).

%% C: codes, M: main_stack, A: alt_stack

%% Plugin OPCodes
eval([print_stack | C], M, A) ->
    'Elixir.MiniForth.U':print_stack(M, A),
    eval(C, M, A);
%% Bitcoin OPCodes
eval([tas | C], [X | M], A) -> eval(C, M, [X | A]);
eval([fas | C], M, [X | A]) -> eval(C, [X | M], A);
eval([], M, A) -> {M, A};
eval(['if' | C], [H | M], A) ->
    {T, F, R} = branches(C),
    eval(choose(H, T, F) ++ R, M, A);
eval([H | C], M, A) ->
    'Elixir.MiniForth.U':debug({H, C, M}, []),
    M1 = op(H, M), eval(C, M1, A).

op(0, M) -> [<<>> | M];
op('&', [Y, X | M]) -> [bitand(X, Y) | M];
op('|', [Y, X | M]) -> [bitor(X, Y) | M];
op('~', [X | M]) -> [invert(X) | M];
op(lshift, [Y, X | M]) -> [lshift(X, Y) | M];
op(rshift, [Y, X | M]) -> [rshift(X, Y) | M];
op(size, [X | M]) -> 
    % io:format("~p~n", [M]),
    [num2bin(byte_size(X)), X | M];
op(X, M) when is_integer(X) -> [num2bin(X) | M];
op(X, M) when is_binary(X) -> [X | M];
op(pick, [N | M]) -> pick(M, N);
op(roll, [N | M]) -> roll(M, N);
op(split, [P, B | M]) -> split(B, P) ++ M;
op(cat, [Y, X | M]) -> [<<X/binary, Y/binary>> | M];
op(drop, [_X | M]) -> M;
op('+', [Y, X | M]) -> [add(X, Y) | M];
op('*', [Y, X | M]) -> [mul(X, Y) | M];
op('/', [Y, X | M]) -> [divide(X, Y) | M];
op('%', [Y, X | M]) -> [do_rem(X, Y) | M];
op(sha256, [H | M]) -> [crypto:hash(sha256, H) | M];
op(ripemd160, [H | M]) ->
    [crypto:hash(ripemd160, H) | M];
% there is only one data type in script -- binary
% the OP_BIN2NUM is trimming the bytes into minimal encoding
op(bin2num, [H | M]) -> [num2bin(bin2num(H)) | M];
op(num2bin, [Y, X | M]) -> [num2bin(X, Y) | M];
op('=', [X, X | M]) -> [<<1>> | M];
op('=', [_Y, _X | M]) -> [<<>> | M];
op('num=', [Y, X | M]) ->
    case bin2num(Y) == bin2num(X) of
      true -> [<<1>> | M];
      false -> [<<>> | M]
    end;
op('num=verify', [Y, X | M]) ->
    case bin2num(Y) == bin2num(X) of
      true -> M;
      false ->
	  raise_error("equal_verify failed.\nleft: ~p, right: "
		    "~p~n",
		    [X, Y])
    end;
op('=verify', [X, X | M]) -> M;
op('=verify', [Y, X | _M]) ->
    raise_error("equal_verify failed.\nleft: ~p, right: "
	      "~p~n",
	      [X, Y]);
op(checksig, [_Y, _X | M]) ->
    % TODO always return true now
    [<<1>> | M];
op(checkmultisignverify, M) ->
    % TODO
    M;
op(depth, M) -> [length(M) | M];
op('.', [X | M]) -> io:format("~p ", [X]), [X | M];
op(cr, M) -> io:format("~n", []), M;
op(pf_inv, [X | M]) -> [b_crypto:pf_inv(X) | M];
op(privkey_to_compressed_pubkey, [X | M]) ->
    ['Elixir.K':privkey_to_compressed_pubkey(X) | M];
op(ecdsa_verify, [Hash, Sig, Pk | M]) ->
    ['Elixir.K':ecdsa_verify(Pk, Sig, Hash) | M];
op(rand_bytes, [X | M]) ->
    [crypto:strong_rand_bytes(X) | M];
op(dersig_encode, [S, R | M]) ->
    ['Elixir.DERSig':encode(R, S) | M];
%% Compile Time Opcodes
op(dip, M)  -> [dip | M];
op(call, M)  -> [call | M];
op(E, _M) when is_atom(E) -> raise_error(E);
op(Q, M) -> [Q | M].

pick(M, N) -> do_pick(M, bin2num(N), []).

do_pick([H | M], 0, A) ->
    [H | lists:reverse(A) ++ [H | M]];
do_pick([H | M], N, A) -> do_pick(M, N - 1, [H | A]).

roll(M, N) -> do_roll(M, bin2num(N), []).

do_roll([H | M], 0, A) -> [H | lists:reverse(A) ++ M];
do_roll([H | M], N, A) -> do_roll(M, N - 1, [H | A]).

split(B, P) ->
    P1 = bin2num(P),
    [binary:part(B, P1, byte_size(B) - P1),
     binary:part(B, 0, P1)].

branches(C) -> split_branch(C, [], [], 0, t).

choose(X, T, F) ->
    case bin2num(X) of
        0 -> F;
        _ -> T
    end.


split_branch([endif | R], T, F, 0, _D) ->
    {lists:reverse(T), lists:reverse(F), R};
split_branch([else | R], T, F, 0, t) ->
    split_branch(R, T, F, 0, f);
split_branch(['if' | R], T, F, L, t) ->
    split_branch(R, ['if' | T], F, L + 1, t);
split_branch(['if' | R], T, F, L, f) ->
    split_branch(R, T, ['if' | F], L + 1, f);
split_branch([endif | R], T, F, L, t) ->
    split_branch(R, [endif | T], F, L - 1, t);
split_branch([endif | R], T, F, L, f) ->
    split_branch(R, T, [endif | F], L - 1, f);
split_branch([X | R], T, F, L, t) ->
    split_branch(R, [X | T], F, L, t);
split_branch([X | R], T, F, L, f) ->
    split_branch(R, T, [X | F], L, f).

bin2num(<<>>) -> 0;
bin2num(B) when is_integer(B) -> B;
bin2num(B) -> do_bin2num(flip_endian(B)).

do_bin2num(<<1:1, D/bits>>) ->
    -binary:decode_unsigned(<<0:1, D/bits>>);
do_bin2num(<<0:1, _D/bits>> = B) ->
    binary:decode_unsigned(B).

num2bin(0) -> <<>>;
num2bin(N) when is_bitstring(N) -> N;
num2bin(N) when N >= 0 ->
    B = binary:encode_unsigned(N),
    B1 = case B of
	   <<1:1, _X/bits>> -> <<0:8, B/bits>>;
	   <<0:1, _X/bits>> -> B
	 end,
    flip_endian(B1);
num2bin(N) ->
    B = binary:encode_unsigned(-N),
    B1 = case B of
	   <<1:1, _X/bits>> -> <<1:1, 0:7, B/bits>>;
	   <<0:1, X/bits>> -> <<1:1, X/bits>>
	 end,
    flip_endian(B1).

num2bin(_, 0) ->
    raise_error('SCRIPT_ERR_IMPOSSIBLE_ENCODING');
num2bin(N, S) -> B = num2bin(N), S1 = bin2num(S), pad_zeros(B, S1).

pad_zeros(<<>>, S) ->
    <<0:(8*S)>>;
pad_zeros(B, S0) ->
    S = bin2num(S0),
    case byte_size(B) < S of
      true ->
	  <<H:1, T/bits>> = flip_endian(B),
	  pad_zeros(flip_endian(<<H:1, 0:8, T/bits>>), S);
      false -> B
    end.

flip_endian(B) ->
    B1 = binary_to_list(B),
    list_to_binary(lists:reverse(B1)).

simple_eval(C) -> hd(element(1, eval(C))).

bitand(A, B) ->
    bitand(A, B, <<>>).

bitand(<<1:1, A/bits>>, <<1:1, B/bits>>, R) ->
    bitand(A, B, <<R/bits, 1:1>>);
bitand(<<_:1, A/bits>>, <<_:1, B/bits>>, R) ->
    bitand(A, B, <<R/bits, 0:1>>);
bitand(<<>>, <<>>, R) -> R.

bitor(A, B) ->
    bitor(A, B, <<>>).

bitor(<<0:1, A/bits>>, <<0:1, B/bits>>, R) ->
    bitor(A, B, <<R/bits, 0:1>>);
bitor(<<_:1, A/bits>>, <<_:1, B/bits>>, R) ->
    bitor(A, B, <<R/bits, 1:1>>);
bitor(<<>>, <<>>, R) -> R.

invert(A) ->
    invert(A, <<>>).

invert(<<0:1, A/bits>>, R) ->
    invert(A, <<R/bits, 1:1>>);
invert(<<1:1, A/bits>>, R) ->
    invert(A, <<R/bits, 0:1>>);
invert(<<>>, R) ->
    R.

rshift(<<>>, _) -> <<>>;
rshift(_, N) when is_integer(N), N < 0 ->
    raise_error('SCRIPT_ERR_INVALID_NUMBER_RANGE');
rshift(B, 0) -> B;
rshift(B, N) when is_bitstring(B), is_integer(N) ->
    Size = bit_size(B) - 1,
    <<B1:Size/bits, _:1>> = B,
    rshift(<<0:1, B1/bits>>, N - 1);
rshift(B, N) -> rshift(num2bin(B), bin2num(N)).

lshift(<<>>, _) -> <<>>;
lshift(_, N) when is_integer(N), N < 0 ->
    raise_error('SCRIPT_ERR_INVALID_NUMBER_RANGE');
lshift(B, 0) -> B;
lshift(B, N) when is_bitstring(B), is_integer(N) ->
    Size = bit_size(B) - 1,
    <<_:1, B1:Size/bits>> = B,
    lshift(<<B1/bits, 0:1>>, N - 1);
lshift(B, N) -> lshift(num2bin(B), bin2num(N)).

mul(X, Y) ->
    num2bin(bin2num(X) * bin2num(Y)).
add(X, Y) ->
    num2bin(bin2num(X) + bin2num(Y)).
divide(X, Y) ->
    num2bin(bin2num(X) / bin2num(Y)).
do_rem(X, Y) ->
    num2bin(bin2num(X) rem bin2num(Y)).

raise_error(S) -> error(S).
raise_error(S, Args) ->
    S1 = io:format(S, Args),
    error(S1).