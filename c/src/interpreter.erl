-module(interpreter).

-export([bin2num/1, eval/1, eval/3, num2bin/1,
	 simple_eval/1]).

eval(S) -> eval(S, [], []).

%% C: codes, M: main_stack, A: alt_stack

eval([tas | C], [X | M], A) -> eval(C, M, [X | A]);
eval([fas | C], M, [X | A]) -> eval(C, [X | M], A);
eval([], M, A) -> {M, A};
eval(['if' | C], [H | M], A) ->
    {T, F, R} = branches(C),
    eval(choose(H, T, F) ++ R, M, A);
eval([H | C], M, A) -> M1 = op(H, M), eval(C, M1, A).

op('%', [Y, X | M]) -> [X rem Y | M];
op('<', [Y, X | M]) -> [bool(X < Y) | M];
op('>=', [Y, X | M]) -> [bool(X >= Y) | M];
op('<=', [Y, X | M]) -> [bool(X =< Y) | M];
op('&', [Y, X | M]) -> [X band Y | M];
op('|', [Y, X | M]) -> [X bor Y | M];
op('~', [X | M]) -> [bnot X | M];
op('and', [Y, X | M]) -> [bool(bool(X) + bool(Y) == 2) | M];
op('or', [Y, X | M]) -> [bool(bool(X) + bool(Y) > 0 ) | M];
op('not', [X | M]) -> [logic_not(X) | M];
op('^', [Y, X | M]) -> [X bxor Y | M];
op(size, [0 | M]) -> [0, 0 | M];
op(size, [X | M]) -> [byte_size(X), X | M];
op(X, M) when is_integer(X) -> [X | M];
% op(X, M)
%     when is_integer(X) ->
%   [binary:encode_unsigned(X) | M];
op(X, M) when is_binary(X) -> [X | M];
op(pick, [N | M]) -> pick(M, N);
op(over, [X, Y | M]) -> [Y, X, Y | M];
op(nop, M) -> M;
op(split, [P, B | M]) -> split(B, P) ++ M;
op(cat, [0, 0 | M]) -> [<<>> | M];
op(cat, [Y, 0 | M]) -> [Y | M];
op(cat, [0, X | M]) -> [X | M];
op(cat, [Y, X | M]) -> [<<X/binary, Y/binary>> | M];
op(swap, [X, Y | M]) -> [Y, X | M];
op(drop, [_X | M]) -> M;
op(tuck, [X, Y | M]) -> [X, Y, X | M];
op('+', [Y, X | M]) -> [X + Y | M];
op('-', [Y, X | M]) -> [X - Y | M];
op('*', [Y, X | M]) -> [X * Y | M];
op('/', [Y, X | M]) -> [X div Y | M];
op(sha256, [H | M]) -> [crypto:hash(sha256, H) | M];
op('1-', [X | M]) -> [X - 1 | M];
op(negate, [H | M]) -> [-H | M];
op(bin2num, [H | M]) -> [bin2num(H) | M];
op(num2bin, [Y, X | M]) -> [num2bin(X, Y) | M];
op(dup, [H | M]) -> [H, H | M];
op('=', [X, X | M]) -> [1 | M];
op('=', [_Y, _X | M]) -> [0 | M];
op(verify, [0 | _M]) -> io:format("verify failed.");
op(verify, [false | _M]) -> io:format("verify failed.");
op(verify, [<<>> | _M]) -> io:format("verify failed.");
op(verify, [_X | M]) -> M;
op('num=verify', [Y, X | M]) ->
    case bin2num(Y) == bin2num(X) of
      true -> [1 | M];
      false -> [0 | M]
    end;
op('num=verify', [Y, X | M]) ->
    case bin2num(Y) == bin2num(X) of
      true -> M;
      false ->
	  io:format("equal_verify failed.\ntop: ~p, second: "
		    "~p~n",
		    [Y, X])
    end;
op('=verify', [X, X | M]) -> M;
op('=verify', [Y, X | _M]) ->
    io:format("equal_verify failed.\ntop: ~p, second: "
	      "~p~n",
	      [Y, X]);
op(checksignverify, M) ->
    % TODO
    M;
op(checkmultisignverify, M) ->
    % TODO
    M;
op('.', [X | M]) -> io:format("~p ", [X]), M;
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
op({quote, _X} = Q, M) -> [Q | M].

bool(0) -> 0;
bool(false) -> 0;
bool(_) -> 1.

pick(M, N) -> do_pick(M, N, []).

do_pick([H | M], 0, A) ->
    [H | lists:reverse(A) ++ [H | M]];
do_pick([H | M], N, A) -> do_pick(M, N - 1, [H | A]).

split(B, P) ->
    [binary:part(B, P, byte_size(B) - P),
     binary:part(B, 0, P)].

branches(C) -> split_branch(C, [], [], 0, t).

choose(0, _T, F) -> F;
choose(false, _T, F) -> F;
choose(_X, T, _F) -> T.

logic_not(0) -> 1;
logic_not(_X) -> 0.

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

bin2num(B) when is_integer(B) -> B;
bin2num(B) -> do_bin2num(flip_endian(B)).

do_bin2num(<<1:1, D/bits>>) ->
    -binary:decode_unsigned(<<0:1, D/bits>>);
do_bin2num(<<0:1, _D/bits>> = B) ->
    binary:decode_unsigned(B).

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

num2bin(N, S) -> B = num2bin(N), pad_zeros(B, S).

pad_zeros(B, S) ->
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
