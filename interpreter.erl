-module(interpreter).

-export([bin2num/1, eval/1, eval/3, num2bin/1, test/0]).

eval(S) -> eval(S, [], []).

eval([toaltstack | C], [X | M], A) ->
    eval(C, M, [X | A]);
eval([fromaltstack | C], M, [X | A]) ->
    eval(C, [X | M], A);
eval(['+' | C], [Y, X | M], A) ->
    eval(C, [X + Y | M], A);
eval(['-' | C], [Y, X | M], A) ->
    eval(C, [X - Y | M], A);
eval(['*' | C], [Y, X | M], A) ->
    eval(C, [X * Y | M], A);
eval(['/' | C], [Y, X | M], A) ->
    eval(C, [X div Y | M], A);
eval(['%' | C], [Y, X | M], A) ->
    eval(C, [X rem Y | M], A);
eval(['>' | C], [Y, X | M], A) ->
    eval(C, [X > Y | M], A);
eval(['<' | C], [Y, X | M], A) ->
    eval(C, [X < Y | M], A);
eval(['>=' | C], [Y, X | M], A) ->
    eval(C, [X >= Y | M], A);
eval(['<=' | C], [Y, X | M], A) ->
    eval(C, [X =< Y | M], A);
eval(['&' | C], [Y, X | M], A) ->
    eval(C, [X band Y | M], A);
eval(['|' | C], [Y, X | M], A) ->
    eval(C, [X bor Y | M], A);
eval(['~' | C], [X | M], A) -> eval(C, [bnot X | M], A);
eval(['not' | C], [X | M], A) ->
    eval(C, [logic_not(X) | M], A);
eval(['xor' | C], [Y, X | M], A) ->
    eval(C, [X bxor Y | M], A);
eval([min | C], [Y, X | M], A) ->
    eval(C, [min(X, Y) | M], A);
eval([max | C], [Y, X | M], A) ->
    eval(C, [max(X, Y) | M], A);
eval([X | C], M, A)
    when is_integer(X) or is_binary(X) ->
    eval(C, [X | M], A);
eval([over | C], [X, Y | M], A) ->
    eval(C, [Y, X, Y | M], A);
eval([nip | C], [X, _Y | M], A) -> eval(C, [X | M], A);
eval(['if' | C], [H | M], A) ->
    {T, F, R} = branches(C),
    eval(choose(H, T, F) ++ R, M, A);
eval([nop | C], M, A) -> eval(C, M, A);
eval([split | C], [P, B | M], A) ->
    eval(C, split(B, P) ++ M, A);
eval([swap | C], [X, Y | M], A) ->
    eval(C, [Y, X | M], A);
eval([drop | C], [_X | M], A) -> eval(C, M, A);
eval([tuck | C], [X, Y | M], A) ->
    eval(C, [X, Y, X | M], A);
eval([OP | C], M, A) ->
    {C1, M1} = op(OP, C, M), eval(C1, M1, A);
eval([], M, A) -> {M, A}.

op(sha256, C, [H | M]) ->
    {C, [crypto:hash(sha256, H) | M]};
op(hash256, C, M) -> {[sha256, sha256 | C], M};
op(not0, C, M) -> {['not', 'not' | C], M};
op(negate, C, [H | M]) -> {C, [-H | M]};
op(bin2num, C, [H | M]) -> {C, [bin2num(H) | M]};
op(dup, C, [H | M]) -> {C, [H, H | M]}.

split(B, P) ->
    [binary:part(B, P, byte_size(B) - P),
     binary:part(B, 0, P)].

branches(C) -> split_branch(C, [], [], 0, t).

choose(0, _T, F) -> F;
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

bin2num(B) when byte_size(B) > 4 -> invalid_range;
bin2num(B) -> do_bin2num(flip_endian(B)).

do_bin2num(<<1:1, D/bits>>) ->
    -binary:decode_unsigned(<<0:1, D/bits>>);
do_bin2num(<<0:1, _D/bits>> = B) ->
    binary:decode_unsigned(B).

num2bin(N) when N > 0 ->
    B = <<N/big>>,
    B1 = case B of
	   <<1:1, _X/bits>> -> <<0:8, B/bits>>;
	   <<0:1, _X/bits>> -> B
	 end,
    flip_endian(B1);
num2bin(N) ->
    B = <<(-N)/big>>,
    B1 = case B of
	   <<1:1, _X/bits>> -> <<1:1, 0:7, B/bits>>;
	   <<0:1, X/bits>> -> <<1:1, X/bits>>
	 end,
    flip_endian(B1).

flip_endian(B) ->
    B1 = binary_to_list(B),
    list_to_binary(lists:reverse(B1)).

%% TESTING

test() -> test1(), test2().

test1() ->
    List = [{255, <<255, 0>>}, {1, <<1>>}, {127, <<127>>},
	    {128, <<128, 0>>}, {-1, <<129>>}, {-127, <<255>>},
	    {-255, <<255, 128>>}],
    Match = fun (N, B) -> N = bin2num(B), B = num2bin(N)
	    end,
    [Match(N, B) || {N, B} <- List].

test2() ->
    1 = bin2num(<<1, 0>>),
    -127 = bin2num(<<255>>),
    -127 = bin2num(<<127, 128>>).
