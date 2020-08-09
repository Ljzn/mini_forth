-module(interpreter).

-export([bin2num/1, eval/1, eval/3, num2bin/1, test/0,
	 unroll/1]).

eval(S) -> eval(S, [], []).

eval([tas | C], [X | M], A) ->
    eval(C, M, [X | A]);
eval([fas | C], M, [X | A]) ->
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
    eval(C, [bool(X > Y) | M], A);
eval(['<' | C], [Y, X | M], A) ->
    eval(C, [bool(X < Y) | M], A);
eval(['>=' | C], [Y, X | M], A) ->
    eval(C, [bool(X >= Y) | M], A);
eval(['<=' | C], [Y, X | M], A) ->
    eval(C, [bool(X =< Y) | M], A);
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
    case op(OP, C, M) of
      {C1, M1} -> eval(C1, M1, A);
      ok -> ok
    end;
eval([], M, A) -> {M, A}.

op(sha256, C, [H | M]) ->
    {C, [crypto:hash(sha256, H) | M]};
op(hash256, C, M) -> {[sha256, sha256 | C], M};
op(not0, C, M) -> {['not', 'not' | C], M};
op(negate, C, [H | M]) -> {C, [-H | M]};
op(bin2num, C, [H | M]) -> {C, [bin2num(H) | M]};
op(num2bin, C, [Y, X | M]) -> {C, [num2bin(X, Y) | M]};
op(dup, C, [H | M]) -> {C, [H, H | M]};
op(rot, C, [X3, X2, X1 | M]) -> {C, [X1, X3, X2 | M]};
op('=', C, [X, X | M]) -> {C, [1 | M]};
op('=', C, [_Y, _X | M]) -> {C, [0 | M]};
op('!', _C, [0 | _M]) -> io:format("verify failed.");
op('!', C, [_X | M]) -> {C, M};
op('=!', C, [X, X | M]) -> {C, M};
op('=!', _C, [Y, X | _M]) ->
    io:format("equal_verify failed.\ntop: ~p, second: "
	      "~p~n",
	      [Y, X]);
op('.', C, [X | M]) ->
    io:format("~p ", [X]),
    {C, M};
op(cr, C, M) ->
    io:format("~n", []),
    {C, M}.

bool(true) -> 1;
bool(false) -> 0.

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

%% Loop Unroll (not support nested loop)

unroll(S) -> unroll_loop(S, [], 0).

unroll_loop([do | C], [S, E | M], J) when is_integer(S), is_integer(E) ->
    {P, C1, Step} = find_loop_part(C, 0, []),
    C2 = loops(P, S, E, [], S, J, Step) ++ C1,
    unroll_loop(C2, M, J);
unroll_loop([do | _C], _M, _J) -> only_support_compile_time_loop;
unroll_loop([H | C], M, J) -> unroll_loop(C, [H | M], J);
unroll_loop([], M, _J) -> lists:reverse(M).

find_loop_part(['+loop' | C], 0, [Step | P]) ->
    {lists:reverse(P), C, Step};
find_loop_part([loop | C], 0, P) ->
    {lists:reverse(P), C, 1};
find_loop_part([do | C], L, P) ->
    find_loop_part(C, L + 1, [do | P]);
find_loop_part([loop | C], L, P) ->
    find_loop_part(C, L - 1, [loop | P]);
find_loop_part(['+loop' | C], L, P) ->
    find_loop_part(C, L - 1, ['+loop' | P]);
find_loop_part([X | C], L, P) ->
    find_loop_part(C, L, [X | P]).

loops(_P, S, E, R, _I, _J, Step) when Step > 0, S >= E; Step < 0, S =< E ->
    lists:flatten(lists:reverse(R));
loops(P, S, E, R, I, J, Step) ->
    loops(P, S + Step, E, [set_ij(unroll_loop(P, [], I), I, J) | R], I + Step, J, Step).

set_ij(P, I, J) -> set_ij(P, I, J, []).

set_ij([j | C], I, J, R) -> set_ij(C, I, J, [J | R]);
set_ij([i | C], I, J, R) -> set_ij(C, I, J, [I | R]);
set_ij([], _I, _J, R) -> lists:reverse(R);
set_ij([H | C], I, J, R) -> set_ij(C, I, J, [H | R]).

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

%% TESTING

simple_eval(C) -> hd(element(1, eval(C))).

test() ->
    test1(), test2(), test3(), test4(), test5(), test6().

test1() ->
    List = [{255, <<255, 0>>}, {1, <<1>>}, {127, <<127>>},
	    {128, <<128, 0>>}, {-1, <<129>>}, {-127, <<255>>},
	    {-255, <<255, 128>>}],
    Match = fun (N, B) -> N = bin2num(B), B = num2bin(N)
	    end,
    [Match(N, B) || {N, B} <- List].

test2() ->
    0 = bin2num(<<0>>),
    1 = bin2num(<<1, 0, 0, 0, 0, 0, 0>>),
    -1 = bin2num(<<1, 0, 0, 0, 0, 0, 128>>),
    0 = bin2num(<<128>>),
    0 = bin2num(<<0, 0, 0, 0, 0, 0, 128>>),
    1 = bin2num(<<1, 0>>),
    -127 = bin2num(<<255>>),
    -127 = bin2num(<<127, 128>>).

test3() ->
    {[<<"am">>], []} = eval([<<"I am a fish">>, 2, 2, rot,
			     rot, split, nip, swap, split, drop]).

test4() ->
    List = [{27, 7, 6}, {27, -7, 6}, {-27, 7, -6},
	    {-27, -7, -6}],
    Match = fun ({A, B, R}) -> {[R], []} = eval([A, B, '%'])
	    end,
    [Match(P) || P <- List].

test5() ->
    <<2, 0, 0, 0>> = simple_eval([2, 4, num2bin]),
    <<5, 0, 0, 128>> = simple_eval([-5, 4, num2bin]).

test6() ->
    {[70, 63, 56, 49, 42, 35, 28, 21, 14, 7], []} = eval([7
							  | unroll([11, 1, do,
								    dup, i, '*',
								    swap, loop,
								    drop])]).
