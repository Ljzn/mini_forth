-module(interpreter).

-export([bin2num/1, eval/1, eval/3, num2bin/1,
	 preworks/1, simple_eval/1, test/0]).

eval(S) -> eval(S, [], []).

eval([tas | C], [X | M], A) -> eval(C, M, [X | A]);
eval([fas | C], M, [X | A]) -> eval(C, [X | M], A);
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
eval([size | C], [X | M], A) ->
    eval(C, [byte_size(X), X | M], A);
eval([X | C], M, A) when is_integer(X) ->
    eval(C, [X | M], A);
% eval([X | C], M, A)
%     when is_integer(X) ->
%     eval(C, [binary:encode_unsigned(X) | M], A);
eval([X | C], M, A) when is_binary(X) ->
    eval(C, [X | M], A);
eval([pick | C], [N | M], A) ->
    M1 = pick(M, N), eval(C, M1, A);
eval([over | C], [X, Y | M], A) ->
    eval(C, [Y, X, Y | M], A);
eval(['if' | C], [H | M], A) ->
    {T, F, R} = branches(C),
    eval(choose(H, T, F) ++ R, M, A);
eval([nop | C], M, A) -> eval(C, M, A);
eval([split | C], [P, B | M], A) ->
    eval(C, split(B, P) ++ M, A);
eval([cat | C], [Y, X | M], A) ->
    eval(C, [<<X/binary, Y/binary>> | M], A);
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
op('=', C, [X, X | M]) -> {C, [1 | M]};
op('=', C, [_Y, _X | M]) -> {C, [0 | M]};
op(verify, _C, [0 | _M]) -> io:format("verify failed.");
op(verify, _C, [false | _M]) ->
    io:format("verify failed.");
op(verify, _C, [<<>> | _M]) ->
    io:format("verify failed.");
op(verify, C, [_X | M]) -> {C, M};
op('num=verify', C, [Y, X | M]) ->
    case bin2num(Y) == bin2num(X) of
      true -> {C, M};
      false ->
	  io:format("equal_verify failed.\ntop: ~p, second: "
		    "~p~n",
		    [Y, X])
    end;
op('=verify', C, [X, X | M]) -> {C, M};
op('=verify', _C, [Y, X | _M]) ->
    io:format("equal_verify failed.\ntop: ~p, second: "
	      "~p~n",
	      [Y, X]);
op(checksignverify, C, M) ->
    % TODO
    {C, M};
op(checkmultisignverify, C, M) ->
    % TODO
    {C, M};
op('.', C, [X | M]) -> io:format("~p ", [X]), {C, M};
op(cr, C, M) -> io:format("~n", []), {C, M};
op(pf_inv, C, [X | M]) -> {C, [b_crypto:pf_inv(X) | M]};
op(rand_bytes, C, [X | M]) ->
    {C, [crypto:strong_rand_bytes(X) | M]};
op(call, C, [{quote, Q} | M]) ->
    {C, lists:reverse(Q) ++ M};
op({quote, _X} = Q, C, M) -> {C, [Q | M]};
op(dip, C, M) -> pop(dip, C, M);
op({inline, _X} = I, C, M) -> pop(I, C, M).

preworks(C) -> preworks(C, [], []).

preworks([OP | C], M, A) ->
    case pop(OP, C, M) of
      {C1, M1} -> preworks(C1, M1, A);
      missing -> preworks(C, [OP | M], A)
    end;
preworks([], M, _A) ->
    M1 = lists:reverse(M),
    case lists:any(fun (X) when X == dip orelse X == call ->
			   true;
		       ({inline, _}) -> true;
		       (_) -> false
		   end,
		   M1)
	of
      true -> preworks(M1);
      false -> M1
    end.

pop(call, C, [{quote, Q} | M]) -> {Q ++ C, M};
pop(dip, C, [{quote, Q}, H | M]) ->
    io:format("M: ~p~n", [M]),
    M1 = [H | lists:reverse(Q) ++ M],
    io:format("M1: ~p~n", [M1]),
    {C, M1};
pop({inline, Code}, C, M) ->
    {M1, _A1} = eval(Code, M, []), {C, M1};
pop(call, C, M) -> io:format("ERROR: ~p~n", [{C, M}]);
pop(_OP, _C, _M) -> missing.

bool(true) -> 1;
bool(false) -> 0.

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

%% TESTING

simple_eval(C) -> hd(element(1, eval(C))).

test() -> test1(), test2(), test3(), test4(), test5().

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
