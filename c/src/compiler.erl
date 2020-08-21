-module(compiler).

-export([unroll/1, compile_literal/1]).


compile_literal(S) ->
    literal(S, [], [], false).

literal([literal | C], M, L, false) ->
    M1 = execute_latest_quote(M, []),
    literal(C, M1, L, false);
literal([literal | _C], _M, _L, true) -> error_doulbe_literal;
literal(['[' | C], M, L, false) ->
    literal(C, M, L, true);
literal(['[' | _C], _M, _L, true) -> error_double_macro;
literal([']' | C], M, L, true) ->
    literal(C, [quote(lists:reverse(L)) | M], [], false);
literal([']' | _C], _M, _L, false) -> error_wrong_right_brick;
literal([H | C], M, L, true) ->
    literal(C, M, [H | L], true);
literal([H | C], M, L, false) ->
    literal(C, [H | M], L, false);
literal([], M, _L, _B) ->
    lists:reverse(M).

quote(L) ->
    {quote, L}.

execute_latest_quote([{quote, L} | M], R) ->
    lists:reverse(R) ++ [interpreter:simple_eval(L) | M];
execute_latest_quote([H | M], R) ->
    execute_latest_quote(M, [H | R]);
execute_latest_quote([], R) ->
    lists:reverse([0 | R]).

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
