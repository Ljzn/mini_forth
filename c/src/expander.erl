-module(expander).

%% Macro Expander
%%

-export([expand/1, step/1]).

expand(C) ->
    C1 = check_inlines(C, []),
    C2 = check_seq(C1, []),
    mix_inlines(C2, []).

mix_inlines([OP | C], M) ->
    case op(OP, C, M) of
      {C1, M1} -> mix_inlines(C1, M1);
      ok -> ok
    end;
mix_inlines([], M) -> lists:reverse(M).

check_inlines([macro_start | C], R) ->
    {I, R1} = collect_inline(C, []),
    check_inlines(lists:reverse(R) ++ [I | R1], []);
check_inlines([H | C], R) ->
    check_inlines(C, [H | R]);
check_inlines([], R) ->
    lists:reverse(R).

check_seq(['{' | C], R) ->
    {I, R1} = collect_seq(C, []),
    lists:reverse(R) ++ [I | R1];
check_seq([H | C], R) ->
    check_seq(C, [H | R]);
check_seq([], R) ->
    lists:reverse(R).

op({macro, Q}, C, M) ->
    {C, [{macro, expand(Q)} | M]};
op({quote, Q}, C, M) ->
    {C, [{quote, expand(Q)} | M]};
op(X, C, M) ->
    {C, [X | M]}.

collect_inline([macro_end | C], R) ->
    {{macro, lists:reverse(R)}, C};
collect_inline([macro_start | C], R) ->
    {P, C1} = collect_inline(C, []),
    collect_inline(C1, [P | R]);
collect_inline([H | C], R) when is_list(H) ->
    collect_inline(C, [check_inlines(H, []) | R]); 
collect_inline([{quote, L} | C], R) ->
    collect_inline(C, [{quote, check_inlines(L, [])} | R]); 
collect_inline([H | C], R) ->
    collect_inline(C, [H | R]). 

collect_seq(['}' | C], R) ->
    {{seq, lists:reverse(R)}, C};
collect_seq(['{' | C], R) ->
    {P, C1} = collect_seq(C, []),
    collect_seq(C1, [P | R]);
collect_seq([H | C], R) when is_list(H) ->
    collect_seq(C, [check_seq(H, []) | R]); 
collect_seq([{quote, L} | C], R) ->
    collect_seq(C, [{quote, check_seq(L, [])} | R]); 
collect_seq([H | C], R) ->
    collect_seq(C, [H | R]). 

step(C) ->
    step(C, []).

step(C, A) ->
    'Elixir.MiniForth.U':debug({C, A}, [{label, <<"Macro expanding">>}]),
    s(C, [], A).

s([{macro, Code} | T], R, A) ->
    step(lists:reverse(R) ++ [{e, X} || X <- Code] ++ T, A);
s([{e, X} | T], R, A) ->
    {M, A1} = eval(X, R, A),
    step(lists:reverse(M) ++ T, A1);
s([Op | T], R, A) when Op == call; Op == curry; Op == eval ->
    R1 = cop(Op, R),
    step(lists:reverse(R1) ++ T, A);
s([H|T], R, A) ->
    s(T, [H | R], A);
s([], R, _) ->
    lists:reverse(R).

%% compile-time ops
cop(call, [{quote, Q} | M]) ->
    lists:reverse(Q) ++ M;
cop(curry, [{quote, Q}, X | M]) ->
    [{quote, [X | Q]} | M];
cop(eval, [{quote, Q} | M]) ->
    lists:reverse([{e, X} || X <- Q]) ++ M.

eval(Op, M, A) when Op == call; Op == curry; Op == eval ->
    {cop(Op, M), A};
eval(Op, M, A) ->
    Ops = 'Elixir.MiniForth.C':interpret_core_word(Op),
    interpreter:eval(Ops, M, A).