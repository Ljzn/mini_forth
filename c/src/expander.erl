-module(expander).

-export([expand/1]).

expand(C) ->
    C1 = check_inlines(C, []),
    mix_inlines(C1, []).

mix_inlines([OP | C], M) ->
    case op(OP, C, M) of
      {C1, M1} -> mix_inlines(C1, M1);
      ok -> ok
    end;
mix_inlines([], M) -> lists:reverse(M).

check_inlines([inline_start | C], R) ->
    {I, R1} = collect_inline(C, []),
    lists:reverse(R) ++ [I | R1];
check_inlines([H | C], R) ->
    check_inlines(C, [H | R]);
check_inlines([], R) ->
    lists:reverse(R).

op({inline, I}, C, M) ->
    {I ++ C, M};
op({quote, Q}, C, M) ->
    {C, [{quote, mix_inlines(Q, [])} | M]};
op(X, C, M) ->
    {C, [X | M]}.

collect_inline([inline_end | C], R) ->
    {{inline, lists:reverse(R)}, C};
collect_inline([inline_start | C], R) ->
    {P, C1} = collect_inline(C, []),
    collect_inline(C1, [P | R]);
collect_inline([H | C], R) when is_list(H) ->
    collect_inline(C, [check_inlines(H, []) | R]); 
collect_inline([{quote, L} | C], R) ->
    collect_inline(C, [{quote, check_inlines(L, [])} | R]); 
collect_inline([H | C], R) ->
    collect_inline(C, [H | R]). 