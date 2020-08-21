-module(expander).

-export([expand/1, expand/2]).

expand(C) ->
    C1 = check_inlines(C, []),
    expand(C1, []).

expand([OP | C], M) ->
    case op(OP, C, M) of
      {C1, M1} -> expand(C1, M1);
      ok -> ok
    end;
expand([], M) -> lists:reverse(M).

check_inlines([inline_start | C], R) ->
    {I, R1} = collect_inline(C, []),
    lists:reverse(R) ++ [I | R1];
check_inlines([H | C], R) ->
    check_inlines(C, [H | R]);
check_inlines([], R) ->
    lists:reverse(R).

op(inline_start, C, M) ->
    {P, C1} = collect_inline(C, []),
    {M1, _A1} = interpreter:eval([P], M, []),
    {C1, M1};
op(dip, C, M) ->
    {M1, _A1} = interpreter:eval([swap, tas, call, fas], M, []),
    {C , M1};
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