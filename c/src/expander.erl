-module(expander).

%% Macro Expander
%%

-export([expand/1, step/1]).

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

op({inline, Q}, C, M) ->
    {C, [{inline, expand(Q)} | M]};
op({quote, Q}, C, M) ->
    {C, [{quote, expand(Q)} | M]};
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

step(C) ->
    step(C, []).

step(C, A) ->
    'Elixir.MiniForth.U':debug(C, [{label, <<"Macro expanding">>}]),
    s(C, [], A).

s([{inline, Code} | T], R, A) ->
    step(lists:reverse(R) ++ [{e, X} || X <- Code] ++ T, A);
s([{e, X} | T], R, A) ->
    {M, A1} = eval(X, R, A),
    step(lists:reverse(M) ++ T, A1);
s([dip | T], [Q, X, Y | R], A) ->
    step(lists:reverse(R) ++ [Y, Q, call, X] ++ T, A);
s([call | T], [{quote, Q} | R], A) ->
    step(lists:reverse(R) ++ Q ++ T, A);
s([curry | T], [{quote, Q}, X | R], A) ->
    step(lists:reverse(R) ++ [{quote, [X | Q]} | T], A);
s([H|T], R, A) ->
    s(T, [H | R], A);
s([], R, _) ->
    lists:reverse(R).

eval(Op, M, A) ->
    Ops = 'Elixir.MiniForth.C':interpret_core_word(Op),
    % io:format("~p~n", [Ops]),
    interpreter:eval(Ops, M, A).