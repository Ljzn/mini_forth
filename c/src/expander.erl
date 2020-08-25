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
    'Elixir.MiniForth.U':debug(C, [{label, <<"Macro expanding">>}]),
    s(C, []).

s([{inline, Code} | T], R) ->
    step(lists:reverse(R) ++ [{e, X} || X <- Code] ++ T);
s([{e, X} | T], R) ->
    step(lists:reverse(eval(X, R)) ++ T);
s([dip | T], [Q, X, Y | R]) ->
    step(lists:reverse(R) ++ [Y, Q, call, X] ++ T);
s([call | T], [{quote, Q} | R]) ->
    step(lists:reverse(R) ++ Q ++ T);
s([H|T], R) ->
    s(T, [H | R]);
s([], R) ->
    lists:reverse(R).

%% Compile-time Opcodes
%%
%% If we need another opcode in the inline definition,
%% should be added here.

eval(over, [X, Y | R]) ->
    [Y, X, Y | R];
eval(swap, [X, Y | R]) ->
    [Y, X | R];
eval(dup, [X | R]) ->
    [X, X | R];
eval(Op, R) ->
    [Op | R].