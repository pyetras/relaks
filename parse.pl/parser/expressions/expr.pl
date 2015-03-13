:- ensure_loaded(latin).

expr(Expr) --> expr0(Expr).

expr0(Expr) --> expr1(ExprL), expr0_rest(ExprL, Expr).
expr0_rest(ExprL, expression(either, [ExprL,ExprR|T])) -->
  blanks, pipe, !, expr1(ExprR), expr0_rest_rest(T).
expr0_rest(Expr, Expr) --> [].
expr0_rest_rest([ExprL|T]) --> blanks, pipe, !, expr1(ExprL), expr0_rest_rest(T).
expr0_rest_rest([]) --> [].

% <Expr>|.. : <Expr>|..
expr1(Expr) --> blanks, dots, !, expr1_rest_ne(expression(const, dots), Expr).
expr1(Expr) --> expr2(Expr1), expr1_rest(Expr1, Expr).
expr1_rest_ne(ExprL, expression(slice, (ExprL, expression(const, dots)))) --> blanks, colon, blanks, dots, !.
expr1_rest_ne(ExprL, expression(slice, (ExprL, ExprR))) --> blanks, colon, expr2(ExprR).
expr1_rest(ExprL, Expr) --> expr1_rest_ne(ExprL, Expr), !.
expr1_rest(Expr, Expr) --> [].

% <Expr> and | or <Expr>
expr2(Expr) --> expr3(ExprL), expr2_rest(ExprL, Expr).
expr2_op(and) --> blanks, and, !.
expr2_op(or) --> blanks, or.
expr2_rest(ExprL, Expr) --> expr2_op(Op), !, expr3(ExprR),
  expr2_rest(expression(bool_math, (Op, ExprL, ExprR)), Expr).
expr2_rest(Expr, Expr) --> [].

% <Expr> == <Expr>
expr3(Expr) --> expr4(ExprL), expr3_rest(ExprL, Expr).
expr3_rest(ExprL, Expr) -->
  blanks, cmp_op(Op), { =(Op, ==) }, !, expr4(ExprR),
  expr3_rest(expression(bool_math, (Op, ExprL, ExprR)), Expr).
expr3_rest(Expr, Expr) --> [].

% <Expr> < <Expr> ...
expr4(Expr) --> expr5(ExprL), expr4_rest(ExprL, Expr).
expr4_rest(ExprL, expression(bool_math, (Op, ExprL, ExprR))) -->
  blanks, cmp_op(Op), { \=(Op, ==) }, !, expr5(ExprR). % does not associate
expr4_rest(Expr, Expr) --> [].

% <Expr> +|- <Expr>
expr5(Expr) --> expr6(ExprL), expr5_rest(ExprL, Expr).
expr5_rest(ExprL, Expr) -->
  blanks, math_op(Op), { =(Op, +), !; =(Op, -) }, !, expr6(ExprR),
  expr5_rest(expression(math, (Op, ExprL, ExprR)), Expr).
expr5_rest(Expr, Expr) --> [].

% <Expr> *|/ <Expr>
expr6(Expr) --> expr7(ExprL), expr6_rest(ExprL, Expr).
expr6_rest(ExprL, Expr) -->
  blanks, math_op(Op), { =(Op, *), !; =(Op, /) }, !, expr7(ExprR),
  expr6_rest(expression(math, (Op, ExprL, ExprR)), Expr).
expr6_rest(Expr, Expr) --> [].

% <Expr> ^ <Expr>
expr7(Expr) --> expr8(ExprL), expr7_rest(ExprL, Expr).
expr7_rest(ExprL, Expr) -->
  blanks, math_op(^), !, expr8(ExprR),
  expr7_rest(expression(math, (^, ExprL, ExprR)), Expr).
expr7_rest(Expr, Expr) --> [].

% not <Expr>
expr8(expression(bool_math, (not, Expr))) --> blanks, not, !, expr8(Expr).
expr8(Expr) --> expr9(Expr).

% * <Expr>
expr9(expression(findall, Expr)) --> blanks, star, !, expr9(Expr).
expr9(Expr) --> expr10(Expr).

% <Expr> ([[name:]Expr[, Expr]])
expr10(Expr) --> expr11(ExprL), expr10_rest(ExprL, Expr).
expr10_rest(LExpr, expression(call, (LExpr, ArgList))) -->
  blanks, lpar, !, expr_arg_list(ArgList), blanks, rpar.
expr10_rest(Expr, Expr) --> [].
expr_arg_list([(Arg, Expr)|T]) -->
  argument(1, Arg, OutIndex),
  expr(Expr), !,
  expr_arg_list_rest(OutIndex, T).
%bad
expr_arg_list([]) --> [].
expr_arg_list_rest(Index, [(Arg, Expr)|T]) -->
  blanks, comma,
  argument(Index, Arg, OutIndex), expr(Expr), !,
  expr_arg_list_rest(OutIndex, T).
expr_arg_list_rest(_, []) --> [].
%very very bad
argument(Index, Name, OutIndex) --> blanks, varname(Name), blanks, colon, !, { OutIndex is Index + 1 }.
argument(Index, Index, OutIndex) --> { OutIndex is Index + 1 }.

% <Expr>\[<Expr>[,<Expr>]\]
% <Expr>.<Expr>
expr11(Expr) --> expr12(ExprL), expr11_rest(ExprL, Expr).
expr11_rest(ExprL, Expr) -->
  blanks, lsq, !, expr_list_ne(IxList), blanks, rsq,
  expr11_rest(expression(access, (ExprL, IxList)), Expr).
expr11_rest(ExprL, Expr) --> dot, !, expr13(ExprR), expr11_rest(expression(dotaccess, (ExprL, ExprR)), Expr).
expr11_rest(Expr, Expr) --> [].

expr_list_e([H|T]) --> expr(H), !, expr_list(T).
%bad
expr_list_e([]) --> [].
expr_list_ne([H|T]) --> expr(H), expr_list(T).
expr_list([H|T]) --> blanks, comma, !, expr(H), expr_list(T).
expr_list([]) --> [].

% <Latin Expr>[|> <Latin Expr>]
expr12(expression(latin, [(Name, Arg)|T])) -->
  latin_expr(latin(Name, Arg)), !, { Name \= generate } , expr12_rest(T).
expr12(Expr) --> expr13(Expr).
expr12_rest([(Name, Arg)|T]) -->
  blanks, bigpipe, !,
  { H = latin(Name, Arg), Arg = (infer, _) }, latin_expr(H), { chainable(Name) }, expr12_rest(T).
expr12_rest([]) --> [].


expr13(Expr) --> blanks, lpar, !, expr(Expr), blanks, rpar.
expr13(Expr) --> latin_expr(Expr), !.

expr13(expression(atomic, (const, Val))) --> blanks, const(Val), !.
expr13(expression(atomic, (num_lit, Val))) --> blanks, num(Val), !.
expr13(expression(atomic, (string_lit, Val))) --> blanks, string_lit(Val), !.
expr13(expression(atomic, (list_lit, Exprs))) --> blanks, lsq, !, expr_list_e(Exprs), blanks, rsq.

expr13(expression(anon_block, (InArgs, OutArgs))) -->
  blanks, lbrace, !, anon_block_args(InArgs, OutArgs), blanks, rbrace.

expr13(expression(atomic, (var, Varname))) --> blanks, varname(Varname).

anon_block_args([In|T1], [Out|T2]) --> anon_block_arg(In, Out), anon_block_args_rest(T1, T2).
anon_block_args_rest([In|T1], [Out|T2]) -->
  blanks, comma, !, anon_block_arg(In, Out), anon_block_args_rest(T1, T2).
anon_block_args_rest([], []) --> [].

anon_block_arg(In, Out) --> blanks, varname(Left), anon_block_arg_rest(Left, In, Out).
anon_block_arg_rest(In, In, Out) --> blanks, rarrow, !, blanks, varname(Out).
anon_block_arg_rest(Out, In, Out) --> blanks, larrow, !, blanks, varname(In).
anon_block_arg_rest(IO, IO, IO) --> [].

const(true) --> tru, !.
const(false) --> fals, !.
const(null) --> null.

dots_to_str(expression(dotaccess, (ExprL, expression(atomic, (var, Varname)))), Str) :- !,
  VarDot = [0'.| Varname],
  dots_to_str(ExprL, Left),
  append(Left, VarDot, Str).
dots_to_str(expression(atomic, (var, Varname)), Varname).

user:portray(expression(atomic, (var, Name))) :- !,
  string_codes(Str, Name),
  format("Σ(~p ~w)", [var, Str]).

user:portray(expression(atomic, (string_lit, Name))) :- !,
  string_codes(Str, Name),
  format("Σ(~p ~q)", [string_lit, Str]).

user:portray(expression(atomic, (X, Y))) :-
format("Σ(~p ~p)", [X, Y]).


user:portray(expression(Name, Args)) :-
  format("Σ~p(~p)", [Name, Args]).

print_codes(Codes) :-
  string_codes(Str, Codes),
  format("~q", [Str]), nl.
