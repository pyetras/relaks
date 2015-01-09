:- module(grammar, [program/3]).

/*
  program := instr [\n instr]
  instr := assignment |
*/
:- ensure_loaded(tokens).
:- portray_text(true).

%FOREACH { gen_blk | nested_gen_blk } %% [AS schema];
latin_expr(latin(foreach, (Rel, Stmts))) --> blanks, foreach, !, expr_rel(Rel), stmts_with_generate(Stmts).

%LIMIT alias n
latin_expr(latin(limit, (Rel, CountExpr))) --> blanks, limit, !, expr_rel(Rel), expr(CountExpr).

%FILTER alias BY expression
latin_expr(latin(filter, (Rel, Expr))) --> blanks, filter, !, expr_rel(Rel), blanks, by, expr_bool(Expr).

%ORDER alias BY { * [ASC|DESC] | field_alias [ASC|DESC] [, field_alias [ASC|DESC] …] }
latin_expr(latin(order, (Rel, OrderExpr))) --> blanks, order, !, expr_rel(Rel), blanks, by, expr_order(OrderExpr).

%SEARCH var1[, var2] { gen_blk | nested_gen_blk }
latin_expr(latin(search, (VarList, Stmts))) --> blanks, search, !, var_list(VarList), stmts_with_generate(Stmts).

%GENERATE expression [, expression …]
generate_expr(latin(generate, ColList)) --> blanks, generate, expr_cols(ColList).

chainable(limit) :- !.
chainable(filter) :- !.
chainable(order) :- !.
chainable(foreach) :- !.

expr_cols([H|T]) --> expr_col(H), expr_cols_rest(T).
expr_cols_rest([H|T]) --> blanks, comma, !, expr_col(H), expr_cols_rest(T).
expr_cols_rest([]) --> [].

expr_col(Expr) --> expr(ExprL), expr_col_rest(ExprL, Expr).
expr_col_rest(ExprL, statement(assign, (ExprR, ExprL))) --> blanks, as, !, expr(ExprR).
expr_col_rest(Expr, Expr) --> [].

stmts_with_generate(List) -->
  stmt_list_e(List1), generate_expr(Expr), { append(List1, [statement(generate, Expr)], List) }.

expr_order([(H, Direction)|T]) --> expr_rel(H), direction(Direction), expr_order_rest(T).
expr_order_rest([(H, Direction)|T]) --> blanks, comma, !, expr_rel(H), direction(Direction), expr_order_rest(T).
expr_order_rest([]) --> [].

direction(asc) --> blanks, asc, !.
direction(desc) --> blanks, desc, !.
direction(asc) --> [].

var_list_e([H|T]) --> blanks, varname(H), !, var_list_rest(T).
var_list_e([]) --> [].
var_list([H|T]) --> blanks, varname(H), var_list_rest(T).
var_list_rest([H|T]) --> blanks, comma, !, blanks, varname(H), var_list_rest(T).
var_list_rest([]) --> [].

expr_rel(Rel) --> { nonvar(Rel) }, !, [].
expr_rel(Rel) --> expr(Rel).

expr_bool(Expr) --> expr(Expr).

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

stmt(nil) -->
  eol, !.

stmt(statement(assign, (Left, Right))) -->
  expr(Left),
  blanks, eq_op, !,
  expr(Right),
  eol.

% block
top_stmt(statement(block, (Name, (InArgs, OutArgs), Assertions, Stmts))) -->
  blanks, def, !, blanks, varname(Name),
  blanks, lpar, block_arg_list(InArgs, OutArgs), blanks, rpar,
  block_rest(Assertions, Stmts).

top_stmt(Stmt) --> stmt(Stmt).

stmt_doend(List) --> blanks, begin, !, stmt_list_e(List), blanks, end.

stmt_list([H|T]) --> stmt(H), stmt_list_rest(T).
stmt_list_e(L) --> stmt_list_rest(L).
stmt_list_rest([H|T]) --> stmt(H), !, stmt_list_rest(T).
stmt_list_rest([]) --> [].

block_rest(Assertions, Stmts) --> blanks, where, !, assertion_list(Assertions), stmt_body(Stmts).
block_rest([], Stmts) --> stmt_body(Stmts).

stmt_body(Excall) --> blanks, ass, !, ex_call(Excall).
stmt_body(List) --> stmt_doend(List).

ex_call(external(Namespace, Addr)) -->
  blanks, varname(Namespace), !,
  dblcolon, expr(Expr), { dots_to_str(Expr, Addr) }.

block_arg_list(InArgs, OutArgs) -->
  def_ins(InArgs), { InArgs \= [] -> Comma = true, !; Comma = false },
  block_arg_list_rest(OutArgs, Comma).

block_arg_list_rest(OutArgs, Comma) -->
  maybe_comma(Comma),
  def_outs(OutArgs), !.
block_arg_list_rest([], _) --> [].

def_ins(InArgs) --> blanks, in, !, blanks, colon, block_varnames(InArgs).
def_ins([]) --> [].

def_outs(OutArgs) --> blanks, out, !, blanks, colon, block_varnames(OutArgs).
def_outs([]) --> [].

maybe_comma(true) --> !, blanks, comma.
maybe_comma(false) --> [].

block_varnames([H|T]) --> blanks, varname(H), block_varnames_rest(T).
block_varnames_rest([], Code, Code) :-
  blanks(Code, R1), comma(R1, R2), blanks(R2, R3), out(R3, R4), blanks(R4, R5), colon(R5, _), !. %hahahahahaha
block_varnames_rest([H|T]) --> blanks, comma, !, blanks, varname(H), block_varnames_rest(T).
%block_varnames_rest([H|T]) --> blanks, comma, blanks, varname(H), { H \= "out" }, !, block_varnames_rest(T).
block_varnames_rest([]) --> [].

assertion_list([H|T]) --> assertion(H), assertion_list_rest(T).
assertion_list_rest([H|T]) --> blanks, comma, !, assertion(H), assertion_list_rest(T).
assertion_list_rest([]) --> [].

assertion(Expr) --> blanks, expr(X), assertion_rest(X, Expr).
assertion_rest(expression(atomic, X), expression(call, (Name, Args))) -->
  { expression(atomic, X) = Expr },
  blanks, eq_op, !, expr(expression(call, (Name, Args1))),
  { append(Args1, [Expr], Args) }.
assertion_rest(expression(call, (Name, Args1)), expression(call, (Name, Args))) -->
  assertion_rest2(Args1, Args).

assertion_rest2(Args1, Args) -->
  blanks, eq_op, !, expr(Arg), { append(Args1, [Arg], Args) }.
assertion_rest2(Args, Args) --> [].

program([H|T]) --> top_stmt(H), program_rest(T).
program_rest([H|T]) --> top_stmt(H), !, program_rest(T).
program_rest([]) --> [].

%misc
%% portray(X) :-
%%   is_list(X),
%%   string_codes(Str, X),
%%   format("~w", [Str]).

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
