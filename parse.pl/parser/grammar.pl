:- module(grammar, [program/3]).
/*
  program := instr [\n instr]
  instr := assignment |
*/
:- ensure_loaded(expressions/expr).
:- ensure_loaded(tokens).
:- portray_text(true).

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

