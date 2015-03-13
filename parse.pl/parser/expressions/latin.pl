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

	