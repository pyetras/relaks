:- use_module(library(plunit)).

:- begin_tests(parser).

:- use_module(grammar).
:- portray_text(true).

%------------ LATIN ------------

test(generate1) :-
  Result = latin(generate, [expression(atomic, (var, "name1"))]),
  grammar:generate_expr(Result, "generate name1", []).

test(generate2) :-
  Result = latin(generate, [expression(atomic, (var, "name1")), expression(atomic, (var, "name2"))]),
  grammar:generate_expr(Result, "   generate name1   ,   name2", []),
  grammar:generate_expr(Result, "generate name1,name2", []).

test(generatef, [fail]) :-
  grammar:generate_expr(_, "generate  ", []).

test(limit) :-
  Result = latin(limit, (expression(atomic, (var, "rel")), expression(atomic, (num_lit, 5)))),
  grammar:latin_expr(X, "limit rel 5", []),
  Result = X.

test(foreach) :-
  Result = latin(foreach, (expression(atomic, (var, "something")), [statement(generate, latin(generate, [_]))])),
  grammar:latin_expr(X, "foreach something generate name1", []),
  Result = X.

test(foreach2) :-
  Result = latin(foreach, (_, Lst)),
  length(Lst, 2),
  grammar:latin_expr(Result, "foreach smt x = limit y smtelse; generate whatever", []).

test(foreach3) :-
  Test = "foreach smt x = foreach smtelse generate thething; generate whatever",
  grammar:latin_expr(_, Test, []).

test(order) :-
  Test = "order rel by x1, x2 asc, x3 desc",
  grammar:latin_expr(latin(order, _), Test, []).

test(filter) :-
  Test = "filter rel by x2 > x3 and x4 == 5",
  grammar:latin_expr(latin(filter, _), Test, []).

%------------ TOKENS ------------

test(timespans) :-
  Result = 70000,
  grammar:num(Result, "1 min 10 s", []),
  grammar:num(Result, "1m10s", []),
  grammar:num(R2, "-1min10s", []), abs(R2, Result).

test(timespans_signsf, [fail]) :-
  grammar:num(_, "1min-10s", []).

test(timespans_unit_orderf, [fail]) :-
  grammar:num(_, "1s 10min", []).

test(timespans_unit_nlf, [fail]) :-
  grammar:num(_, "1min\n10s", []).


%------------ EXPRESSIONS ------------
test(slice, [fail]) :-
  Tests = ["..:..", "..:5", "hay:5", "12:1m10s", "1:.."],
  member(Case, Tests),
  \+ grammar:expr(expression(slice, _), Case, []).

test(bool_and_or, [fail]) :-
  Tests = ["   5    and   4", "yes and no", "true or false"],
  member(Case, Tests),
  \+ grammar:expr(expression(bool_math, _), Case, []).

test(bool_math, [fail]) :-
  Tests = [(" 5  >= 4", >=), ("num ==num", ==), ("num<4", <), ("sth > 2", >), ("1 <= 1", <=)],
  member((Case, Op), Tests),
  \+ grammar:expr(expression(bool_math, (Op, _, _)), Case, []).

test(math, [fail]) :-
  Tests = [(" 5   +  5", +), ("num-4", -), ("num*  4", *), ("3 / num", /), ("num ^ 3", ^)],
  member((Case, Op), Tests),
  \+ grammar:expr(expression(math, (Op, _, _)), Case, []).

test(add_assoc) :-
  grammar:expr(X, "1 + 3 + 4 - 5", []),
  grammar:expr(X, "(((1 + 3) + 4) - 5)", []).

test(mul_assoc) :-
  grammar:expr(X, "1 * 3 * 4 / 5", []),
  grammar:expr(X, "((1 * 3) * 4) / 5", []).

test(pow_assoc) :-
  grammar:expr(X, "1 ^ 2 ^ 3 ^ 4", []),
  grammar:expr(X, "((1 ^ 2) ^ 3) ^ 4", []).

test(math_compo) :-
  grammar:expr(expression(math, X), "1 + 2 * 3 + 4 / 5 - 1*2 ^ 3", []),
  grammar:expr(expression(math, X), "1 + (2 * 3) + (4 / 5) - (1*(2 ^ 3))", []).

test(not_op) :-
  grammar:expr(expression(bool_math, (not, _)), " not  1", []).

test(findall_op) :-
  grammar:expr(expression(findall, _), "  * maybe", []).

test(dotaccess, [fail]) :-
  Tests = ["x.y", "  asd.yy.sf"],
  member(Case, Tests),
  \+ grammar:expr(expression(dotaccess, _), Case, []).

test(access, [fail]) :-
  Tests = ["asdf(yes, no)", "proc(1,2,3,4)", "proc(nom: nom, om: mom, 1)"],
  member(Case, Tests),
  \+ grammar:expr(expression(call, _), Case, []).

test(block_arg_list, [fail]) :-
  Tests = [("in: oee, out, three, one", 4, 0), ("in: oee, out: three, one", 1, 2), ("out: three, one", 0, 2)],
  member((Case, LL1, LL2), Tests),
  \+ (grammar:block_arg_list(L1, L2, Case, []), length(L1, LL1), length(L2, LL2)).

test(op_precedence) :-
  Test = "1 and 2 + 2 * 3.5 + asd.xy[1] == 5 == 4 * 2 ^ not 2 + 3 > 1",
  Correct = "1 and ((((2 + (2 * 3.5) + ((asd.xy)[1])) == 5) == (((4 * (2 ^ (not 2))) + 3)) > 1))",
  grammar:expr(X, Test, []), grammar:expr(X, Correct, []).

test(anon_block) :-
  Test = "{ one -> two, three, four -> five,six<-seven}",
  Ins = ["one", "three", "four", "seven"],
  Outs = ["two", "three", "five", "six"],
  grammar:expr(expression(anon_block, (Ins, Outs)), Test, []).

test(latin_chain) :-
  Test = "limit rel 5 |> filter by head > 5 |> limit two |> foreach generate x, y |> order by y",
  grammar:expr(expression(latin, L), Test, []),
  L = [(limit, _, _), (filter, _, _), (limit, _, _), (foreach, _, _), (order, _, _)].

%------------ STATEMENTS ------------
test(block_arg_list, [fail]) :-
  Tests = [("in: oee, out, three, one", 4, 0), ("in: oee, out: three, one", 1, 2), ("out: three, one", 0, 2)],
  member((Case, LL1, LL2), Tests),
  \+ (grammar:block_arg_list(L1, L2, Case, []), length(L1, LL1), length(L2, LL2)).

test(block) :-
  Test = "def proc(in: in1, out: out1) where is_integer(out1), length(in1) = 5 do end",
  grammar:top_stmt(statement(block, (_Name, (_Ins, _Outs), _Contracts, _Stmts)), Test, []).

test(block_ext) :-
  Test = "def proc(in:in1,out:out1) where is_integer(out1), length(in1) = 5 as python::default.sort",
  grammar:top_stmt(statement(block, (_, _, _, external("python", "default.sort"))), Test, []).


:- end_tests(parser).
