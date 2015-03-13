:- module(parser, [parse/3, parse_str/3]).
:- use_module(grammar).
:- use_module(library(list_util), [take/3]).
:- use_module(library(lambda)).

parse(File, AST, Errors) :-
  open(File, read, Stream),
  read_stream_to_codes(Stream, Str),
  close(Stream),
  append(Str, "\n", Strnl),
  parse_str(Strnl, AST, Errors).

parse_str(Str, CleanAST, Errors) :-
  program(AST, Str, Rest),
  (Rest = [] ->
    transform(AST, CleanAST), Errors = []
    ;
    append(Parsed, Rest, Str), count_nl(Parsed, N), NN is N + 1,
      length(Rest, Len), LL is min(20, Len), take(Rest, LL, Error), string_codes(ErrStr, Error),
      Errors = [(NN, ErrStr)]
  ).

count_nl([0'\n|T], N) :- !, count_nl(T, M), N is M + 1.
count_nl([_|T], N) :- count_nl(T, N).
count_nl([], 0).

flatten2(X, Y) :- flatten(X, Y), !.
flatten2(X, X) :- \+ is_list(X).

transform(AST, AST1) :- transform_hlp(AST, AST0), flatten(AST0, AST1).

transform_hlp(AST, AST1) :- is_list(AST), !, remove_nils(AST, AST0), maplist(\X^Y^(transform_hlp(X, Y)), AST0, AST1).
transform_hlp(statement(block, X), statement(block, Y)) :- !, transform_block(X, Y).
transform_hlp(statement(assign, (L, R)), Out) :-
  transform_latin_expr(L, R, LL), !,
  (is_list(LL) -> maplist(\X^Y^(Y = statement(assign, (L, X))), LL, Out);
    Out = statement(assign, (L, LL))
  ).

transform_hlp(X, X).

transform_block((Name, (InArgs, OutArgs), Assertions, Stmts),
          (Name, (InArgs, OutArgs), Assertions, Stmts2)) :- !,
  transform(Stmts, Stmts2).

transform_latin_expr(VarName, expression(latin, List), List2) :- !,
  List = [H|T],
  maplist(\X^Y^(X = (Name, _, Rest), Y = expression(latin_expr, (Name, VarName, Rest))), T, List0),
  List1 = [expression(latin_expr, H)|List0],
  maplist(\X^Y^(transform_hlp(X, Y)), List1, List2).

transform_latin_expr(_, X, X).

remove_nils(Stmts, Stmts1) :- exclude(X^(X = nil), Stmts, Stmts1).
