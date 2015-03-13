#!/usr/bin/env swipl -q
:- initialization main.

:- use_module(parser/parser).
:- portray_text(true).


main :-
  current_prolog_flag(argv, [File]),
  parse(File, AST, Errors),
  (Errors = [] ->
  	print_list(AST, print_line);
  	print_list(Errors, print_err)
  ),
  halt.
main :- halt(1).

print_line(L) :- format("~p", [L]).
print_err((NN, ErrStr)) :- format("Error at line ~d: ~p...", [NN, ErrStr]).

print_list([], _).
print_list([H|T], Iter) :- Clause =.. [Iter,H], call(Clause), nl, print_list(T, Iter).

