#!/usr/bin/env swipl -q
:- initialization main.

:- ensure_loaded(parser/parser).


main :-
  current_prolog_flag(argv, [File]),
  parse(File, _),
  halt.

main :- halt(1).