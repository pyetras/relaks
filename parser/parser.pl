:- use_module(grammar).
:- use_module(library(list_util), [take/3]).
:- use_module(library(lambda)).

parse(File, AST) :-
  open(File, read, Stream),
  read_stream_to_codes(Stream, Str),
  close(Stream),
  append(Str, "\n", Strnl),
  program(AST, Strnl, Rest),
  (
    Rest = [], !, transform(AST, CleanAST), format("~p", [CleanAST]), nl
    ;
    append(Parsed, Rest, Strnl), count_nl(Parsed, N), NN is N + 1,
      length(Rest, Len), LL is min(20, Len), take(Rest, LL, Error), string_codes(ErrStr, Error),
      format("Error at line ~d: ~p...", [NN, ErrStr]), nl, fail
  ).

transform(AST, AST1) :- remove_nils(AST, AST1).

remove_nils(Stmts, Stmts1) :- exclude(X^(X = nil), Stmts, Stmts1).

count_nl([0'\n|T], N) :- !, count_nl(T, M), N is M + 1.
count_nl([_|T], N) :- count_nl(T, N).
count_nl([], 0).