:- use_module(library(dcg/basics)).
:- use_module(library(apply)).

latinkws(["foreach", "filter", "limit", "join", "distinct", "order", "by", "sample",
  "asc", "as", "desc", "load", "store", "and", "or", "search", "generate", "null", "not"]).
cmps(["==", "!=", "<=", ">=", "<", ">"]).
maths(["^", "/", "*", "%", "+", "-"]).
%% bools(["||", "&&"]).

eol --> whites, [C], { [C] = "\n", !; [C] = ";" }, !, whites, rest_eol.
%% eol --> whites, eos.
rest_eol --> [C], { [C] = "\n", !; [C] = ";" }, !, whites, rest_eol.
rest_eol --> [].

wordsep --> blank, !.
wordsep, [C] --> [C], { code_type(C, punct) }, !.
wordsep --> eos.

:- dynamic(init_lexer/0).

init_lexer :-
  asserta((init_lexer :- !)),
  latinkws(Keywords),
  maplist(make_keyword, Keywords),
  cmps(Cmps),
  maplist(make_op(cmp_op), Cmps),
  %% bools(Bools),
  %% maplist(make_op(bool_op), Bools),
  maths(Maths),
  maplist(make_op(math_op), Maths).

make_keyword(Str) :-
  atom_codes(KW, Str),
  Term =.. [KW, List, Rest],
  assertz((
    Term :- append(Str, Rest1, List), wordsep(Rest1, Rest)
  )).

make_op(Name, Str) :-
  atom_codes(Op, Str),
  Term =.. [Name, Op, List, Rest],
  assertz((Term :- append(Str, Rest, List), !)).

reserved(["def", "true", "false", "do", "end", "and", "or", "null"]).

varname([H|T]) -->
  [H], { is_init_char(H) }, varname_cont(T),
  { reserved(Reserved), \+ member([H|T], Reserved) }.
varname_cont([H|T]) --> [H], { code_type(H, prolog_identifier_continue)}, !, varname_cont(T).
varname_cont([]) --> [].

is_init_char(X) :- is_letter(X), !; X = 0'_, !; X = 0'$, !; X = 0'@, !.
is_letter(X) :- code_type(X, alpha).

dot --> ".".

dots --> "..".

comma --> ",".

eq_op --> "=".

star --> "*".

colon --> ":".

%% semicolon --> ";".

lpar --> "(".
rpar --> ")".

lsq --> "[".
rsq --> "]".

tru --> "true", wordsep.
fals --> "false", wordsep.

begin --> "do", wordsep.
end --> "end", wordsep.

bigpipe --> "|>".

def --> "def", wordsep.

string_lit(Str) --> [L], { code_type(L, quote) }, string_without([L, 0'\n], Str), [L].

where --> "where", wordsep.

in --> "in", wordsep.
out --> "out", wordsep.

lbrace --> "{".
rbrace --> "}".

larrow --> "<-".
rarrow --> "->".

ass --> "as", wordsep.

dblcolon --> "::".

pipe --> "||".

timespans([(ns, 0.1), (ms, 1), (s, 1000), (min, 60000), (m, 60000), (hrs, 3600000), (h, 3600000)]).

timespan(Mul, Text, Rest) :-
  timespans(Timespans),
  member((Unit, Mul), Timespans),
  atom_codes(Unit, Str),
  append(Str, Rest, Text), !.
timespan(Mul, Last, Text, Rest) :-
  timespan(Mul, Text, Rest),
  Last > Mul.

num(X) -->
  number(N),
  whites, timespan(Mul), !,
  num_ts_rest(Rest, Mul),
  {sgn(N, S), X is N*Mul + S*Rest}. %no tail rec
num(X) --> number(X).

num_ts_rest(X, Last) -->
  whites, number(N), { N >= 0 }, whites, timespan(Mul, Last), !, num_ts_rest(Rest, Mul), {X is N*Mul + Rest}.
num_ts_rest(0, _) -->  [].

sgn(X, S) :- X >= 0 -> S = 1 ; S = -1.

word(X) --> string(X), [Space], { \+ code_type(Space, alpha) }.

:- init_lexer.

