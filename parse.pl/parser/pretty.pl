:- portray_text(true).

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

user:portray(statement(block, (Name, (Ins, Outs), Contracts, Stmts))) :-
  format("λ~p(→~p, ~p→) where ~p", [Name, Ins, Outs, Contracts]), 
  nl,
  print_list(Stmts, print_stmt).
  
print_stmt(L) :- format("  ~p", [L]).
print_list([], _).
print_list([H|T], Iter) :- Clause =.. [Iter,H], call(Clause), nl, print_list(T, Iter).
  