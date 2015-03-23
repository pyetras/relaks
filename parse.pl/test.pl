assign(Var1, Var2) :- Var2 >:< Var1.
assign_choice(Var1, Var2) :- Var1.type = list(T), !, member(X, Var1.value), Var2 = var{value: X, type: choice(T)}.
%assign choice wykonuje tylko jedno przypisanie - przynajmniej dopóki nie zrobimy nawrotu

val(Var1, X) :- assign_choice(Var1, Var2), Var2.value = X.

search(Var1, Choices) :- bagof(X, val(Var1, X), Choices).
search2(Var2) :- X = Var2.value, format("X = ~p", [X]), nl, fail.
search3(Var2, Choices) :- findall(X, get_dict(value, Var2, X), Choices).

test2(Var) :- member(X, [1, 2]), asserta(value_of(Var, X)).
search4(Var2, Choices) :- findall(X, value_of(Var2, X), Choices).

:- use_module(library(clpfd)).
ass(X, Y) :- X in 1..5, Y in 2..5, ((X #< 2) #==> Y in 2..3), ((X #>= 2) #==> Y #= 5).
search5(Vars, Choices) :- findall(Vars, label(Vars), Choices).

