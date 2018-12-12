matches(X, X).
matches(X, union(A, _)) :- matches(X, A).
matches(X, union(_, B)) :- matches(X, B).
matches(X, concat(A, B)) :- 
   string_concat(Pre, Suf, X), matches(Pre, A), matches(Suf, B).
   
matches(_, opt(_)).

matches("", rep(_)).
matches(X, rep(A)) :- string_concat(Pre, Suf, X), matches(Pre, A), matches(Suf, rep(A)).