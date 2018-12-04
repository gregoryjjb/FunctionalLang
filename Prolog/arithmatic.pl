add(X, zero, X).
add(X, inc(Y), Z) :- add(X, Y, W), Z = inc(W).

mul(_, zero, zero).
mul(X, inc(Y), Z) :- mul(X, Y, W), add(W, X, Z).

expo(_, zero, inc(zero)).
expo(X, inc(Y), Z) :- expo(X, Y, W), mul(X, W, Z).

less(zero, inc(_)).
less(inc(X), inc(Y)) :- less(X, Y).

%eval(E, V) :- 