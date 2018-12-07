%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Gregory Brisebois
% CS152 Prolog Assignment

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 1. Simple Knowledge Base

student(smith).
student(jones).
student(nguyen).
student(young).
student(sherman).

instructor(pearce).
instructor(godel).
instructor(escher).
instructor(bach).

taken(smith, cs46a, pass).
taken(smith, cs46b, fail).
taken(smith, cs146, pass).
taken(jones, cs46a, pass).
taken(jones, cs46b, pass).
taken(jones, cs146, pass).
taken(nguyen, cs46a, pass).
taken(nguyen, cs46b, pass).
taken(nguyen, cs151, pass).

teaches(pearce, cs152).
teaches(godel, cs151).
teaches(escher, cs146).
teaches(bach, cs46b).
teaches(bach, cs46a).

prerequisite(cs152, cs151).
prerequisite(cs152, cs46b).
prerequisite(cs151, cs46b).
prerequisite(cs146, cs46b).
prerequisite(cs46b, cs46a).

academic(X) :- student(X); instructor(X).

pass(S, C) :- taken(S, C, pass).

studentOf(S, I) :- taken(S, C, _), teaches(I, C).

lowerDiv(cs46a).
lowerDiv(cs46b).

upperDivisionCsStudent(S) :- foreach(lowerDiv(L), taken(S, L, pass)).
	
teachesPrereq(I) :- teaches(I, P), prerequisite(_, P).

canTake(S, C) :- foreach(prerequisite(C, P), taken(S, P, pass)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 3. Arithmetic

add(X, zero, X).
add(X, inc(Y), Z) :- add(X, Y, W), Z = inc(W).

mul(_, zero, zero).
mul(X, inc(Y), Z) :- mul(X, Y, W), add(W, X, Z).

expo(_, zero, inc(zero)).
expo(X, inc(Y), Z) :- expo(X, Y, W), mul(X, W, Z).

less(zero, inc(_)).
less(inc(X), inc(Y)) :- less(X, Y).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 5. Evaluating Expressions

eval(num(X), Y):- Y is X.
eval(sum(X, Y), Z):- eval(X, A), eval(Y, B), Z is A + B.
eval(prod(X, Y), Z):- eval(X, A), eval(Y, B), Z is A * B.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 8. UML Class Diagrams
%    (Simplified)

implements(expression, funcall).
implements(expression, identifier).
implements(specialForm, conditional).
implements(literal, boole).

extends(expression, specialForm).
extends(expression, literal).
extends(value, literal).

has(conditional, condition, expression).
has(conditional, consequent, expression).
has(conditional, alternative, expression).
has(funcall, operator, identifier).
has(funcall, operands, expression).

depends(C, C).
depends(C, P) :- (extends(Z, C), depends(Z, P));
                 (implements(Z, C), depends(Z, P));
                 (has(C, _, Z), depends(Z, P)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 9. State Machine

%%%% Problem 1

identifier(state0, X, state1) :- is_alpha(X).
identifier(state0, X, dead) :- not(is_alpha(X)).

identifier(state1, X, state1) :- is_alpha(X); is_digit(X).
identifier(state1, X, dead) :- not(is_alpha(X)), not(is_digit(X)).

identifier(dead, _, dead).

%%%% Problem 2

moves(S, [], S).
moves(S, [H | T], F) :- identifier(S, H, I), moves(I, T, F).

%%%% Problem 3

accept(S) :- string_chars(S, L), moves(state0, L, state1).


