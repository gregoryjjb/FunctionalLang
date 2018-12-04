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

% Lab 1

academic(X) :- student(X); instructor(X).

pass(S, C) :- taken(S, C, pass).

studentOf(S, I) :- taken(S, C, _), teaches(I, C).

lowerDiv(cs46a).
lowerDiv(cs46b).

upperDivisionCsStudent(S) :- foreach(lowerDiv(L), taken(S, L, pass)).
	
teachesPrereq(I) :- teaches(I, P), prerequisite(_, P).

canTake(S, C) :- foreach(prerequisite(C, P), taken(S, P, pass)).











