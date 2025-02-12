member(X, cons(X, _)).
member(X, cons(_, Rest)) :- member(X, Rest).
