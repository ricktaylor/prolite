:- initialization((','(X=1, var(X)))).

:- initialization((','(var(X), X=1))).

:- initialization((','(X = true, call(X)))).
