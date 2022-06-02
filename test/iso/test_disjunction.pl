:- initialization((';'(true,fail))).

:- initialization((';'((!, fail), true))).

:- initialization((';'(!, call(3)))).

:- initialization((';'((X = 1, !), X = 2))).

:- initialization((','(';'(X=1, X=2), ';'(true, !)))).
