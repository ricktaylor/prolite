:- initialization(('->'(true, true))).

:- initialization(('->'(true, fail))).

:- initialization(('->'(fail, true))).

:- initialization(('->'(true, X=1))).

:- initialization(('->'(';'(X=1, X=2), true))).

:- initialization(('->'(true, ';'(X=1, X=2)))).
