b(X) :-
    Y = (write(X), X),
    call(Y).

a(1).
a(2).

:- initialization(call(!)).

:- initialization(call(fail)).

:- initialization((fail,X)).

:- initialization(call( (fail, call(1)) )).

:- initialization(b(_)).

:- initialization(b(3)).

:- initialization(( Z = !, call( (Z=!, a(X), Z) )) ).

:- initialization(call( (Z=!, a(X), Z) )).

:- initialization(call((write(3),X))).

:- initialization(call((write(3),call(1)))).

:- initialization(call(X)).

:- initialization(call(1)).

:- initialization(call((fail,1))).

:- initialization(call((write(3),1))).

:- initialization(call((1;true))).