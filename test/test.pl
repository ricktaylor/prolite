fib(0, 1) :- !.
fib(1, 1) :- !.
fib(N, Result) :-
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fib(N1, Result1),
    fib(N2, Result2),
    Result is Result1 + Result2.

:- initialization((fib(5,X),write(X))).

/*tiny(0) :- !.
tiny(X) :- X is X - 1.*/
