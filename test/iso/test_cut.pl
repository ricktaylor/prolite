twice(!) :- write('C ').
twice(true) :- write('Moss ').

goal((twice(_),!)).
goal(write('Three ')).

:- initialization(!).

:- initialization((!, fail; true)).

:- initialization((call(!), fail; true)).

:- initialization((twice(_), !, write('Forwards '), fail)).

:- initialization(((! ; write('No ')), write('Cut disjunction '), fail)).

:- initialization((twice(_), (write('No '); !), write('Cut '), fail)).

:- initialization((twice(_), (!, fail; write('No ')))).

:- initialization((twice(X), call(X), write('Forwards '), fail)).

:- initialization((goal(X), call(X), write('Forwards '), fail)).

:- initialization((twice(_), \+(\+(!)), write('Forwards '), fail)).

:- initialization((twice(_), once(!), write('Forwards '), fail)).

:- initialization((twice(_), call(!), write('Forwards '), fail)).
