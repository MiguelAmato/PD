rama(void, []) :- !.
rama(arbol(X, void, void), [X]) :- !.
rama(arbol(X, I, void), C) :- !,
    rama(I, C1),
    C=[X|C1].
rama(arbol(X, void, D), C) :- !,
    rama(D, C1),
    C=[X|C1].
rama(arbol(X, D, _), C) :-
    rama(D, C1),
    C=[X|C1].
rama(arbol(X, _, I), C) :-
    rama(I, C1),
    C=[X|C1].

ramas(void, []) :- !.
ramas(A, L) :-
    findall(Ar, rama(A, Ar), L).

