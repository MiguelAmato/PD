% Hoja 7

% 1

sumintersec([],[],0).
sumintersec([],[_|_],0).
sumintersec([_|_],[],0).
sumintersec([X|Xs],[X|Ys],N):- sumintersec(Xs,Ys,N1), N is N1+X.
sumintersec([X|Xs],[Y|Ys],N):- (X<Y->sumintersec(Xs,[Y|Ys],N);sumintersec([X|Xs],Ys,N)).

% 2

nomiembro(_,[]).
nomiembro(X, [Y|Ys]):- X \= Y, nomiembro(X, Ys).

hazconjunto([],[]).
hazconjunto([X|Xs],Ys):- nomiembro(X,Ys), hazconjunto(Xs,[X|Ys]).
hazconjunto([X|Xs],[X|Ys]):- hazconjunto(Xs,[X|Ys]).

union([],[],[]).
union([A|C1s],[A|C2s],C):- union(C1s,C2s,[A|C]).
union([C1|C1s],[C2|C2s],C):- (nomiembro(C1,C) -> (nomiembro(C2,C) -> union(C1s, C2s, [C1,C2|C]); union(C1s, C2s, [C1|C])); (nomiembro(C2, C) -> union(C1s, C2s, [C2|C]) ; union(C1s, C2s, C))).

interseccion([],[],[]).
interseccion([_|_], [], []).
interseccion([], [_|_], []).
interseccion([A|C1s],[A|C2s],C):- interseccion(C1s,C2s,[A|C]).
interseccion([C1|C1s],[C2|C2s],C):- (nomiembro(C1, C2s) -> interseccion(C1s,[C2|C2s],C) ; interseccion(C1s,[C2|C2s],[C1|C])).

