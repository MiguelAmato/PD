sumatree(void,0).
sumatree(a(X,L,R),N):- sumatree(L,N1), sumatree(R,N2), N is N1+N2+X.

maximo(void,0).
maximo(a(X,L,R),N):- maximo(L,N1), maximo(R,N2), N is max(X,max(N1,N2)).

sublistasAux([],[]).
sublistasAux([X|L],[X|L1]):- sublistasAux(L,L1).
sublistasAux([_|L],L1):- sublistasAux(L,L1).

sublistas(L1,L2):- setof(X,sublistasAux(L1,X),L2).

rama(void,[]).
rama(a(X,L,R),[X|L1]):- rama(L,L1).
rama(a(X,L,R),[X|L1]):- rama(R,L1).

