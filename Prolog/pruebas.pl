hombre(pedro).
hombre(juan).
hombre(luis).
hombre(hector).
hombre(jose).
hombre(ramon).
hombre(ricardo).
hombre(alejandro).
hombre(jorge).
hombre(fernando).
hombre(arturo).

mujer(maria).
mujer(ana).
mujer(laura).
mujer(rosa).

progenitor(pedro,luis) :- !.
progenitor(pedro,juan).
progenitor(ricardo, jose).
progenitor(juan, hector).

padre(X,Y):-hombre(X),progenitor(X,Y).
madre(X,Y):-mujer(X),progenitor(X,Y).
abuelo(X,Y):-hombre(X),progenitor(X,Z),progenitor(Z,Y).
abuela(X,Y):-mujer(X),progenitor(X,Z),progenitor(Z,Y).
tia(X,Y):-mujer(X),progenitor(Z,Y),progenitor(Z,X).
hermano(X,Y):-hombre(X),progenitor(Z,X),progenitor(Z,Y).
tatara(X,Y):-hombre(X),progenitor(X,Z),progenitor(Z,W),progenitor(W,Y).
tataratatara(X,Y):-hombre(X),progenitor(X,Z),progenitor(Z,W),progenitor(W,T),progenitor(T,Y).
tataratataratatara(X,Y):-hombre(X),progenitor(X,Z),progenitor(Z,W),progenitor(W,T),progenitor(T,R),progenitor(R,Y).