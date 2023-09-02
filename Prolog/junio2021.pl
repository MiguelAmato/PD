aplana([],[]):-!.
aplana([[]|L],LA) :-!, aplana(L,LA).
aplana([[X|Xs]|L],LA) :-!, append([X|Xs],L,L1), aplana(L1,LA).
aplana([X|L],[X|LA]) :- atomic(X),aplana(L,LA).