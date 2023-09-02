nat(c).
nat(s(X)) :- nat(X).

sum(c, X, X).
sum(X, c, X).
sum(s(X), s(Y), Z) :- Z = s(s(Z1)), sum(X, Y, Z1).

prod(c, _, c).
prod(_, c, c).
prod(s(X), s(Y), Z) :- prod(X, s(Y), Z1), sum(Z1, s(Y), Z).

pot(_, c, s(c)).
pot(X, s(Y), Z) :- pot(X, Y, Z1), prod(X, Z1, Z).

fact(c, s(c)). 
fact(s(X), Z) :- fact(X, Z1), prod(s(X), Z1, Z).

fib(c, _, c).
fib(s(c), _, s(c)).
fib(s(s(N)), X, Z) :- fib(N, X, Z1), fib(s(N), X, Z2), sum(Z1, Z2, Z).

p(a). p(b). q(a). q(X) :- r(X). r(a).

