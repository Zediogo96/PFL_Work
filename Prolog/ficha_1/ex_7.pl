a(a1, 1).
a(A2, 2).
a(a3, N).
b(1, b1).
b(2, B2).
b(N, b3).
c(X, Y):- a(X, Z), b(Z, Y).
d(X, Y):- a(X, Z), b(Y, Z).
d(X, Y):- a(Z, X), b(Z, Y).


/**
% b
i. A = a3; (A2 dá warning pq é singleton??)
ii. A = 2
iii. c (A,b3) -> a (A, Z) , b (Z, b3) -> A = a1.
iv. c(A,B) -> a (A, Z) , b (Z, B) -> A = a1, B = b1; A = a1, B = b3.
v. d (A,B) -> 
    | a(A,Z), b(B,Z) -> A = a1, B = b1; A = a1, B = b3; A = a3, B = b1; A = a3, B = b3.

% c
% PARA CORRER "MODO DEBUGGING", correr "trace." antes de correr o query. no SWIPL.