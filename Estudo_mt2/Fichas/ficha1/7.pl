a(a1, 1).
a(A2, 2).
a(a3, N).
b(1, b1).
b(2, B2).
b(N, b3).
c(X, Y):- a(X, Z), b(Z, Y).
d(X, Y):- a(X, Z), b(Y, Z).
d(X, Y):- a(Z, X), b(Z, Y).

% b

% i.    yes.
% ii.   A = 2.
% iii.  A = A1.
% iv.   A = a1, B = b1.
% v.    A = a1, B = 2.  