% 1

% a

fatorial(0,1).
fatorial(N,F) :- 
    NewN is N - 1,
    fatorial(NewN, F1),
    F is F1 * N.

% b 

somaRec(0,0).
somaRec(N, Sum) :-
    NewN is N - 1,
    somaRec(NewN, Sum1),
    Sum is Sum1 + N.

% c

fibonacci(0,0).
fibonacci(1,1).
fibonacci(N, Fib) :-
    N1 is N - 1,
    N2 is N - 2,
    fibonacci(N1, Fib1),
    fibonacci(N2, Fib2),
    Fib is Fib1 + Fib2.

% d

isPrime(X) :- 
    NX is X - 1,
    isPrime(X,NX).

isPrime(_,1) :- !.
isPrime(X,Y) :-
    (X mod Y) =\= 0,
    NY is Y - 1,
    isPrime(X, NY).



