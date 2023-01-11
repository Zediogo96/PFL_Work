% a

list_to(0, []).
list_to(N, List) :-
    Next is N - 1,
    list_to(Next, R),
    append(R, [N], List).

% b

list_from_to(Sup, Sup, [Sup]).
list_from_to(Inf, Sup, List) :-
    Next is Sup - 1,
    list_from_to(Inf, Next, R),
    append(R, [Sup], List).

% c

list_from_to_step(Inf, _, Sup, []) :- 
    Inf > Sup, !.

% d

list_from_to_step(Inf, Step, Sup, [Inf | T]) :-
    Next is Inf + Step, 
    Step =< Sup,
    list_from_to_step(Next, Step, Sup, T).

% e

isPrime(X) :- 
    NX is X - 1,
    isPrime(X,NX).

isPrime(_,1) :- !.
isPrime(X,Y) :-
    (X mod Y) =\= 0,
    NY is Y - 1,
    isPrime(X, NY).

primes(1, []) :- !.
primes(N, L) :-
    Next is N - 1,
    primes(Next, Start),
    (
        isPrime(N), append(Start, [N], L), !;
        L = Start, !

    ).

% f 

fibonacci(0,0).
fibonacci(1,1).
fibonacci(N, Fib) :-
    N1 is N - 1,
    N2 is N - 2,
    fibonacci(N1, Fib1),
    fibonacci(N2, Fib2),
    Fib is Fib1 + Fib2.

fibs(0, [0]).
fibs(N, List) :- 
    Next is N - 1,
    fibs(Next, R),
    fibonacci(N, NF),
    append(R, [NF], List).

