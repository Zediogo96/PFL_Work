% B %%%%%%%%%%%%%%%%
factorial(0, 1) :- !.
factorial(N, F) :-
    N > 0,
    N1 is N - 1,
    factorial(N1, F1),
    F is N * F1.
% factorial(10, F), nl, write(F), halt -> halt manda swipl a baixo

% B %%%%%%%%%%%%%%%%
somaRec(0, 0).
somaRec(N, Sum) :-
    N > 0,
    N1 is N - 1,
    somaRec(N1, Sum1),
    Sum is N + Sum1.
    
% C %%%%%%%%%%%%%%%%
fibonacci(0, 0).
fibonacci(1, 1).
fibonacci(N,F) :- 
    N >= 0,
    N1 is N - 1,
    N2 is N - 2,
    fibonacci(N1, F1),
    fibonacci(N2, F2),
    F is F1 + F2.

% D %%%%%%%%%%%%%%%%
divisible(X, Div) :- Div < X,
                        0 is X rem Div.
divisible(X, Div) :- Div < X,
                        NewDiv is Div + 1,
                        divisible(X, NewDiv).

isPrime(X) :- X >= 2,
              Div is 2,
              \+ divisible(X, Div).











    