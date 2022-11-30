% a 

%% Solution w/o Append (using a worker) %%%
list_to(N, List) :- list_to(N, [], List).

list_to(0, List, List).
list_to(N, Acc, List) :- 
    N > 0,
    NewN is N - 1,
    list_to(NewN, [N | Acc], List).

%% Solution w Append %%%

list_to2(0, []).
list_to2(N, List) :-
    NewN is N - 1,
    list_to2(NewN, Front),
    append(Front,[N], List).

% b

list_from_to(Lim,Lim, [Lim]).
list_from_to(Inf, Sup, List) :-
    Inf < Sup,
    NewInf is Inf + 1, 
    list_from_to(NewInf, Sup, Back),
    append([Inf], Back, List).

% c

list_from_step(Inf, _ , Sup, []) :- Inf > Sup.
list_from_step(Lim, _, Lim, [Lim]).
list_from_step(Inf, Step, Sup, List) :-
    Inf < Sup,
    NewInf is Inf + Step,
    list_from_step(NewInf, Step, Sup, Back),
    append([Inf], Back, List).


% e 

%% From Ex 1 %%%%
divisible(X, Div) :- Div < X,
                        0 is X rem Div.
divisible(X, Div) :- Div < X,
                        NewDiv is Div + 1,
                        divisible(X, NewDiv).

isPrime(X) :- X >= 2,
              Div is 2,
              \+ divisible(X, Div).
%%% 

primes(1,[]).
primes(N, List) :- N >= 2,
                   not(isPrime(N)),
                   NewN is N - 1,
                   primes(NewN, List).
primes(N, List) :- N >= 2,
                   isPrime(N),
                   NewN is N - 1,
                   primes(NewN, Front),
                   append(Front, [N], List).

