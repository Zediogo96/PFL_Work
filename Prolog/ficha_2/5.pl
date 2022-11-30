% a 
list_size([], 0).
list_size([_|T], S) :- 
    list_size(T, ST),
    S is ST + 1.

% b 
list_sum([], 0).
list_sum([H | T], Acc) :- 
    list_sum(T, NewAcc),
    Acc is NewAcc + H.

% c 
list_prod([], 1).
list_prod([H | T], Acc) :-
    list_prod(T, NewAcc),
    Acc is NewAcc * H.

%d -> diz no ex que têm o mm tamanho
inner_product([], [], 1).
inner_product([H1 | T1], [H2 | T2], Acc) :-
    inner_product(T1, T2, NewAcc),
    Acc is NewAcc + (H1 * H2).

%e

count(_, [], 0).
count(E, [H | T], C) :- H =\= E, count(E,T,C).
count(E, [H | T], C) :- H = E, count(E,T, NewC), C is NewC + 1.



