% a 

list_size([], 0).
list_size([_ | T], Size) :- 
    list_size(T, TSize),
    Size is TSize + 1.

% b

list_sum([], 0).
list_sum([H | T], Acc) :- 
    list_sum(T, Acc1),
    Acc is Acc1 + H.

% c

list_prod([], 1).
list_prod([H | T], Acc) :-
    list_prod(T, Acc1),
    Acc is Acc1 * H.

% d

inner_product([], [], 0).
inner_product([H1 | T1], [H2 | T2], Acc) :-
    length([H1 | T1], S1), length([H2 | T2], S2), S1 =\= S2, fail;
    inner_product(T1, T2, Acc1),
    Acc is Acc1 + (H1 * H2).