% a
is_ordered([_]).
is_ordered([]).
is_ordered([F,S | T]) :- F =< S, is_ordered([S | T]).

% b

insert_ordered(Val, L1, L2) :-
    append(L3, L4, L1),
    append(L3, [Val | L4], L2),
    is_ordered(L2).

% c

insert_sort([], []).
insert_sort([H | T], R) :-
    insert_sort(T, R1),
    insert_ordered(H, R1, R).