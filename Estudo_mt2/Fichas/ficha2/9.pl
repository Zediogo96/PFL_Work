% a

counter([], _, 0).
counter([Elem | T], Elem, Count) :- counter(T, Elem, Count1), Count is Count1 + 1.
counter([_H | T], Elem, Count) :- counter(T, Elem, Count).

rle([], []).
rle([H | T], [H-Count | T1]) :-
    counter([H | T], H, Count),
    remove([H | T], H, T2),
    rle(T2, T1).

remove([], _, []).
remove([H | T], H, T1) :- remove(T, H, T1).
remove([H | T], Elem, [H | T1]) :- remove(T, Elem, T1).

% b

un_rle([], []).
un_rle([H-N | T], List2) :-
    create_list(H, N, Temp),
    un_rle(T, R1),
    append(Temp, R1, List2).

create_list(Elem, 1, [Elem]).
create_list(Elem, Count, R) :-
    Next is Count - 1,
    create_list(Elem, Next, R1),
    append([Elem], R1, R).


