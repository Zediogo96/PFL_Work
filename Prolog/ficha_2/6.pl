% a 
invert(L1, L2) :- invert(L1,[], L2). % paradigm in prolog where we "create" a new function (same name, diff param) that does all the work
invert([], ACC, ACC). % if the list is empty, accumulator contains the reversed list
invert([H|T], ACC, L2) :- invert(T, [H|ACC], L2). % else continue reversing it

% b 

del_one(_,[],[]) :- !.
del_one(E, [E|T], T).
del_one(E, [H|T], [H,L]) :- % always insert here the result of the bellow operations
    E \= H,
    del_one(E,T,L).

% c

del_all(_, [], []).
del_all(E, [E|T], R) :-
    del_all(E, T, R).
del_all(E, [H|T], [H|T1]) :- 
    (E \= H),
    del_all(E, T, T1).

% d
del_all_list([],[],[]).
del_all_list([], R, R).
del_all_list([H|T], L, R1) :- 
    del_all(H, L, R),
    del_all_list(T, R, R1).

% e 

del_dups([],[]).
del_dups(L1, R) :- del_dups(L1, [], R).

del_dups([], R, R).
del_dups([H|T], Dist, R) :-
    (
        member(H, Dist),
        del_dups(T, Dist, R)
    ;
        \+ member(H, Dist),
        del_dups(T, [H|Dist], R)
    ).

% f ?????????

% g 

replicate(Amount, Elem, R) :- replicate(Amount, Elem, [], R).

replicate(0, _, R, R).
replicate(Amount, Elem, Acc, R) :- 
    New_Amount is (Amount - 1),
    replicate(New_Amount, Elem, [Elem|Acc], R).

% h

intersperse(Elem, L1, L2) :- intersperse(Elem, L1, [], L2).

intersperse(_, [], R, R).
intersperse(Elem, [H|T], Acc, R) :- 
    intersperse(Elem, T, [H , Elem | Acc], R).

% i
insert_elem(0, [H1 | T1], E, [E, H1 | T1]).
insert_elem(I, [H1 | T1], E, [H2 | T2]) :- 
                                           I > 0,
                                           H1 = H2,
                                           NewI is I - 1,
                                           insert_elem(NewI, T1, E, T2).

% j
remove_elem(0, [E|T1], E, T1).
remove_elem(I, [H1|T1], E, [H2|T2]) :-
    I > 0,
    H1 = H2,
    NewI is I - 1,
    remove_elem(NewI, T1, E, T2).

% k 
replace([H1|T1], 0, Old, New, [H2|T2]) :- Old = H1,
                                          New = H2, 
                                          T1 = T2.                  

replace([H1|T1], I, Old, New, [H2|T2]) :- I > 0,
                                          H1 = H2,
                                          NewI is I - 1,
                                          replace(T1, NewI, Old, New, T2).






