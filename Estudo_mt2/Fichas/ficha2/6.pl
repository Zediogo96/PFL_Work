% a
invert([], []).
invert([H | T], R) :-
    invert(T, R1),
    append(R1, [H], R).


% b 

del_one(Elem, [Elem | T], R) :-
    R = T.

del_one(Elem, [H | T], R) :-
    del_one(Elem, T, R1),
    R = [H | R1].

% c

del_all(_, [], []).

del_all(Elem, [Elem | T], R) :-
    del_all(Elem, T, R).

del_all(Elem, [H | T], R) :-
    del_all(Elem, T, R1),
    R = [H | R1].

% d ----------------------------------

del_all_list(_, [], []).

del_all_list(Elemts, [H | T], R) :- 
    ((member(H, Elemts)) -> 

        del_all_list(Elemts, T, R)
        ;
        del_all_list(Elemts, T, R1),
        R = [H | R1]
     ).

% ou

% del_all_list(_, [], []).
% del_all_list([], L, L).
% del_all_list([H | T], L, R) :-
%     del_all(H, L, L1),
%     del_all_list(T, L1, R).


% e ----------------------------------

del_dups([], []).
del_dups([HD | TD], R) :- 
    ((member(HD, TD)) ->
        del_dups(TD, R)
        ;
        del_dups(TD, R1),
        R = [HD | R1]
    ).

% f -----------------------------------

list_perm([], []).
list_perm([H1 | T1], L2) :-
    del_one(H1, L2, DelL2),
    L2 \= DelL2,
    list_perm(T1, DelL2).

% g -----------------------------------

replicate(0, Elem, [Elem]).
replicate(Amount, Elem, List1) :- 
    Next is Amount - 1,
    replicate(Next, Elem, R1),
    List1 = [Elem | R1].

% h -----------------------------------

intersperse(_, [], []).
intersperse(_, [H | []], [H]).
intersperse(Elem, [H | T], [H, Elem | R]) :-
    intersperse(Elem, T, R).

% i -----------------------------------

% insert element at index
insert_elem(0, List, E, [E | List]).
insert_elem(I, [H | T], E, R) :-
    Next is I - 1,
    insert_elem(Next, T, E, R1),
    R = [H | R1].

% j -----------------------------------

delete_elem(0, [_ | T], _, T).
delete_elem(I, [H | T], E, R) :-
    Next is I - 1,
    delete_elem(Next, T, E, R1),
    R = [H | R1].

% k -----------------------------------

replace([Old | T], 0, Old, New, [New | T]).
replace([H | T], Index, Old, New, List) :-
    Next is Index - 1,
    replace(T, Next, Old, New, R1),
    List = [H | R1].



