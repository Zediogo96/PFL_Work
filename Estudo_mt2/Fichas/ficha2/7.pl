% a

list_append(L1, L2, R) :- append(L1, L2, R).

% b

list_member(E, L) :- append(_, [E | _], L), !.

% c

list_last(List, Last) :- append(_, [Last | []], List).

% d

list_nth(N, List, Elem) :- append(_D, [Elem | _], List), length(_D, T), T = N.

% e

list_append([], []).
list_append([H | T], Result) :- 
        list_append(T, R),
        append(H, R, Result).

% f 

list_del(List, Elem, Res) :- 
    append(Temp1, [Elem | Temp2], List),
    append(Temp1, Temp2, Res).

% g

list_before(First, Second, List) :- 
    append(T1, [First | _], List), append(T2, [Second| _], List), length(T1, Length1), length(T2, Length2), (Length1 < Length2).

% above was unecessary 

%  PRAISE THE SUN \O/ PRAISE THE SUN
%                 ||
%                / \  
% |||||||||||||||||||||||||||||||||||||||

list_before_opt(First, Second, List) :-
    append(_, [First | Rest], List), append(_, [Second | _], Rest).

% h

list_replace_one(X,Y, List, R) :-
    append(Start, [X | Rest], List), append(Start, [Y | Rest], R).

% i

list_repeated(X, List) :-
    append(_, [X | Rest], List), append(_, [X | _], Rest).

% j

list_slice(L, I, Size, R) :-
    append(Start, SliceStart, L), length(Start, I), append(Slice, _, SliceStart), length(Slice, Size), R = Slice.

% k

list_shift_rotate(L1, N, List2) :-
    append(Start, Rest, L1), length(Start, N), append(Rest, Start, List2).

% SOLUÇÕES LATE NIGHT PATRIOCIONADAS POR ESTA PLAYLIST: https://open.spotify.com/playlist/37i9dQZF1DX6rsDrBNGuWW?si=96e1ecbb359e4955