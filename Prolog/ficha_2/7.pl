% a 
list_append([],L, L). % if the first element is empty, we will put L2 in the result
list_append([Head|Tail], List2, [Head|List]):- % else -> concat H + List (Result) and then make recursive functions
     list_append(Tail, List2, List).

% b

list_member(Elem, List) :- append(_Part1, [Elem | _Part2], List).

% c 

list_last(List, Last) :- append(_ExceptLast, [Last | []], List).

% d

list_nth(N, List, Elem) :- append(Part1, [Elem|_Part2], List), length(Part1, N).

% e 

list_append2([L1 | []], List) :- L1 = List.
list_append2([L1 | LN], List) :- list_append(L1, Lrem, List), list_append2(LN, Lrem).

% f 

list_del(List, Elem, Res) :- append(Part1, [Elem | Part2], List),
                             append(Part1, Part2, Res).

% g

list_before(First, Second, List) :- append(_Part1, [First | Rem], List),
                                    append(_Part2, [Second | _Part3], Rem).

% h

list_replace_one(X, Y, L1, L2) :- append(Part1, [X | Rem], L1), 
                                  append(Part1, [Y | Rem], L2).

% i

list_repeated(X, List) :- append(_Part1, [X | Rem], List),
                          append(_Part2, [X | _Part3], Rem).

% j

list_slice(L1, Idx, Size, L2) :- append(Part1, Part2, L1), length(Part1, Idx), append(L2, _Rem, Part2), length(L2, Size). 

% k

list_shift_rotate(L1, N, L2) :- append(Pt1, Rem, L1), length(Pt1, N), append(Rem, Pt1, L2). 

