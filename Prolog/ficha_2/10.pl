% a 

is_ordered([_]) :- !.
is_ordered([H,S|T]) :- H =< S,
                       is_ordered([S|T]).

% b
insert_ordered(Value, [], [Value]). % é o maior elemento
insert_ordered(Value, [H | T], [Value, H | T ]) :- Value =< H.
insert_ordered(Value, [H | T], [H | T2]) :- 
    Value > H,
    insert_ordered(Value, T, T2).

% c 

insert_sort([], []).
insert_sort([H | T], OrderedList) :- 
    insert_sort(T, OrderedTail),
    insert_ordered(H, OrderedTail, OrderedList).

