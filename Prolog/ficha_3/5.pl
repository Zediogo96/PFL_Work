parent(grace, phil).
parent(frank, phil).

parent(dede, claire).
parent(jay, claire).

parent(dede, mitchell).
parent(jay, mitchell).

parent(jay, joe).
parent(gloria, joe).

parent(javier, manny).
parent(gloria, manny).

parent(barb, cameron).
parent(merle, cameron).

parent(barb, pameron).
parent(merle, pameron).

parent(phil, haley).
parent(claire, haley).

parent(phil, alex).
parent(claire, alex).

parent(phil, luke).
parent(claire, luke).

parent(mitchell, lily).
parent(cameron, lily).

parent(mitchell, rexford).
parent(cameron, rexford).

parent(pameron, calhoun).
parent(bo, calhoun).

parent(dylan, george).
parent(haley, george).

parent(dylan, poppy).
parent(haley, poppy).

% a 
children(Person, Children) :- findall(Child, parent(Person, Child), Children).

% b

children_of([], []).
children_of([Person | People], [Person-Children | Rest]) :- children(Person, Children), nl, children_of(People, Rest).

% d

couple(P1-P2) :- parent(P1, X), parent(P2, X), P1 \= P2.

% e
couples(List) :- bagof(Couple, couple(Couple), List). % bagof includes duplicates, meaning jay-dede & dede-jay
couples_(Res) :- setof(Couple, couple(Couple), Res). % setof eliminates those duplicates

% f
spouse_children(Person, Spouse-Children) :- couple(Person, Spouse),
                                            children(Person, Children).

% g
immediate_family(Person, Progenitores-SC) :- findall(Parent, parent(Parent, Person), Progenitores),
                                             spouse_children(Person, SC).

% h
parent_of_two(Parent) :- setof(Child, parent(Parent, Child), Children),
                         length(Children, NChildren),
                         NChildren >= 2.

parents_of_two(Parents) :- setof(Parent, parent_of_two(Parent), Parents).

