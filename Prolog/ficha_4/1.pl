% dynamic means that the definition of the predicate may change during the course of the program, usually because of clauses being asserted or retracted
:- dynamic male/1, female/1, private/1.

% Adding Clauses 

% - assert/1 adds a clause to the database (or a query basically)
% - asserta/1 adds a clause to the beginning of the database
% - assertz/1 adds a clause to the end of the database

% a 

save_person(male, Name) :- assert(male(Name)).
save_person(female, Name) :- assert(female(Name)).
save_person(private, Name) :- assert(private(Name)).

% this means that we'll test for e.g ?- male(jose). -> yes 

add_person :- format('male/female/private?', []), nl,
              read(Gender), nl,
              format('name?', []), nl,
              read(Name),
              save_person(Gender, Name).

% b

:- dynamic parent/2.

save_parents(Person) :- format('Who is the father?', []), nl,
                        read(Father), nl,
                        format('Who is the mother?', []), nl,
                        read(Mother), nl,
                        assert(parent(Father, Person)),
                        assert(parent(Mother, Person)).

% we'll test after adding with with 
% - parent(domingos, jose) -> yes.
% - parent(suzana, jose) -> yes.
% -- parent(domingos, suzana) -> no.

% c 

%% Removing Clauses
% ** retract/1 ** -> removes a clause from the program (the first that matches the given clause)
% ** retractall/1 ** -> retracts all clause smatching the specified head
% abolish/1 -> remove all clauses and properties of the specified predicate


% 1st we need to remove the gender clause created through add_person
remove_gender(Person) :- male(Person), retract(male(Person)).
remove_gender(Person) :- female(Person), retract(female(Person)).
remove_gender(Person) :- private(Person), retract(private(Person)).
remove_gender(_).

% ~n inside the string is equivalent to nl
remove_person :- format('Which Person do you wish to delete from the World?~n', []),
                 read(Person), nl,
                 remove_gender(Person),
                 retractall(parent(_, Person)). % removes all relation of childhood that the person has
                 retractall(parent(Person, _)). % removes all relations of parenthood that the person has




