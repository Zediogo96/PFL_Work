% a)
% gender
female(grace).
female(dede).
female(gloria).
female(barb).
female(claire).
female(pameron).
female(haley).
female(alex).
female(lily).
female(poppy).

male(frank).
male(jay).
male(javier).
male(merle).
male(phil).
male(mitchell).
male(joe).
male(manny).
male(cameron).
male(bob).
male(dylan).
male(luke).
male(rexford).
male(calhoun).
male(george).

% relationships
parent(grace, phil).
parent(frank, phil).
parent(dede, claire).
parent(jay, claire).
parent(dede, mitchell).
parent(jay, mitchell).
parent(jay, joe).
parent(gloria, joe).
parent(gloria, manny).
parent(javier, manny).
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

% b
    % i.    female(haley).
    % ii.   male(gil).
    % iii.  parent(frank, phil).
    % iv.   parent(P, claire).
    % v.    parent(gloria, S).
    % vi.   parent(jay, _X), parent(_X, Y).
    % vii.  parent(X, _Y), parent(_Y, lily).
    % viii. parent(alex, _X). 
    % ix.   parent(jay, X), \+ parent(gloria, X).    


% c 
    % a é pai de b
    father(X,Y) :- parent(X,Y), male(X).
    mother(X,Y) :- parent(X,Y), female(X).

    grandparent(X,Y) :- father(X,_Z), parent(_Z,Y).
    grandmother(X,Y) :- mother(X,_Z), parent(_Z,Y).

    siblings(X,Y) :- parent(_Z,X), parent(_Z,Y), X \= Y.
    % halfsiblings(X,Y) :- \+ siblings(X,Y), parent(P,X), parent(P,Y), (X \= Y).
    cousins(X,Y) :- parent(PX, X), parent(PY, Y), siblings(PX, PY), (X \= Y), (PX \= PY).
    uncle(X,Y) :- parent(P, Y), siblings(X,P).

% d
    % cousins(haley, lily).
    % father(X, luke).
    % uncle(X, lily).
    % grandparent(X, lily).
    % siblings(lily, S).
    % halfsiblings(lily, HS).

% e

    married(jay, gloria, 2008).
    married(jay, dede, 1968).
    divorced(jay, dede, 2003).


