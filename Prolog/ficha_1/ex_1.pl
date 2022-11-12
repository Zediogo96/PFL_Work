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
male(bo).
male(dylan).
male(luke).
male(rexford).
male(calhoun).
male(george).

% family

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


/* 1b */
%i. female(haley).
%ii. male(gil).
%iii. parent(phil, frank).
%iv. parent(claire, X).
%v. parent(X, gloria).
%vi. parent(X, jay), parent(Y, X).
%vii. parent(lily, X), parent(X,Y).
%viii. parent(X, alex).
%ix. parent(jay, X), \+ (parent(gloria, X)).

/* 1c */
% father(X,Y) :- male(X), parent(X,Y).
% grandparent(X,Y) :- parent(X,Z), parent(Z,Y).
% grandfather(X,Y) :- parent(X,Z), parent(Z,Y), male(X).
% grandmother(X,Y) :- parent(X,Z), parent(Z,Y), female(X).
% siblings(X,Y) :- parent(F,X), parent(F,Y), parent(M,X), parent(F,X), dif(X,Y), dif(F,M).
% halfsibling(X,Y) :- siblings(X,Y), parent(PX,X), parent(PY,Y), (PX \= PY).
% cousins(X,Y) :- parent(PX, X), parent(PY, Y), siblings(PX, PY), (X\=Y), (PX \= PY).
% uncle (X,Y) :- parent(Z,Y), siblings(Z,X), male(X)

/* 1e */
married(jay, gloria, 2008).
married(jay, dede, 1968).
divorced(jay, dede, 2003).

% BONUS - deve definir-se as regras "para os dois lados"
married(A,B,Y) :- divorced(B,A,Y).
divorced(A,B,Y) :- married(B,A,Y).
