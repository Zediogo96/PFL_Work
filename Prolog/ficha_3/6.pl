:- use_module(library(ordsets)).

leciona(algoritmos, adalberto).
leciona(bases_de_dados, bernardete).
leciona(compiladores, capitolino).
leciona(estatistica, diogenes).
leciona(redes, ermelinda).
leciona(shitlog, ermelinda).

frequenta(algoritmos, alberto).
frequenta(algoritmos, bruna).
frequenta(algoritmos, cristina).
frequenta(algoritmos, diogo).
frequenta(algoritmos, eduarda).

frequenta(bases_de_dados, antonio).
frequenta(bases_de_dados, bruno).
frequenta(bases_de_dados, cristina).
frequenta(bases_de_dados, duarte).
frequenta(bases_de_dados, eduardo).

frequenta(compiladores, alberto).
frequenta(compiladores, bernardo).
frequenta(compiladores, clara).
frequenta(compiladores, diana).
frequenta(compiladores, eurico).

frequenta(estatistica, antonio).
frequenta(estatistica, bruna).
frequenta(estatistica, claudio).
frequenta(estatistica, duarte).
frequenta(estatistica, eva).

frequenta(redes, alvaro).
frequenta(redes, beatriz).
frequenta(redes, claudio).
frequenta(redes, diana).
frequenta(redes, eduardo).

%% AUXILIAR RULES %%%%%

aluno(X,Y) :- leciona(Cadeira, Y), frequenta(Cadeira, X).

prof(X,Y) :- aluno(Y,X).

colega(X,Y, UC) :- frequenta(UC, X), frequenta(UC,Y), X \= Y.

% a 
% esta solução daria duplicados (mesmo aconteceria com bagof em vez de setof em baixo)
teachers(T) :- findall(Teacher, leciona(_, Teacher), T).

% b -> Utilizando setof, já estamos a garantir que não acontecem duplicados
% ^ is the intersection operator
teachers_(T) :- setof(Teacher, Cadeira^leciona(Cadeira, Teacher), T).

% c
students_of(T, Students) :- setof(Student, aluno(Student, T), Students).

% d
teachers_of(S, Teachers) :- setof(Teacher, prof(Teacher, S), Teachers).

% e
% multiple rules can be used in for e.g. setof predicate
common_courses(S1, S2, Courses) :- setof(Course, (frequenta(Course, S1), frequenta(Course, S2)), Courses).



% f
courses(Student, Courses) :- setof(Course, frequenta(Course, Student), Courses).

more_than_one(Student) :- courses(Student, Courses), length(Courses, L), L > 1.

more_than_one_course(Result) :- setof(Student, more_than_one(Student), Result).

% g

strangers(L) :- setof(S1-S2, (UC1,UC2)^(frequenta(UC1, S1), frequenta(UC2, S2), S1 \= S2), AllPairs),
                setof(S1-S2, UC^colega(S1,S2,UC), Pairs),
                ord_subtract(AllPairs, Pairs, L). % must include library


% h

good_groups(L) :- setof(S1-S2, (UC1,UC2)^(frequenta(UC1, S1), frequenta(UC1, S2), frequenta(UC2, S1), frequenta(UC2, S2), S1 \= S2), L).

                






