% a

leciona(algoritmos, adalberto).
leciona(bases_de_dados, bernardete).
leciona(compiladores, capitolino).
leciona(estatistica, diogenes).
leciona(redes, ermelinda).


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

professor_de(X,Y) :- frequenta(_UC, Y), leciona(_UC, X).
uc_comum(X,Y) :- frequenta(_UC, X), frequenta(_UC, Y), (X \= Y).
colega(X,Y) :- leciona(_,X), leciona(_,Y); frequenta(_Z, X), frequenta(_Z, Y), (X \= Y).

% b

% i.    leciona(X, diogenes).
% ii.   leciona(_, felismina).
% iii.  frequenta(X, claudio).
% iv.   frequenta(_, dalmindo).
% vi.   professor_de(bernardete, eduarda).
% vii.  uc_comum(alberto, alvaro).

% c 

% i.   professor_de(bernardete, antonio).
% ii.  professor_de(bernardete, Y).
% iii. professor_de(X, antonio).
% iv.  professor_de(X, Alunos), professor_de(Y, Alunos).
% v.   colega(ermelinda, diogenes). ; colega(antonio, bruna).
% vii. frequenta(X, Aluno), frequenta(Y, Aluno), (X \= Y).
