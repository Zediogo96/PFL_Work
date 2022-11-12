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

/* b */
% leciona(X, diogenes).
% felismina leciona alguma UC
% leciona(X, felismina).
% frequenta(X, claudio).
% frequenta(X, dalmindo).
% leciona(X,bernardete), frequenta(X, eduarda).
% frequenta(X, alberto), frequenta(x, alvaro).

/* c */
aluno(X,Y) :- frequenta(UC,X), leciona(UC,Y).
professor(X,Y) :- aluno(Y,X).
colega(X,Y) :- frequenta(UC,X), frequenta(UC,Y), X \= Y; leciona(UC,X), leciona(UC,Y), X \= Y.

