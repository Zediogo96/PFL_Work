cargo(tecnico, eleuterio).
cargo(tecnico, juvenaldo).
cargo(analista, leonilde).
cargo(analista, marciliano).
cargo(engenheiro, osvaldo).
cargo(engenheiro, porfirio).
cargo(engenheiro, reginaldo).
cargo(supervisor, sisnando).
cargo(supervisor_chefe, gertrudes).
cargo(secretaria_exec, felismina).
cargo(diretor, asdrubal).
chefiado_por(tecnico, engenheiro).
chefiado_por(engenheiro, supervisor).
chefiado_por(analista, supervisor).
chefiado_por(supervisor, supervisor_chefe).
chefiado_por(supervisor_chefe, diretor).
chefiado_por(secretaria_exec, diretor).

% a
/*
i.
funções chefiadas por analista e que são ocupadas por alguém chamado sisnando
ii.
funções X que chefiam tecnico e depois quais chefiam X
iii.
nome e cargo de pessoas que são chefiadas por um supervisor
iv. 
cargo que é chefiado por um diretor, mas não é ocupado por
alguém chamado felismina
*/

% b
/*
i.
X = supervisor.
ii.
X = engenheiro, Y = supervisor.
iii.
J = analista,
P = leonilde;
J = analista,
P = marciliano.
iv. 
P = supervisor_chefe -- (nome = gertrudes)
*/

% c
chefe(X,Y) :- cargo(C1_, X), cargo(C2_, Y), chefiado_por(C2_, C1_).

