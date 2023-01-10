% Database

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

% i.    sisnando é chefia por algum analista?
% ii.   quais são os cargos que alguem __tecnico__ chefia e que outros cargos também chefiam os mesmos?
% iii.  quais são os cargos chefiados por __supervisor__ e quais os nomes das pessoas?
% iv.   que cargos são chefiados por __diretor__ e não são o cargo da felismina?

% b

% i.    X = supervisor.
% ii.   X = engenheiro, Y = osvaldo.
% iii.  J = analista, P = leonide.
% iv.   P = supervisor_chefe.

% c

% i.
chefe_de(X,Y) :- cargo(_C1, X), cargo(_C2, Y), chefiado_por(_C2, _C1).
% ii.
chefia_mm_cargo(X,Y) :- chefe_de(_C, X), chefe_de(_C, Y), (X \= Y).
% iii.
brokies(X) :- cargo(X, _), \+ chefe_de(X, _).
% iv.
big_bosses(Y) :- cargo(_X, Y), \+ chefe_de(_, Y).



