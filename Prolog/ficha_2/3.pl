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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% X é chefe de Y ..
chefe(X,Y) :- cargo(C1, X), cargo(C2, Y), chefiado_por(C1, C2).

superior(X,Y) :- chefe(Y,X).
superior(X,Y) :- chefe(Z, X), superior(X,Z).
