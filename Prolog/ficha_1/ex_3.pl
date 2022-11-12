% a 
piloto(lamb).
piloto(besenyei).
piloto(chambliss).
piloto(maclean).
piloto(mangold).
piloto(jones).
piloto(bonhomme).

equipa(lamb, breitling).
equipa(besenyei, red_bull).
equipa(chambliss, red_bull).
equipa(maclean, mediterranean_racing_team).
equipa(mangold, cobra).
equipa(jones, matador).
equipa(bonhomme, matador).

aviao(lamb, mx2).
aviao(besenyei, edge540).
aviao(chambliss, edge540).
aviao(maclean, edge540).
aviao(mangold, edge540).
aviao(jones, edge540).
aviao(bonhomme, edge540).

circuito(istanbul).
circuito(budapest).
circuito(porto).

vencedor(jones, porto).
vencedor(mangold budapeste).
vencedor(mangold, istambul).

n_gates(istanbul, 9).
n_gates(budapest, 6).
n_gates(porto, 5).

% b
vencedor(X,porto).
vencedor(X,budapest).
n_gates(X, _N), _N > 8.
aviao(X, _P), _P \= edge540.
vencedor(X, _C), vencedor(X, _D), _C \= _D.
vencedor(X, porto), aviao(X, Y).


