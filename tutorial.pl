pai(raimundo, rosana).
pai(raimundo, rosiane).
pai(raimundo, mumundo).

irmao(X, Y) :- pai(Z,X), pai(Z, Y), X\=Y.


