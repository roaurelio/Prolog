maior(a, b).

superior(A, B) :- maior(A, B).


membro(X, [X|_]).
membro(X, [_|L]):- membro(X,L).



isMembro(X, L) :- membro(X, L).
isNotMembro(X, L) :- not(membro(X,L)).
