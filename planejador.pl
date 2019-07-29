%%%%Estados%%%%

ontable(a).
ontable(b).

on(a,b).
on(b,a).

holding(a).
holding(b).

putdown(a) :- ontable(a).
putdown(b) :- ontable(b).

stack(a,b) :- on(a,b).
stack(b,a) :- on(b,a).

pickup(a) :- holding(a).
pickup(b) :- holding(b).

clear(a) :- not(on(b,a)), ontable(a).
clear(b) :- not(on(a,b)), ontable(b).

handempty.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


action(pickup(X),
       [handempty, ontable(X), clear(X)],
       [del(handempty), del(clear(X)), del(ontable(X)), add(holding(X))]).

action(pickup2(X),
       [handempty, on(X,Y), clear(X)],
       [del(handempty), del(clear(X)), del(on(X,Y)), add(clear(Y)), add(holding(X))]).

action(putdown(X),
       [holding(X)],
       [del(holding(X)), add(ontable(X)), add(clear(X)), add(handempty)]).

action(stack(X,Y),
       [holding(X), clear(Y)],
       [del(holding(X)), del(clear(Y)), add(handempty), add(on(X,Y)), add(clear(X))]).

delFactState(X,[X], F)  :- F = [].
delFactState(X,[X|L],F) :- F = L.
delFactState(X,[Y|L],F) :-
    delFactState(X,L,F0),
    F = [Y|F0].

pertence(X,[X]).
pertence(X,[X|_]).
pertence(X,[_|Y]) :- pertence(X,Y).

listPertence([X],L):- pertence(X,L).
listPertence([X|Y], L) :-
    pertence(X,L),
    listPertence(Y,L).

addFactState(X,[],[X]).
addFactState(X,L,F) :- F = [X|L].

processEffectList([del(X)],L,F) :- delFactState(X,L,F).
processEffectList([del(X)|Y],L,F) :-
    delFactState(X,L,F0),
    processEffectList(Y,F0,F).
processEffectList([add(X)],L,F) :- addFactState(X,L,F).
processEffectList([add(X)|Y],L,F) :-
    addFactState(X,L,F0),
    processEffectList(Y,F0,F).


findAction(S0,S,P) :-
    action(A,PC,E),
    listPertence(PC,S0),
    processEffectList(E,S0,S),
    P = [A].


avaliaGoal([], _, []).
avaliaGoal([X|Y], S, F) :-
    pertence(X,S),
    avaliaGoal(Y,S,F0),
    F = [X|F0].


plan0(S0,[G],[],S0):- pertence(G,S0).
plan0(S0,[G],P,S) :-
    action(A,PC,E),
    pertence(add(G),E),
    listPertence(PC,S0),
    processEffectList(E,S0,S),
    P = [A].
plan0(S0,[G|L],P,S) :-
    action(A,PC,E),
    (   pertence(add(G),E) ; pertence(G,S0)),
    listPertence(PC,S0),
    processEffectList(E,S0,S1),
    plan0(S1,L,P0,S),
    P = [A|P0].
plan0(S0,[G],P,S):-
    action(A,[X|L],E),
    pertence(X,S0),
    listPertence(L,S0),
    not(pertence(add(G),E)),
    not(pertence(G,S0)),
    processEffectList(E,S0,S1),
    plan0(S1,[G],P0,S),
    P = [A|P0].
plan0(S0,[G|Y],P,S):-
    action(A,[X|L],E),
    pertence(X,S0),
    listPertence(L,S0),
    not(pertence(add(G),E)),%não pertence aos efeitos da ação
    not(pertence(G,S0)),    %não pertence à lista de objetivos
    processEffectList(E,S0,S1),
    plan0(S1,[G|Y],P0,S),
    P = [A|P0].

naoPertence([X], L, [X]) :- not(pertence(X,L)).
naoPertence([X|Y],L,S) :-
    not(pertence(X,L)),
    naoPertence(Y, L, S1),
    S = [X|S1].


%planning(S,G,P,F)
plan(S,G,P,F) :- avaliaGoal(G,S,F),P=[].
plan(S0,G,P,F) :-
    plan0(S0,G,P0,S),
    plan(S,G,P1,F),
    P = [P0|P1].

















