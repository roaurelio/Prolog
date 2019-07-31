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


plan0(S0,[G],[],S0,[G]):- pertence(G,S0).
plan0(S0,[G],P,S,[G])  :-    %%somente um objetivo
    action(A,PC,E),
<<<<<<< HEAD
    pertence(add(G),E),       %o objetivo pertence lista dos efeitos da aï¿½ï¿½o
    listPertence(PC,S0),      %estado atual atende prï¿½-requisito
    processEffectList(E,S0,S),%aplicar efeitos no estado atual
    P = [A].                  %adicionar aï¿½ï¿½o
plan0(S0,[G|L],P,S,F) :-          %%lista de objetivos
    action(A,PC,E),
    (pertence(add(G),E);         %objetivo pertence a lista dos efeitos da aï¿½ï¿½o
    pertence(G,S0)),             %ou objetivo pertence ao estado atual
    listPertence(PC,S0),         %e estado atual atende prï¿½-requisitos
    processEffectList(E,S0,S1),  %aplicar efeitos no estado atual
    plan0(S1,L,P0,S,F0),
    P = [A|P0],
    F = [G|F0].                  %adiciona aï¿½ï¿½o
plan0(S0,[G],P,S,[G]):-             %%somente um objetivo
    action(A,[X|L],E),
    pertence(X,S0),              %estado da mï¿½o pertence aos prï¿½-requisitos da aï¿½ï¿½o
    listPertence(L,S0),          %estado atende prï¿½-requisitos
    not(pertence(add(G),E)),     %objetivo nï¿½o pertence ï¿½ lista de efeitos
    not(pertence(G,S0)),         %objetivo nï¿½o pertence ao estado atual
=======
    pertence(add(G),E),       %o objetivo pertence lista dos efeitos da ação
    listPertence(PC,S0),      %estado atual atende pré-requisito
    processEffectList(E,S0,S),%aplicar efeitos no estado atual
    P = [A].                  %adicionar ação
plan0(S0,[G|L],P,S,F) :-          %%lista de objetivos
    action(A,PC,E),
    (pertence(add(G),E);         %objetivo pertence a lista dos efeitos da ação
    pertence(G,S0)),             %ou objetivo pertence ao estado atual
    listPertence(PC,S0),         %e estado atual atende pré-requisitos
    processEffectList(E,S0,S1),  %aplicar efeitos no estado atual
    plan0(S1,L,P0,S,F0),
    P = [A|P0],
    F = [G|F0].                  %adiciona ação
plan0(S0,[G],P,S,[G]):-             %%somente um objetivo
    action(A,[X|L],E),
    pertence(X,S0),              %estado da mão pertence aos pré-requisitos da ação
    listPertence(L,S0),          %estado atende pré-requisitos
    not(pertence(add(G),E)),     %objetivo não pertence à lista de efeitos
    not(pertence(G,S0)),         %objetivo não pertence ao estado atual
>>>>>>> master
    processEffectList(E,S0,S1),  %aplica efeito no estado atual
    plan0(S1,[G],P0,S),
    P = [A|P0].
plan0(S0,[G|Y],P,S,F):-
    action(A,[X|L],E),
    pertence(X,S0),
    listPertence(L,S0),
    not(pertence(add(G),E)),%nï¿½o pertence aos efeitos da aï¿½ï¿½o
    not(pertence(G,S0)),    %nï¿½o pertence ï¿½ lista de objetivos
    processEffectList(E,S0,S1),
    plan0(S1,[G|Y],P0,S,F0),
    P = [A|P0],
    F = [G|F0].
plan0(S0,[G|Y],P,S,F):-
    action(_,[X|L],E),
    pertence(X,S0),
<<<<<<< HEAD
    not(listPertence(L,S0)),% nï¿½o pertence ao estado atual
    not(pertence(add(G),E)),% nï¿½o pertence aos efeitos da aï¿½ï¿½o
    not(pertence(G,S0)),    % nï¿½o pertence ï¿½ lista de objetivos
    naoPertence(L,S0,NP),   % obtem lista com itens da aï¿½ï¿½o que nï¿½o pertencem a S
=======
    not(listPertence(L,S0)),% não pertence ao estado atual
    not(pertence(add(G),E)),% não pertence aos efeitos da ação
    not(pertence(G,S0)),    % não pertence à lista de objetivos
    naoPertence(L,S0,NP),   % obtem lista com itens da ação que não pertencem a S
>>>>>>> master
    copiarLista(NP,X,Y,S1,P1),
    plan0(S1,[G|Y],P2,S2,F0),
    plan0(S2,F,P3,S,F1),
    P = [P1|P2],
    P = [P3,P],
    F = [F0,F1].

copiarLista([X],Y,S0,S,P) :-
    action(A,[Y|_],E),
    pertence(add(X),E),
    processEffectList(E,S0,S),
    P = A.
copiarLista([X|L],Y,S0,S,P) :-
    action(A,[Y|_],E),
    pertence(add(X),E),
    processEffectList(E,S0,S1),
    copiarLista(L,Y,S1,S,P1),
    P = [A|P1].


naoPertence([X], L, [X]) :- not(pertence(X,L)).
naoPertence([X|Y],L,S) :-
    not(pertence(X,L)),
    naoPertence(Y, L, S1),
    S = [X|S1].

findGoalInAction(S,G):-
    action(_,L,E),
    listPertence(L,S),
    pertence(add(G),E).

%planning(S,G,P,F)
%plan(S,G,P,F) :- avaliaGoal(G,S,F),P=[].
plan(S0,G,P,F) :-
    plan0(S0,G,P0,S),
    plan(S,G,P1,F),
    P = [P0|P1].

















