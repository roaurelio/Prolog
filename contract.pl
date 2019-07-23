
skills(sue, [carpentry, concrete]).
skills(john, [sewing, wallpaper, drywall]).
skills(mary, [concrete, drywall, sewing]).
skills(tom, [carpentry, plumbing]).

requires(bedroom,  [[s, sewing, 3],      [w, wallpaper, 8]]).
requires(bathroom, [[c, carpentry, 6],   [d, drywall, 8],   [p, plumbing, 5]]).
requires(kitchen,  [[c1, carpentry, 11], [p1, plumbing, 2], [p2, plumbing, 1], [s1, sewing, 4]]).

% Job: requires(Job, TaskList)
% Tasklist = [TaskName, TaskType, TaskTime]

% Workers: lista ordenada de pessoas e tempo contratado -
% [Person, TimeContracted]

% Assignments: lista de atribuições de tarefas
% [TaskName, Person]

pertence(X, [X|_]).
pertence(X, [_|L]):- pertence(X,L).

membro([X,_], [[X,_]|_]).
membro([X,_], [_|L]) :- membro([X,_],L).

%find person with taskType in your skill's list
findPerson(TaskType, [Person]) :-
    skills(Person, L),
    pertence(TaskType, L).

workerList([], []).
workerList([[_, TaskType, Time]],[[Person, Time]]):-
    findPerson(TaskType, [Person]).
workerList([[_, TaskType, Time]|X],W):-
    findPerson(TaskType, [Person]),
    workerList(X,L),
    not(membro([Person, Time],L)),
    W = [[Person, Time]|L].

%worker list
worker(Job, W) :-
    requires(Job, TaskList),
    workerList(TaskList, W).


assignmentList([], []).
assignmentList([[TaskName, TaskType, _]],[[TaskName, Person]]):-
    findPerson(TaskType, [Person]).
assignmentList([[TaskName, TaskType, _]|X],A):-
    findPerson(TaskType, [Person]),
    assignmentList(X, L),
    not(membro([TaskName, Person],L)),
    A = [[TaskName, Person]|L].
assignmentList([[TaskName, TaskType, _]|X],[[Person, _]|W],A):-
    assignmentList(X,W,L),
    not(membro([TaskName, Person],L)),
    A = [[TaskName, Person]|L].


assignment(Job, A) :-
    requires(Job, TaskList),
    assignmentList(TaskList, A).
assignment(Job, W, A) :-
    requires(Job, TaskList),
    assignmentList(TaskList, W, A).

%contract(Job, Workers, Assignments)
% Exemplo: contract(bathroom, [[john, J]|W], [[c, tom]|A])
contract(J, W, A) :-
    worker(J, W),
    assignment(J, A).

contract(J,W,A) :-






