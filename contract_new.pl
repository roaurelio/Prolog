
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

existWorker(Worker,[[Worker,_]]).
existWorker(Worker,[_|L]) :- existWorker(Worker,L).


findElement(X,[X|_]).
findElement(X,[_|L]) :- findElement(X,L).

insertElement([],[X],[X]).
insertElement([[H,H2]|T],[[X,X2]],[[H,H2]|Y]) :-
    H \== X,
    insertElement(T,[[X,X2]],Y).

findWorker(Skill,Worker) :-
    skills(Worker, L),
    findElement(Skill, L).

findInTaskList([[_, TaskType, TaskTime]], W) :-
    findWorker(TaskType,Worker),
    W = [Worker, TaskTime].

findInTaskList([[_,TaskType,TaskTime]|X], W) :-
    findWorker(TaskType, Worker),
    findInTaskList(X,L),
    insertElement(L,X,L),
    W = [[Worker, TaskTime],L].

workers(Job,W):-
    requires(Job, TaskList),
    findInTaskList(TaskList, W).


