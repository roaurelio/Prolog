
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

findElement(X,[X|_]).
findElement(X,[_|L]) :- findElement(X,L).

%------------%
%-- Worker --%
%------------%

findWorker(Skill,Worker) :-
    skills(Worker, L),
    findElement(Skill, L).

findWorkerInTaskList([[_, TaskType, TaskTime]], W) :-
    findWorker(TaskType,Worker),
    W = [Worker, TaskTime].

findWorkerInTaskList([[_,TaskType,TaskTime]|X], W) :-
    findWorker(TaskType, Worker),
    findWorkerInTaskList(X,L),
    W = [[Worker, TaskTime],L].

%----------------%
%-- Assignment --%
%----------------%

findAssignmentInTaskList([[TaskName, TaskType, _]], A) :-
    findWorker(TaskType,Worker),
    A = [TaskName, Worker].

findAssignmentInTaskList([[TaskName,TaskType,_]|X], A) :-
    findWorker(TaskType, Worker),
    findAssignmentInTaskList(X,L),
    A = [[TaskName, Worker],L].


contract(Job,W,A):-
    requires(Job, TaskList),
    findWorkerInTaskList(TaskList, W),
    findAssignmentInTaskList(TaskList, A).


