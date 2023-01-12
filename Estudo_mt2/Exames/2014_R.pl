hours([7, 7.5, 8, 8.5, 9, 9.5, 10, 10.5]).
airTime('The Walking Dead',sunday,9).
airTime('The Walking Dead 2',sunday,10).

airTime('Game of Thrones',sunday,8.5).
airTime('The Big Bang Theory',monday,8).
airTime('How I Met Your Mother',thursday,8).
airTime('Mad Men',sunday,10).
views('The Walking Dead',11).
views('Game of Thrones',5).
views('The Big Bang Theory',9).
views('Mad Men',2.5).
views('How I Met Your Mother',19).
network('The Walking Dead',amc).
network('The Walking Dead 2',amc).
network('Mad Men',amc).
network('Game of Thrones',hbo).
network('The Big Bang Theory',cbs).
network('How I Met Your Mother',cbs).

% a
tvShowNetwork(Network, DayOfWeek, Hour, TvShow) :-
    network(TvShow, Network), airTime(TvShow, DayOfWeek, Hour).

% b

% mostViews(+Network, -TvShow, -DayOfWeek, -Hour)
mostViews(Network, TvShow, DayOfWeek, Hour) :- 
    network(TvShow, Network), airTime(TvShow, DayOfWeek, Hour), views(TvShow, Views), 
    \+ (network(TvShow2, Network), airTime(TvShow2, _, _), views(TvShow2, Views2), 
    Views2 > Views).

% c

hottestTvShows([], []).
hottestTvShows([Network | T], TvShows) :-
    hottestTvShows(T, TvShows2),
    mostViews(Network, TvShow, _, _),
    append([Network-TvShow], TvShows2, TvShows).
    % TvShows = [Network-TvShow | TvShows2].

% d

getShows(Network, Result) :- 
    findall([Show-Day-Hour], (network(Show, Network), airTime(Show, Day, Hour), member(Hour, hours)), Result).

schedule(Network, Result) :-
    getShows(Network, Temp),
    sort_by_hour(Temp, Result).

    
sort_by_hour(Shows, Sorted) :-
    maplist(split_show, Shows, SplitShows),
    keysort(SplitShows, Sorted),
    maplist(join_show, Sorted, _Sorted).

split_show(Show-Day-Hour, Hour-Show-Day) :- true.

join_show(Hour-Show-Day, Show-Day-Hour) :- true.


% 2

project(projA, cenasA).
project(projB, cenasB).
project(projC, cenasC).

task(projA, 1, 'skrr', 1).
task(projB, 2, 'skrr', 1).
task(projC, 3, 'skrr', 1).

task(projA, 4, 'skrr', 1).
task(projB, 5, 'skrr', 1).


% task(ProjID,TaskId,Description,NecessaryTime). % tarefa de um projeto
% precedence(ProjID,TaskId1,TaskId2). % precedência entre tarefas (TaskId1->TaskId2)

% a

getTasksProj(ProjID, Tasks, Length) :-
    findall([TaskID], task(ProjID, TaskID, _, _), Tasks),length(Tasks, Length).

proj_tasks(L) :- 
    findall([ProjID-Length], (project(ProjID, _Name), getTasksProj(ProjID, _Tasks, Length)), L).

% b

