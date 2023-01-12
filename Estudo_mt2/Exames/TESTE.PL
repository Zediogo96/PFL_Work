hours([7, 7.5, 8, 8.5, 9, 9.5, 10, 10.5]).
airTime('The Walking Dead',sunday,9).
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
    \+ (network(TvShow2, Network), airTime(TvShow2, DayOfWeek2, Hour2), views(TvShow2, Views2), 
    Views2 > Views).

% find tvshows with most views for each network inside Networks without using forall
hottestTvShows([], []).
hottestTvShows([Network | T], TvShows) :-
    hottestTvShows(T, TvShows2),
    mostViews(Network, TvShow, DayOfWeek, Hour),
    TvShows = [[Network, TvShow, DayOfWeek, Hour] | TvShows2].

% schedule(+Network,+DayOfWeek,-Schedule)
schedule(Network, DayOfWeek, Schedule) :-
    findall(Hour, tvShowNetwork(Network, DayOfWeek, Hour, _), Schedule).


schedule(+Network,+DayOfWeek,-Schedule) :- 
    network(Show, Network), 


