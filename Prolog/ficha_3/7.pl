%class(Course, ClassType, DayOfWeek, Time, Duration)
class(pfl, t, '1 Seg', 11, 1).
class(pfl, t, '4 Qui', 10, 1).
class(pfl, tp, '2 Ter', 10.5, 2).
class(lbaw, t, '1 Seg', 8, 2).
class(lbaw, tp, '3 Qua', 10.5,2).
class(ltw, t, '1 Seg', 10, 1).
class(ltw, t, '4 Qui', 11, 1).
class(ltw, tp, '5 Sex', 8.5, 2).
class(fsi, t, '1 Seg', 12, 1).
class(fsi, t, '4 Qui', 12, 1).
class(fsi, tp, '3 Qua', 8.5, 2).
class(rc, t, '4 Qui', 8, 2).
class(rc, tp, '5 Sex', 10.5, 2).

% a 
same_day(UC1, UC2) :- class(UC1,_,Day,_,_),class(UC2,_,Day,_,_).

% b
daily_courses(Day, Courses) :- findall(Course, class(Course,_,Day,_,_), Courses).

% c
short_classes(Courses) :- findall(Course/Type/Day, (class(Course,Type, Day, _, Dur), Dur < 2) , Courses).

% d 

course_classes(Course, Courses) :- findall(Day/Time-Type, class(Course, Type, Day, Time,_), Courses).

% e

courses(R) :- setof(Course, (CT,D,T,DT)^class(Course,CT,D,T,DT), R).

% f

print_list([]).
print_list([H | T]) :- write(H), nl, print_list(T).

% setof ordena (pela order de argumentos passados)
schedule :- setof(Day/Time/Duration-Course-Type, class(Course, Type, Day, Time, Duration), Classes), print_list(Classes).


% h

find_class :- write('Please input the Day: '), read(Day), nl,
              write('Please input the Time: '), read(Time), nl,
              class(Course, _, Day, Time, Duration),
              write(Course/Time/Duration).
