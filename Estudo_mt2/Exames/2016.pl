participant(1234, 17, 'Pé coxinho').
participant(3423, 21, 'Programar com os pés').
participant(3788, 20, 'Sing a bit').
participant(4865, 22, 'Pontes de esparguete').
participant(8937, 19, 'Pontes de pen-drives').
participant(2564, 20, 'Moodle Hack').

perfomance(1234, [120,120,120,120]).
perfomance(3423, [32,120,45,120]).
perfomance(3788, [110,2,6,43]).
perfomance(4865, [120,120,110,120]).
perfomance(8937, [97,101,105,110]).

% 1
madeItThrough(ID) :- 
    perfomance(ID, Presses), member(120, Presses).

% 2

getValueAtIndex(List, Index, Value) :-
    append(_Start, [Value | _Rest], List), Adjust is Index - 1, length(_Start, Adjust).

juriTimes([], _, [], 0).
juriTimes([H | T], Juri, Times, Total) :-
    juriTimes(T, Juri, Times1, Total1),
    perfomance(H, Presses),
    getValueAtIndex(Presses, Juri, Value),
    Total is Total1 + Value,
    append([Value], Times1, Times).

% 3

patientJuri(JuriMember) :-
    perfomance(P1, Presses1), perfomance(P2, Presses2), P1 =\= P2,
    getValueAtIndex(Presses1, JuriMember, V1), getValueAtIndex(Presses2, JuriMember, V2),
    V1 =:= 120, V2 =:= 120.

% 4

sumTimes([], 0).
sumTimes([H | T], Count) :-
    sumTimes(T, Count1),
    Count is Count1 + H.


bestParticipant(P1, P2, Z) :-
    perfomance(P1, Presses1), perfomance(P2, Presses2), P1 =\= P2,
    sumTimes(Presses1, Sum1), sumTimes(Presses2, Sum2),
    ((Sum1 > Sum2 -> Z = P1 ; Z = P2)).

% 5

allPerfs :- 
    \+ allPerfsAux.

allPerfsAux :- 
    participant(ID, _, Act),
    perfomance(ID, Res),
    format('~w:~w:~w', [ID, Act, Res]), nl, fail.

% 6

nSuccessfulParticipants(T) :-
    findall(ID, (perfomance(ID, Presses), \+ (member(X, Presses), X =\= 120)), Temp), length(Temp, T).


% 7

getIndexOfJuris([],_, []).
getIndexOfJuris([Time | T], N, JuriList) :-
    N1 is N + 1,
    ((Time =:= 120) ->
        getIndexOfJuris(T, N1, JuriList1),
        append([N], JuriList1, JuriList)
    ;
    getIndexOfJuris(T, N1, JuriList)
    ).

juriFans(JuriFanList) :-
    findall([ParticipantID-JurisMausList], (perfomance(ParticipantID, Presses), getIndexOfJuris(Presses, 1, JurisMausList)), JuriFanList).

% 8

:-use_module(library(lists)).

eligibleOutcome(ID, Perf, TT) :-
    perfomance(ID, Times),
    madeItThrough(ID),
    participant(ID, _, Perf),
    sumlist(Times, TT).

nextPhase(N, Participants) :- 
    setof([TT-PID-Perf], eligibleOutcome(PID, Perf, TT), ParticipantsTemp),
    append(Participants, Rest, ParticipantsTemp),
    length(Participants, N).

% 9 
% É um green cut.

% 10

impoe(X,L) :-
    length(Mid, X),
    append(L1, [X | _], L), append(_, [X | Mid], L1).

% O predicato retorna true se existir uma sub-list que começa em X e acaba em X, e entre estes dois X's têm X elementos.
