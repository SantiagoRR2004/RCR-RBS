:- dynamic pair/2.

ciencias("Física") :- add_pair("Física", 1).


add_pair(Key, Value) :-
    not(pair(Key, _)),
    assert(pair(Key, Value)),
    !. % Cut to prevent backtracking

add_pair(Key, Value) :-
    pair(Key, OldValue),
    NewValue is OldValue + Value,
    retract(pair(Key, OldValue)), % Remove old pair
    assert(pair(Key, NewValue)).  % Assert new pair


obtain_list(List) :- findall(pair(Key, Value), pair(Key, Value), List).


quicksort([], []).
quicksort([Pivot | Tail], Sorted) :-
    partition(Pivot, Tail, Smaller, Larger),
    quicksort(Smaller, SortedSmaller),
    quicksort(Larger, SortedLarger),
    append(SortedLarger, [Pivot | SortedSmaller], Sorted).

partition(_, [], [], []).
partition(pair(PivotKey,PivotValue), [pair(Key2,Value2) | Xs], [pair(Key2,Value2) | Smaller], Larger) :-
    Value2 =< PivotValue,
    partition(pair(PivotKey,PivotValue), Xs, Smaller, Larger).

partition(pair(PivotKey,PivotValue), [pair(Key2,Value2) | Xs], Smaller, [pair(Key2,Value2) | Larger]) :-
    Value2 > PivotValue,
    partition(pair(PivotKey,PivotValue), Xs, Smaller, Larger).


printPairs([]).

printPairs([pair(Key, Value) | Tail]) :-
    writef("%w: %w\n", [Key, Value]),
    printPairs(Tail).


final :- obtain_list(L), quicksort(L,R), printPairs(R), !.


