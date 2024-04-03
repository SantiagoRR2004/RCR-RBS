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





final :- obtain_list(L).


% forall(pair(X, _), writef(X))