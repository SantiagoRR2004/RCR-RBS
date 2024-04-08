% Si ponemos dynamic significa que podemos cambiar la base de conocimiento en tiempo de ejecución
:- dynamic pair/2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


ciencias("Física") :- add_pair("Física", 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Esta es la parte en la que creamos las parejas o las actualizamos

% Todavía no se le ha dado a la carrera un valor
add_pair(Key, Value) :-
    not(pair(Key, _)), % Si no existe la pareja
    assert(pair(Key, Value)), % La añades
    !. % Corte para evitar el backtracking

% Ya se le ha dado a la carrera un valor y le sumamos el nuevo
add_pair(Key, Value) :-
    pair(Key, OldValue), % Obtenemos el valor antiguo
    NewValue is OldValue + Value, % Sumamos el nuevo valor
    retract(pair(Key, OldValue)), % Eliminas la antigua
    assert(pair(Key, NewValue)).  % Añades la nueva

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Esto obtiene la lista de todas las parejas que están en la base de conocimiento

% findall(Template, Goal, List)
% findall es un predicado que se utiliza para obtener una lista de todas las instancias de un término que satisfacen un objetivo dado.
obtain_list(List) :- findall(pair(Key, Value), pair(Key, Value), List).

% https://www.swi-prolog.org/pldoc/man?predicate=findall/3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Esto es para ordenar la lista de parejas con el algoritmo quicksort

quicksort([], []). % Caso base

quicksort([Pivot | Tail], Sorted) :- % El primero es el pivote
    partition(Pivot, Tail, Smaller, Larger), % Particionamos la lista
    quicksort(Smaller, SortedSmaller), % Ordenamos la lista de los menores
    quicksort(Larger, SortedLarger), % Ordenamos la lista de los mayores
    append(SortedLarger, [Pivot | SortedSmaller], Sorted). % Juntamos las dos listas poniendo los mayores primero

partition(_, [], [], []). % Caso base

% Si el valor de la pareja es menor o igual al pivote, lo ponemos en la lista de menores
partition(pair(PivotKey,PivotValue), [pair(Key2,Value2) | Xs], [pair(Key2,Value2) | Smaller], Larger) :-
    Value2 =< PivotValue,
    partition(pair(PivotKey,PivotValue), Xs, Smaller, Larger).

% Si el valor de la pareja es mayor al pivote, lo ponemos en la lista de mayores
partition(pair(PivotKey,PivotValue), [pair(Key2,Value2) | Xs], Smaller, [pair(Key2,Value2) | Larger]) :-
    Value2 > PivotValue,
    partition(pair(PivotKey,PivotValue), Xs, Smaller, Larger).

% https://stackoverflow.com/questions/63081678/how-to-quicksort-list-of-lists-in-prolog
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Esto es para imprimir la lista de parejas que se le da

printPairs([]). % Caso base

% Lo hacemos de forma recursiva
printPairs([pair(Key, Value) | Tail]) :-
    writef("%w: %w\n", [Key, Value]), % Imprimimos la pareja
    printPairs(Tail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Esto es para que la perosna pueda ver la lista de carreras
% Primero obtenemos la lista
% Luego la ordenamos
% Y finalmente la imprimimos

final :- obtain_list(L), quicksort(L,R), printPairs(R), !.
% Hay un corte para evitar el backtracking

