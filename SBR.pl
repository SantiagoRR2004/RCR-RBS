% Si ponemos dynamic significa que podemos cambiar la base de conocimiento en tiempo de ejecución
:- dynamic pair/2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Esto es para que sepa que carreras puede elegir con su nota

% Aquí ponemos las carreras y las notas de corte

carrera_nota("Medicina", 12.690).
carrera_nota("Comunicaciones y Periodismo", 12.372).
carrera_nota("Matemáticas", 12.260).
carrera_nota("Ingeniería Aeroespacial", 12.090).
carrera_nota("Física",  11.760).
carrera_nota("Ingeniería Informática",  11.106).
carrera_nota("Relaciones Internacionales",  11.096).
carrera_nota("Psicología",  10.942).
carrera_nota("Enfermería",  10.932).
carrera_nota("Biología",  10.330).
carrera_nota("Ciencias de la Actividad Física y del Deporte",  9.740).
carrera_nota("Educación Infantil",  9.186).
carrera_nota("Historia",  8.980).
carrera_nota("Ingeniería Mecánica",  8.940).
carrera_nota("Filología Hispánica", 8.690).
carrera_nota("Diseño", 8.442).
carrera_nota("Derecho", 8.410).
carrera_nota("Educación Social", 7.230).
carrera_nota("Filosofía", 7.710).
carrera_nota("Historia del Arte", 6.930).
carrera_nota("Bellas Artes", 6.682).
carrera_nota("Ciencia y Tecnología de los Alimentos", 6.514).
carrera_nota("Administración y Dirección de Empresas", 6.278).
carrera_nota("Economía", 6.080).
carrera_nota("Filología Inglesa", 5.330).
carrera_nota("Ciencias Políticas y de la Administración", 5.260). 
carrera_nota("Turismo", 5.000).
carrera_nota("Arquitectura", 5.000).

% Si tiene más nota se le suma un punto
% Si tiene menos nota se le resta la diferencia
nota(X) :-
    forall(carrera_nota(C, N),
           (X >= N -> % Si la nota es mayor o igual a la de corte
               add_pair(C, 1) % Añadimos un punto
           ; 
               true
           )),
    forall(carrera_nota(C, N),
           (X < N -> % Si la nota es menor a la de corte
               FinalN is X - N, % Restamos la diferencia
               add_pair(C, FinalN)
           ; 
               true
           )).
% El forall itera sobre todas las carreras y notas de corte
% Ponemos el true para que no haya un fallo si no se cumple la condición


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ciencias("Física") :- add_pair("Física", 1).
% ciencias("Matemáticas") :- add_pair("Matematicas", 1).
% letras("Filologia hispanica") :- add_pair("Filologia hispanica", 1).

% ciencias("Física") :- add_pair("Física", 1).

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

