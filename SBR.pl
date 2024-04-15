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
carrera_nota("Química", 7.790).
carrera_nota("Filosofía", 7.710).
carrera_nota("Educación Social", 7.230).
carrera_nota("Historia del Arte", 6.930).
carrera_nota("Bellas Artes", 6.682).
carrera_nota("Ciencia y Tecnología de los Alimentos", 6.514).
carrera_nota("Administración y Dirección de Empresas", 6.278).
carrera_nota("Ciencias del mar", 6.150).
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
% Esto es por el tipo de bachillerato

% cada tipo de bachillerato es un conjunto representado como una lista
% Estos conjuntos no son disjuntos. Tienen elementos en común
% Si haces 2 tipos de bachillerato las carreras en común contarán el doble
bachilleratoCiencias(["Medicina", "Matemáticas", "Física", "Psicología", "Enfermería", "Biología", "Ciencias de la Actividad Física y del Deporte", "Química", "Ciencia y Tecnología de los Alimentos", "Ciencias del mar", "Economía", "Arquitectura"]).
bachilleratoTech(["Matemáticas", "Física", "Ingeniería Aeroespacial", "Ingeniería Informática", "Ingeniería Mecánica", "Arquitectura"]).
bachilleratoLetras(["Comunicaciones y Periodismo", "Filología Hispánica", "Filosofía", "Filología Inglesa"]).
bachilleratoArtes(["Diseño", "Historia del Arte", "Bellas Artes", "Arquitectura"]).
bachilleratoSociales(["Comunicaciones y Periodismo", "Relaciones Internacionales", "Educación Infantil", "Historia", "Derecho", "Filosofía", "Educación Social", "Historia del Arte", "Administración y Dirección de Empresas", "Economía", "Ciencias Políticas y de la Administración", "Turismo"]).

bachillerato("Ciencias") :- bachilleratoCiencias(C), forall(member(X, C), add_pair(X, 1)).
bachillerato("Tecnología") :- bachilleratoTech(C), forall(member(X, C), add_pair(X, 1)).
bachillerato("Letras") :- bachilleratoLetras(C), forall(member(X, C), add_pair(X, 1)).
bachillerato("Artes") :- bachilleratoArtes(C), forall(member(X, C), add_pair(X, 1)).
bachillerato("Sociales") :- bachilleratoSociales(C), forall(member(X, C), add_pair(X, 1)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A más idiomas, darles más puntos.
carreras_de_idiomas(["Filología inglesa", "Ciencias Políticas y de la Administración", "Turismo", "Filología Hispánica"]).
idiomas(X) :-
    X > 2, carreras_de_idiomas(L), forall(member(C, L), add_pair(C, 1)), !.
idiomas(_) :- true.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
resolver_problemas :- 
    add_pair("Matemáticas", 3), add_pair("Física", 3), add_pair("Ingenería Mecánica", 1), add_pair("Química", 1).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
curiosidad_tecnologica :- add_pair("Ingenería Mecánica", 1), add_pair("Ingenería Informática").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
carreras_de_leer(["Filología Hispánica", "Filología Inglesa", "Filosofía"]).
libros_al_mes(X) :- 
    X>2, carreras_de_leer(L), forall(member(C, L), add_pair(C, 2)), !.
libros_al_mes(X) :-
    X>0, carreras_de_leer(L), forall(member(C, L), add_pair(C, 1)), !.
libros_al_mes(_) :- true.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
carreras_de_memorizar(["Historia del Arte", "Historia", "Derecho", "Medicina", "Filosofía"]).
memoria("buena") :- carreras_de_memorizar(L), forall(member(C, L), add_pair(C, 1)).
memoria("regular") :- true.
memoria("mala") :- carreras_de_memorizar(L), forall(member(C, L), add_pair(C, -1)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
carreras_faciles(["Educación Infantil",  "Turismos", "Diseño", "Educación Social"]).
carreras_medio_facil(["Administración y Dirección de Empresas", "Psicología", "Bellas Artes", "Ciencias del mar", "Educación Social"]).
carreras_medio(["Filología Hispánica", "Historia del Arte", "Ciencias Políticas y de la Administración", "Economía", "Ciencia y Tecnología de los Alimentos"]).
carreras_medio_dificl(["Ingenería Informática", "Filología Inglesa", "Ingenería Mecánica", "Historia", "Enfermería", "Comunicaciones y Periodismo", "Arquitectura", "Química", "Relaciones Internacionales"]).
carreras_dificiles(["Física", "Matemáticas", "Derecho", "Medicina", "Ingenería Aeroespacial", "Biología", "Filosofía"])
tiempo_estudio(X) :- 
    X<=1, carreras_faciles(L), forall(member(C, L), add_pair(C, 3)), carreras_medio_facil(L), forall(member(C, L), add_pair(C, 1)),
    carreras_medio_dificl(L), forall(member(C, L), add_pair(C, -1)), carreras_dificiles(L), forall(member(C, L), add_pair(C, -3)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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

