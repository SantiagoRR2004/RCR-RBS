% Si ponemos dynamic significa que podemos cambiar la base de conocimiento en tiempo de ejecución.
:- dynamic pair/2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Esto es para que sepa que carreras puede elegir con su nota.

% Aquí ponemos las carreras y las notas de corte.

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

% Si tiene más nota se le suma un punto.
% Si tiene menos nota se le resta la diferencia.
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
% El forall itera sobre todas las carreras y notas de corte.
% Ponemos el true para que no haya un fallo si no se cumple la condición.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Esto es por el tipo de bachillerato.

% cada tipo de bachillerato es un conjunto representado como una lista.
% Estos conjuntos no son disjuntos. Tienen elementos en común.
% Si haces 2 tipos de bachillerato las carreras en común contarán el doble
bachilleratoCiencias(["Medicina", "Matemáticas", "Física", "Psicología", "Enfermería", "Biología", "Ciencias de la Actividad Física y del Deporte", "Química", "Ciencia y Tecnología de los Alimentos", "Ciencias del mar", "Economía", "Arquitectura"]).
bachilleratoTech(["Matemáticas", "Física", "Ingeniería Aeroespacial", "Ingeniería Informática", "Ingeniería Mecánica", "Arquitectura"]).
bachilleratoLetras(["Comunicaciones y Periodismo", "Filología Hispánica", "Filosofía", "Filología Inglesa"]).
bachilleratoArtes(["Diseño", "Historia del Arte", "Bellas Artes", "Arquitectura"]).
bachilleratoSociales(["Comunicaciones y Periodismo", "Relaciones Internacionales", "Educación Infantil", "Historia", "Derecho", "Filosofía", "Educación Social", "Historia del Arte", "Administración y Dirección de Empresas", "Economía", "Ciencias Políticas y de la Administración", "Turismo"]).

bachillerato("Ciencias") :- bachilleratoCiencias(C), forall(member(X, C), add_pair(X, 2)).
bachillerato("Tecnología") :- bachilleratoTech(C), forall(member(X, C), add_pair(X, 2)).
bachillerato("Letras") :- bachilleratoLetras(C), forall(member(X, C), add_pair(X, 2)).
bachillerato("Artes") :- bachilleratoArtes(C), forall(member(X, C), add_pair(X, 2)).
bachillerato("Sociales") :- bachilleratoSociales(C), forall(member(X, C), add_pair(X, 2)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% La cantidad de idiomas que habla el usuario.

% Las carreras en las que viene bien hablar varios idiomas se guardan en una lista.
% Si habla más de dos idiomas, le suma un punto a las carreras de dicha lista.
carreras_de_idiomas(["Filología Inglesa", "Ciencias Políticas y de la Administración", "Turismo", "Filología Hispánica"]).
idiomas(X) :-
    X > 2, carreras_de_idiomas(L), forall(member(C, L), add_pair(C, 1)), !.
idiomas(_) :- true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Aquí tenemos en cuenta el afán de realizar problemas matemáticos del usuario.

% Si tiene dicho afán, le sumamos 3 puntos a Matemáticas y Física, puesto que estas carreras se centran en resolver problemas.
% Se le suma un punto a Ingeniería Mecánica y Química porque también hay problemas, pero menos.
resolver_problemas :- 
    add_pair("Matemáticas", 3), add_pair("Física", 3), add_pair("Ingeniería Mecánica", 1), add_pair("Química", 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Con esta regla se atiende a la pregunta de si al usuario le interesa saber cómo funciona la tecnología de uso diario.

% Le sumamos un punto a las ingerías.
curiosidad_tecnologica :- add_pair("Ingeniería Mecánica", 1), add_pair("Ingeniería Informática", 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Queremos ver el gusto por la lectura del usuario.

% En una lista almacenamos las carreras en las que tendrá que leer varios libros.
% Si lee 3 o más libros al mes, le sumamos 2 puntos a estas carreras.
% De 1 a 2 libros al mes, un punto sólo.
% Cero libros, nada. Devuelve true, para que si lee ceros libros prolog no devuelva false.
carreras_de_leer(["Filología Hispánica", "Filología Inglesa", "Filosofía"]).
libros_al_mes(X) :- 
    X>2, carreras_de_leer(L), forall(member(C, L), add_pair(C, 2)), !.
libros_al_mes(X) :-
    X>0, carreras_de_leer(L), forall(member(C, L), add_pair(C, 1)), !.
libros_al_mes(_) :- true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Atendemos a la memoria que tiene el usuario.

% Como en otras reglas, guardamos en una lista las carreras para las que se necesita una buena memoria.
% Por lo tanto, si tiene buena memoria, se le añade un punto a estas carreras.
% Si es regular, no hace nada. (true para que no salga false por terminal).
% Si es mala, dado que no sería recomendable que vaya a estas carreras, se les resta un punto.
carreras_de_memorizar(["Historia del Arte", "Historia", "Derecho", "Medicina", "Filosofía"]).
memoria("buena") :- carreras_de_memorizar(L), forall(member(C, L), add_pair(C, 1)).
memoria("regular") :- true.
memoria("mala") :- carreras_de_memorizar(L), forall(member(C, L), add_pair(C, -1)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Con estas reglas vamos a darle valores a todas las carreras dependiendo del tiempo para estudiar que tiene el individuo.

% Separamos todas las carreas en cinco listas disjuntas según el tiempo que creemos que necesitaría una persona media para aprobar.
% Van de menos tiempo necesario a más.

carreras_faciles(["Educación Infantil",  "Turismo", "Diseño", "Educación Social"]).
carreras_medio_facil(["Administración y Dirección de Empresas", "Psicología", "Bellas Artes", "Ciencias del mar"]).
carreras_medio(["Filología Hispánica", "Historia del Arte", "Ciencias Políticas y de la Administración", "Economía", "Ciencia y Tecnología de los Alimentos"]).
carreras_medio_dificl(["Ingeniería Informática", "Filología Inglesa", "Ingeniería Mecánica", "Historia", "Enfermería", "Comunicaciones y Periodismo", 
    "Arquitectura", "Química", "Relaciones Internacionales"]).
carreras_dificiles(["Física", "Matemáticas", "Derecho", "Medicina", "Ingeniería Aeroespacial", "Biología", "Filosofía"]).

% La primera regla de tiempo de estudio atiende a quien tiene menos de una hora de estudio. Como tiene poco tiempo se le da tres puntos
% a las "faciles", pues creemos que el tiempo es un factor de peso. A las medio faciles aunque no serían tan recomendables, puede llegar a sacarlas,
% entonces se le da un punto. Al final se añade un corte para que no vaya al resto de reglas, 
% dado que cero cumple la condición de este regla y las dos seguientes.
tiempo_estudio(X) :- 
    X<1, carreras_faciles(L1), forall(member(C1, L1), add_pair(C1, 3)), carreras_medio_facil(L2), forall(member(C2, L2), add_pair(C2, 1)),
    carreras_medio_dificl(L3), forall(member(C3, L3), add_pair(C3, -2)), carreras_dificiles(L4), forall(member(C4, L4), add_pair(C4, -3)), !.

% La siguiente regla de tiempo de estudio es para si tiene entre 1 y 2 horas diarias.
tiempo_estudio(X) :- 
    X<3, carreras_faciles(L1), forall(member(C1, L1), add_pair(C1, 2)), carreras_medio_facil(L2), forall(member(C2, L2), add_pair(C2, 3)),
    carreras_medio_dificl(L3), forall(member(C3, L3), add_pair(C3, -1)), carreras_dificiles(L4), forall(member(C4, L4), add_pair(C4, -3)), !.

% Después entre 3 y 4 horas.
tiempo_estudio(X) :- 
    X<5, carreras_faciles(L1), forall(member(C1, L1), add_pair(C1, 1)), carreras_medio_facil(L2), forall(member(C2, L2), add_pair(C2, 2)),
    carreras_medio_dificl(L3), forall(member(C3, L3), add_pair(C3, 3)), carreras_dificiles(L4), forall(member(C4, L4), add_pair(C4, -1)), !.

% La últimas, más de cinco horas.
tiempo_estudio(X) :- 
    X>4, carreras_faciles(L1), forall(member(C1, L1), add_pair(C1, -1)), carreras_medio_facil(L2), forall(member(C2, L2), add_pair(C2, 1)),
    carreras_medio_dificl(L3), forall(member(C3, L3), add_pair(C3, 2)), carreras_dificiles(L4), forall(member(C4, L4), add_pair(C4, 3)), !.

% Al final se ejecuta una sóla regla que le dará unos valores a cada tipo de carrera dependiendo del tiempo que tengas y nuestro criterio subjetivo.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Miramos si el usuario podría trabajar en un futuro atendiendo a personas.

% En las carreras cuyo trabajo la persona interactúe con personas, se almacenan en una lista
% Si es apto, el usuario escribe publico. se recorre la lista con el forall y se le añade un punto a estas carreras.
carreras_frente_publico(["Psicología", "Educación Infantil", "Turismo", "Medicina", "Enfermería", "Comunicaciones y Periodismo",
    "Educación Social", "Relaciones Internacionales", "Ciencias de la Actividad Física y del Deporte"]).
publico :- carreras_frente_publico(L), forall(member(C, L), add_pair(C, 1)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Aquí no solo si puede trabajar con público, sino también si le gusta ayudar.

% Una vez más, se almacenan dichas carreras en las que en un futuro ayudas a gente.
% Se recorre la lista y se añade un punto a las carreras de ayudar.
carreras_de_ayudar(["Psicología", "Educación Infantil", "Medicina", "Enfermería", "Educación Social","Ciencias de la Actividad Física y del Deporte"]).
ayudar :- carreras_de_ayudar(L), forall(member(C, L), add_pair(C, 1)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Este apartado está orientado a probar si se tiene facilidad para el arte, que aprecia la belleza.

% Se almacenan carreras de arte en la lista y se les da un punto, si el usuario aprecia la belleza.
carreras_de_apreciar_belleza(["Bellas Artes", "Historia del Arte", "Diseño"]).
belleza :- carreras_de_apreciar_belleza(L), forall(member(C, L), add_pair(C, 1)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Si el usuario tiene imaginación se le da un punto a estas tres carreras.

% Es necesaria la imaginación para hacer algo artístico o imaginar una estructura 3D apartir de un plano 2D (Arquitectura)
carreras_de_imaginacion(["Bellas Artes", "Diseño", "Arquitectura"]).
imaginativo :- carreras_de_imaginacion(L), forall(member(C, L), add_pair(C, 1)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Para ciertas carreras se necesita poder empatizar y comprender los sentimientos del otro con el fin de poderlo orientarlo.

% Por ello se les da un punto a las carreras en las que tienes la responsabilidad de atender y orientar a alguien
carreras_de_empatia(["Psicología", "Educación Infantil", "Educación Social"]).
empatico :- carreras_de_empatia(L), forall(member(C, L), add_pair(C, 1)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Observar si la persona podría debatir y defender sus ideas.

% En estas carreras la mejor forma de aprender es exponiéndote a las ideas contrarias de los demás
carreras_de_debate(["Economía", "Filosofía", "Ciencias Políticas y de la Administración"]).
debatir :- carreras_de_debate(L), forall(member(C, L), add_pair(C, 1)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% En este apartado se valora la ambición

% Aquí tenemos carreras que la gente asocia a personas reputadas en la sociedad:
% Matemáticos, empresarios, abogados, médicos, políticos e ingenieros.
carreras_de_ambicion(["Matemáticas", "Administración y Dirección de Empresas", "Derecho", "Medicina", 
    "Ingeniería Aeroespacial", "Ciencias Políticas y de la Administración"]).
ambicioso :- carreras_de_ambicion(L), forall(member(C, L), add_pair(C, 1)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Muchos estudiantes en su tiempo libre dan clases particulares para ganar dinero.
% Esto puede ser buena referencia para orientarlos en su futuro.

% Sumamos un punto a las carreras con las que puedes ser profesor.
carreras_de_educacion(["Matemáticas", "Física", "Educación Infantil", "Filología Hispánica", "Filología Inglesa", "Historia",
    "Economía", "Química", "Educación Social", "Filosofía", "Biología"]).
profesor :- carreras_de_educacion(L), forall(member(C, L), add_pair(C, 1)), add_pair("Educación Infantil", 1), add_pair("Educación Social", 1).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Algunas carreras te obligan a mantenerte informado de la actualidad económica y política.

% Si el usuario ya tiene la costumbre de informarse, se le suma un punto a estas carreras.
carreras_de_actualidad(["Economía", "Filosofía", "Comunicaciones y Periodismo", "Ciencias Políticas y de la Administración"]).
informado :- carreras_de_actualidad(L), forall(member(C, L), add_pair(C, 1)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Si estás interesado en cuidar tu salud, tal vez te interese estas carreras.

% Le damos un punto a las carreras relacionadas con el deporte y la alimentación.
sano :- add_pair("Ciencias de la Actividad Física y del Deporte", 1), add_pair("Ciencia y Tecnología de los Alimentos", 1).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Un punto a carreras en las no tengas que relacionarte con nadie, un historiador o artista solitario.
independiente :- add_pair("Bellas Artes", 1), add_pair("Historia", 1).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Carreras en las que realizas experimentos en un laboratorio.
carreras_de_laboratorio(["Física", "Química", "Biología"]).
investigador :- carreras_de_laboratorio(L), forall(member(C, L), add_pair(C, 1)).
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Aquí eliminamos todas las parejas de la base de conocimiento

% retractall es un predicado que se utiliza para eliminar todas las instancias de un término que satisfacen un objetivo dado.
clear :- retractall(pair(_, _)). % Esto es para limpiar la base de conocimiento

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Con esto le damos al usuario el enlace a la documentación
:- initialization(writef("Si tiene dudas de como usar este programa vaya a https://docs.google.com/document/d/1fYjPJnBXXabiXbtqP8fNKxdZ-X9CXQ14PZGefzGl370")).
% initialization es un predicado que se ejecuta al inicio del programa
