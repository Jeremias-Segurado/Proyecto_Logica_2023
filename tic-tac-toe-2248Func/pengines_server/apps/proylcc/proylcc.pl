:- module(proylcc, 
	[  
		join/4
	]).


use_module(library(random)).

:-dynamic puntaje/1.
rangoMINPotencia(1, 1).
rangoMAXPotencia(1, 8).
puntaje(0).
/**
 * join(+Grid, +NumOfColumns, +Path, -RGrids) 
 * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
 * 
*/
join(Grid, NumOfColumns, Path, RGrids):-    
	sumar_y_agregar(Grid, Path, Grilla0, NuevoPath, 0), 
    GridsAux = [],
	append(GridsAux, [Grilla0], Grids2), 
	join_desp(Grilla0, NumOfColumns, NuevoPath, Grids2, RGrids, 1). 

%TODO OK
join_desp(_, _, _, G1, G1, 0).
join_desp(Grid, NumOfColumns, Path, RGrids, RGridsNew, A):-
	agregar_bloques_nuevos(Grid, Path, NumOfColumns, GridNBloc),
    agregar_grilla(Grid, GridNBloc, RGrids, RGrids1),
	generar_desplazar(GridNBloc, Path, NumOfColumns, GridDes),
    agregar_grilla(GridNBloc,  GridDes, RGrids1, RGrids2),
	subir_path(Path, NumOfColumns,Path2),
    GridNBloc \== GridDes->  %no se desplazo mas 0s
		join_desp(GridDes, NumOfColumns, Path2, RGrids2, RGridsNew, A),
	!
	;
    %Tengo que volver a ejecutar el agregar bloques en la ultima instancia
    %porque al  negarse los cambios del bloque de arriba no se agregaria 
    %la ultima grilla.
    agregar_bloques_nuevos(Grid, Path, NumOfColumns, GridNBloc),
    agregar_grilla(Grid, GridNBloc, RGrids, RGrids1),
	join_desp(_, _, _, RGrids1, RGridsNew, 0).

%FUNCA
agregar_grilla(GrillaVieja, GrillaNueva, LGrillas, LGrillasNueva):-
    GrillaVieja \== GrillaNueva,
    append(LGrillas, [GrillaNueva], LGrillasNueva).
agregar_grilla(GrillaVieja, GrillaNueva, LGrillas, LGrillasNueva):-
    GrillaVieja == GrillaNueva,
    LGrillasNueva = LGrillas.
    
%FUNCA
subir_path([], _, []).
subir_path([H|T], NumOfColumns, [R|Z]):-
	R is H-NumOfColumns,
	subir_path(T, NumOfColumns, Z).
%-----------------------------------------------------
%------------> consult("pengines_server/apps/proylcc/proylcc.pl").
%FUNCA
agregar_bloques_nuevos(G, [], _, G).
agregar_bloques_nuevos(Grilla, Path, NumOfColumns, GridRes):-
	Path = [H|T],
	H > 0,
	H =< NumOfColumns,
	rangoMINPotencia(1, Min),
	rangoMAXPotencia(1, Max),
	bloqueRandom(Min, Max, B),
	replace_nth(H, Grilla, B, GridNew),
	agregar_bloques_nuevos(GridNew, T, NumOfColumns, GridRes)
	;
	Path = [_|T],
	agregar_bloques_nuevos(Grilla, T, NumOfColumns, GridRes),
	!.
%resultadoSuma(+Sum, -Pot).
%Sum: es la suma de las casillas conectadas,
%Pot: es la potencia tal que 2^Pot es la potencia de 2 igual o mayor a Sum.
resultadoSuma(Sum, 2) :- Sum=<4, !.
resultadoSuma(Sum, Pot) :- 
		SumAux is Sum/2,
		resultadoSuma(SumAux, PotAux),
		Pot is PotAux + 1,
		Sum =< 2**Pot,
		!.
%bloqueRandom(+Min, +Max, -Bloque)
%Min: Potencia minima del bloque
%Max: potencia maxima, esta no se incluye en los posibles valores.
%Bloque: es la potencia de 2 tal que 2**Pot, con Pot entre Min y Max.
bloqueRandom(Min, Max, Bloque):-
	random_between(Min, Max, Pot),
	Bloque is 2**Pot.



%replace_nth(+Index, ?Lista1, +Elem, ?Lista2)
%	TRUE si la segunda lista es igual a la primera hasta llegar 
% 	a la posicion Index donde tiene el elemento Elem.
replace_nth(1, [_|T], X, [X|T]).
replace_nth(N, [H|T], X, [H|R]):-
	N>1,
	N1 is N-1,
	replace_nth(N1, T, X, R).

%generar_desplazar(+Grilla1, +Posiciones_a_desplazar, +NumeroDeColumnas -Grilla2).
%	TRUE si la segunda grilla(lista) es igual a la primera con los elementos de las 
%	posiciones cambiadas con los elementos que estan inmediatamente arriba de ellos(En la misma columna).
generar_desplazar(G, [], _, G).
generar_desplazar(G1, [H|T], NumOfColum, G2):-
	H > NumOfColum, %Corroborar, estaria como ->
	Sig is H-NumOfColum,
	nth1(Sig, G1, ElemSig),
	nth1(H, G1, ElemAnt),
	replace_nth(Sig, G1, ElemAnt, G3),
	replace_nth(H, G3, ElemSig, G4),
	generar_desplazar(G4, T,NumOfColum, G2)
	;
	generar_desplazar(G1, T, NumOfColum, G2).
%ToDo:: Sin uso
copy_list([],[]).
copy_list([H|T], [H|Z]):-
	copy_list(T, Z).

%FUNCA
%asume Path valido
sumar_y_agregar(Grilla, Path, GrillaNueva, NuevoPath, Suma):-
	Path = [H|T],
    T \== [],
	NuevoPath = [H|Z],
	nth1(H, Grilla, Elem),
	replace_nth(H, Grilla, 0, G2),
	SumaAux is Suma+Elem,
	sumar_y_agregar(G2, T, GrillaNueva, Z, SumaAux),
    !.
sumar_y_agregar(Grilla, Path, GrillaNueva, [], Suma):-
    Path = [H|T],
    T == [],
	nth1(H, Grilla, Elem),
	SumaAux is Suma+Elem,
	puntaje(P),
	retract(puntaje(P)),
    PN is P+SumaAux,
	assert(puntaje(PN)),
	resultadoSuma(SumaAux, NuevaPot),
    NBloc is 2**NuevaPot, 
	replace_nth(H, Grilla, NBloc, G2),
	GrillaNueva = G2.


sumar_y_agregar(Grilla, Path, NumOfColum, GrillaNueva, NuevoPath, Suma):-
	Path = [H|T],
	NuevoPath = [H|Z],
	nth1(H, Grilla, Elem),
	replace_nth(H, Grilla, 0, G2),
	SumaAux is Suma+Elem,
	sumar_y_agregar(G2, T, NumOfColum, GrillaNueva, Z, SumaAux).
sumar_y_agregar(Grilla, X, NumOfColum, GrillaNueva, [], Suma):-
	number(X),
	nth1(X, Grilla, Elem),
	SumaAux is Suma+Elem,
	puntaje(P),
	retract(punta(P)),
	assert(puntaje(P+SumaAux)),
	resultadoSuma(SumaAux, NuevaPot),
	replace_nth(X, Grilla, NuevaPot, G2),
	GrillaNueva = G2.

