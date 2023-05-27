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
 * %------------> consult("pengines_server/apps/proylcc/proylcc.pl").
*/
%Path: Lista de posiciones dentro de una lista(grilla).
join(Grid, NumOfColumns, Path, RGrids):-  	
	map_path(Path, NumOfColumns, PathFINAL), 
	sumar_y_agregar(Grid, PathFINAL, Grilla0, NuevoPath, 0, Puntaje),
	cambiar_puntaje(Puntaje),
    GridsAux = [],
	append(GridsAux, [Grilla0], Grids2), 
	join_desp(Grilla0, NumOfColumns, NuevoPath, Grids2, RGrids, 1). 

cambiar_puntaje(Puntaje):-
	puntaje(P),
	retract(puntaje(P)),
    PN is P+Puntaje,
	assert(puntaje(PN)).

%join_desp(+Grid, +NumOfColumns, +Path, +RGrids, -RGridsNew, +A)
join_desp(_, _, _, G1, G1, 0). %CAMBIOS
join_desp(Grid, NumOfColumns, Path, RGrids, RGridsNew, A):-
	agregar_bloques_nuevos(Grid, Path, NextPath1, NumOfColumns, GridNBloc),    
	generar_desplazar(GridNBloc, NextPath1, NextPath2, NumOfColumns, GridDes),
	GridNBloc \== GridDes,    
    append(RGrids, [GridDes], RGrids2),
	join_desp(GridDes, NumOfColumns, NextPath2, RGrids2, RGridsNew, A),
	!
	;
    agregar_bloques_nuevos(Grid, Path, _, NumOfColumns, GridNBloc),    
	append( RGrids,[GridNBloc], RGrids2),
	join_desp(_, _, _, RGrids2, RGridsNew, 0).


%subir_path(+Path, +NumOfColumns, -PathNew)
subir_path([], _, []). %DESUSO
subir_path([H|T], NumOfColumns, [R|Z]):-
	R is H-NumOfColumns,
	subir_path(T, NumOfColumns, Z).

%agregar_bloques_nuevos(+Grilla, +Path, +NumOfColumns, -GridRes)
agregar_bloques_nuevos(G, [], [], _, G). %CAMBIOS
agregar_bloques_nuevos(Grilla, Path, NextPath, NumOfColumns, GridRes):-
	Path = [H|T],
	H > 0,
	H =< NumOfColumns,
    NextPos is H-NumOfColumns,
	rangoMINPotencia(1, Min),
	rangoMAXPotencia(1, Max),
	bloqueRandom(Min, Max, B),
	replace_nth(H, Grilla, B, GridNew),
	agregar_bloques_nuevos(GridNew, T, NextPathAux, NumOfColumns, GridRes),
    append([NextPos], NextPathAux, NextPath)
	;
	Path = [H|T],
	agregar_bloques_nuevos(Grilla, T, NextPathAux, NumOfColumns, GridRes),
    append([H], NextPathAux, NextPath).

%resultadoSuma(+Sum, -Pot).
%Sum: es la suma de las casillas conectadas,
%Pot: es la potencia tal que 2^Pot es la potencia de 2 igual o mayor a Sum.
resultadoSuma(Sum, 1) :- Sum=<2, !.
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

%generar_desplazar(+Grilla1, +Posiciones_a_desplazar, -NextPath, +NumeroDeColumnas -Grilla2).
%	TRUE si la segunda grilla(lista) es igual a la primera con los elementos de las 
%	posiciones cambiadas con los elementos que estan inmediatamente arriba de ellos(En la misma columna).
generar_desplazar(G, [], [], _, G). 
generar_desplazar(G1, [H|T], NextPath, NumOfColum, G2):-
	H > NumOfColum, 
	Sig is H-NumOfColum,
	nth1(Sig, G1, ElemSig),
	ElemSig \== 0,
	nth1(H, G1, ElemAnt),
	replace_nth(Sig, G1, ElemAnt, G3),
	replace_nth(H, G3, ElemSig, G4),
	generar_desplazar(G4, T, NextPathAux, NumOfColum, G2),
	append([Sig], NextPathAux, NextPath),
	!
	;
	generar_desplazar(G1, T, NextPathAux, NumOfColum, G2),
	append([H], NextPathAux, NextPath).

%sumar_y_agregar(+Grilla, +Path, -GrillaNueva, -NuevoPath, +Suma, -Puntaje)
%Suma se debe iniciar en 0.
sumar_y_agregar(Grilla, Path, GrillaNueva, NuevoPath, Suma, Puntaje):-
	Path = [H|T],
    T \== [],
	NuevoPath = [H|Z],
	nth1(H, Grilla, Elem),
	replace_nth(H, Grilla, 0, G2),
	SumaAux is Suma+Elem,
	sumar_y_agregar(G2, T, GrillaNueva, Z, SumaAux, Puntaje),
    !.
sumar_y_agregar(Grilla, Path, GrillaNueva, [], Suma, Puntaje):-
    Path = [H|T],
    T == [],
	nth1(H, Grilla, Elem),
	Puntaje is Suma+Elem,
	resultadoSuma(Puntaje, NuevaPot),
    NBloc is 2**NuevaPot, 
	replace_nth(H, Grilla, NBloc, G2),
	GrillaNueva = G2.

ver_siguiente_bloque_SHELL(Grilla, Path, NumOfColumns, BloqueResultado):-
	map_path(Path, NumOfColumns, PathFINAL),
	ver_siguiente_bloque(Grilla, PathFINAL, 0, BloqueResultado).

%ver_siguiente_bloque(+Grilla, +Path, +Suma, -BloqueResultado)                                           
ver_siguiente_bloque(Grilla, [H|T], Suma, BloqueResultado):-
	T == [],
	nth1(H, Grilla, Elem),
	SumaAux is Suma+Elem,
	resultadoSuma(SumaAux, NuevaPot),
	BloqueResultado is 2**NuevaPot.
ver_siguiente_bloque(Grilla, Path,Suma, BloqueResultado):-
	Path = [H|T],
	nth1(H, Grilla, Elem),
	SumaAux is Suma+Elem,
	ver_siguiente_bloque(Grilla, T, SumaAux, BloqueResultado).
 

isValidPos(I, J, NumOfRows, NumOfColumns) :-
	I > 0, J > 0, I =< NumOfRows, J =< NumOfColumns.

%adjacents(+Grid, +NumOfColumns, +NumOfRows, +X, +Y, -List)
adjacents(Grid, NumOfColumns, NumOfRows, X, Y, List) :-
	isValidPos(X, Y, NumOfRows, NumOfColumns),
	Pos1 is (X-1) * NumOfColumns + Y,
	nth1(Pos1, Grid, Elem),
	findall(Pos, 
				(between(-1, 1, DIStep),
				between(-1, 1, DJStep),
				XAdj is X + DIStep,
				YAdj is Y + DJStep,
				isValidPos(XAdj, YAdj, NumOfRows, NumOfColumns),          
				Pos is (XAdj-1) * NumOfColumns + YAdj,          
				nth1(Pos, Grid, Elem2),
				Elem == Elem2), 
			List).

%adjacents_all(+Grid, +Path, +Visitados, +NumOfColumns, +NumOfRows, -RList)
adjacents_all(_, [], _, _, _, []).
adjacents_all(Grid, Path, Visitados, NumOfColumns, NumOfRows, RList):-
	Path = [H|T],
	not(member(H, Visitados)),
	append([H],Visitados, Visitados2),
	PosXAux is H div NumOfColumns,
	PosYAux is H mod NumOfColumns,
	(PosYAux == 0 -> PosY is NumOfColumns; PosY is PosYAux ),
	(PosYAux == 0 -> PosX is PosXAux; PosX is PosXAux+1),
	adjacents(Grid, NumOfColumns, NumOfRows, PosX, PosY, AdjList),
	append(T, AdjList, NewPath),
	adjacents_all(Grid, NewPath, Visitados2, NumOfColumns, NumOfRows, RList2),
	(last(RList2, LastElem), LastElem < H -> append(RList2, [H], RList) ; append([H], RList2, RList)),
	!
	;
	Path = [_|T],
	adjacents_all(Grid, T, Visitados, NumOfColumns, NumOfRows, RList).

%booster(+Grid, +NumOfColumns, -RGrids)
booster(Grid, NumOfColumns, RGrids):- 
	length(Grid, CantElem),
	NumOfRows is CantElem/NumOfColumns,	
	booster_shell(Grid, Grid, 1, [], ListOf0, NumOfColumns, NumOfRows, AuxRGrids),
	last(AuxRGrids, Grid0),
	write(ListOf0),
	join_desp(Grid0, NumOfColumns, ListOf0, AuxRGrids, RGrids, 1).
	
%(+Grid, +Index, +Visitados, -ListOf0, +NumOfColumns, +NumOfRows, -RGrids).
booster_shell(Grid, _, Index, _, [], _, _,[]):- %CAMBIOS
	length(Grid, CantElem),
	Index > CantElem.
booster_shell(Grid, GridNew, Index, Visitados, ListOf0, NumOfColumns, NumOfRows, RGrids):-
	not(member(Index, Visitados)),
	Index2 is Index + 1,
	adjacents_all(Grid, [Index], [], NumOfColumns, NumOfRows, AdjPath),
	append(AdjPath, Visitados, VisitadosNew),
	sumar_y_agregar(GridNew, AdjPath, GridOf0, PathOf0, 0, _),
	booster_shell(Grid, GridOf0, Index2, VisitadosNew, ListOf0New, NumOfColumns, NumOfRows, RGridsNew),	
    append(PathOf0, ListOf0New, ListOf0),
	append([GridOf0], RGridsNew, RGrids),
	!
	;   
	Index2 is Index+1,
	booster_shell(Grid, GridNew, Index2, Visitados, ListOf0, NumOfColumns, NumOfRows, RGrids).


%map_path(+PathOrigin, +NumOfColumns, -PathFINAL)
map_path([], _, []).
map_path(PathOrigin, NumOfColumns, PathFINAL):-
	PathOrigin = [H|T],
	H = [X|Y],
	Pos is X * NumOfColumns + Y + 1,
	map_path(T, NumOfColumns, PathFINALFINAL),
	append([Pos], PathFINALFINAL, PathFINAL).
