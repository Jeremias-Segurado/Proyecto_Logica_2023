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
*/
join(Grid, NumOfColumns, Path, RGrids):-  	
	mapear_path(Path, NumOfColumns, PathFINAL), 
	sumar_y_agregar(Grid, PathFINAL, Grilla0, NuevoPath, 0, Puntaje),
	cambiar_puntaje(Puntaje),
    GridsAux = [],
	append(GridsAux, [Grilla0], Grids2), 
	desplazar(Grilla0, NumOfColumns, NuevoPath, Grids2, RGrids, 1). 

/**
 * cambiar_puntaje(-Puntaje)
 * Cambia el predicado asociado al puntaje sumando el anterior con el nuevo. Siempre es verdadero este predicado.
 */
cambiar_puntaje(Puntaje):-
	puntaje(P),
	retract(puntaje(P)),
    PN is P+Puntaje,
	assert(puntaje(PN)).

%desplazar(+Grid, +NumOfColumns, +Path, +RGrids, -RGridsNew, +A)
/**
 * desplazar(+Grid, +NumOfColumns, +Path, +RGrids, -RGridsNew, +A)
 * desplaza los elementos ubicados en las posiciones indicadas por el Path hacia arriba del todo agregando
 * una Grilla nueva por cada iteracion del desplazamiento y cada vez que los elementos llegan a la primera fila, estos son
 * reemplazados por nuevos valores aleatorios que corresponden a una potencia de 2. Se utiliza un metodo de corte empleando
 * la entrada A, cuando esta es 0 se llega al caso base.
 */ 
desplazar(_, _, _, G1, G1, 0). 
desplazar(Grid, NumOfColumns, Path, RGrids, RGridsNew, A):-
	agregar_bloques_nuevos(Grid, Path, NextPath1, NumOfColumns, GridNBloc),    
	generar_desplazamientos(GridNBloc, NextPath1, NextPath2, NumOfColumns, GridDes),
	GridNBloc \== GridDes,    
    append(RGrids, [GridDes], RGrids2),
	desplazar(GridDes, NumOfColumns, NextPath2, RGrids2, RGridsNew, A),
	!
	;
    agregar_bloques_nuevos(Grid, Path, _, NumOfColumns, GridNBloc),    
	append( RGrids,[GridNBloc], RGrids2),
	desplazar(_, _, _, RGrids2, RGridsNew, 0).

/**
 * agregar_bloques_nuevos(+Grilla, +Path, 'NextPath +NumOfColumns, -GridRes)
 * Chequea las posiciones en la grilla indicadas en el Path que se encuentren en la primera fila, reemplazando
 * los elementos de dicha ubicacion por potencias de 2 aleatorian entre un rango definido en predicados declarados arriba.
 * A su vez, cuando se realiza el cambio, se modifica dicha ubicacion marcada por el Path para que se ignore.
 */
agregar_bloques_nuevos(G, [], [], _, G). 
agregar_bloques_nuevos(Grilla, Path, NextPath, NumOfColumns, GridRes):-
	Path = [H|T],
	H > 0,
	H =< NumOfColumns,
    NextPos is H-NumOfColumns,
	rangoMINPotencia(1, Min),
	rangoMAXPotencia(1, Max),
	bloque_random(Min, Max, B),
	reemplazar_lista(H, Grilla, B, GridNew),
	agregar_bloques_nuevos(GridNew, T, NextPathAux, NumOfColumns, GridRes),
    append([NextPos], NextPathAux, NextPath)
	;
	Path = [H|T],
	agregar_bloques_nuevos(Grilla, T, NextPathAux, NumOfColumns, GridRes),
    append([H], NextPathAux, NextPath).

/**
 * resultado_suma(+Sum, -Pot).
 * Asocia la menor Potencia necesaria para que el numero Sum sea
 * menor o igual a 2 elevado dicho potencia.
 */ 
resultado_suma(Sum, 1) :- Sum=<2, !.
resultado_suma(Sum, Pot) :- 
        SumAux is Sum/2,
        resultado_suma(SumAux, PotAux),
        Pot is PotAux + 1,
        Sum =< 2**Pot,
        !.

/**
 * bloque_random(+Min, +Max, -Bloque)
 * Retorna una potencia de 2, cuyo exponente se encuentra entre el rango [Min; Max] sin incluir el valor de Max.
 */ 
bloque_random(Min, Max, Bloque):-
	random_between(Min, Max, Pot),
	Bloque is 2**Pot.

/**
 * reemplazar_lista(+Index, ?Lista1, +Elem, ?Lista2)
 * Itera sobre 2 listas desde el primer elemento (posicion 1) hasta N, en esa ubicacion la segunda lista contiene el valor X,
 * para el resto de elementos ambas listas son identicas.
 */ 
reemplazar_lista(1, [_|T], X, [X|T]).
reemplazar_lista(N, [H|T], X, [H|R]):-
	N>1,
	N1 is N-1,
	reemplazar_lista(N1, T, X, R).

/**
 * generar_desplazamientos(+Grilla1, +Posiciones_a_desplazar, -NextPath, +NumeroDeColumnas -Grilla2)
 * Desplaza los elementos ubicados en las posiciones marcadas por el Path hacia la posicion de la fila de arriba, 
 * si en la posicion siguiente se encuentra el elemento 0 o esta ubicacion excede los limites de la grilla no se 
 * efectua desplazamiento.
 * A su vez, si se efectua desplazamiento, se cambia la ubicacion del elemento indicada por el Path por su nueva ubicaion.  
 */ 
generar_desplazamientos(G, [], [], _, G). 
generar_desplazamientos(G1, [H|T], NextPath, NumOfColum, G2):-
	H > NumOfColum, 
	Sig is H-NumOfColum,
	nth1(Sig, G1, ElemSig),
	ElemSig \== 0,
	nth1(H, G1, ElemAnt),
	reemplazar_lista(Sig, G1, ElemAnt, G3),
	reemplazar_lista(H, G3, ElemSig, G4),
	generar_desplazamientos(G4, T, NextPathAux, NumOfColum, G2),
	append([Sig], NextPathAux, NextPath),
	!
	;
	generar_desplazamientos(G1, T, NextPathAux, NumOfColum, G2),
	append([H], NextPathAux, NextPath).

/**
 * sumar_y_agregar(+Grilla, +Path, -GrillaNueva, -NuevoPath, +Suma, -Puntaje)
 * Suma los elementos de la grilla ubicados en las posiciones indicadas por el Path generando una potencia de 2 acorde a la suma y ubicandola
 * en la posicion de la grilla indicada por el ultimo valor del Path, en el resto de ubicaciones se reemplazan los elementos por 0.
 * Retorna dicha suma como el Puntaje y modifica el Path quitandole su ultimo elemento. Para iterar correctamente, se necesita llamar este predicado
 * con el valor de Suma en 0.
 */ 
sumar_y_agregar(Grilla, Path, GrillaNueva, NuevoPath, Suma, Puntaje):-
	Path = [H|T],
    T \== [],
	NuevoPath = [H|Z],
	nth1(H, Grilla, Elem),
	reemplazar_lista(H, Grilla, 0, G2),
	SumaAux is Suma+Elem,
	sumar_y_agregar(G2, T, GrillaNueva, Z, SumaAux, Puntaje),
    !.
sumar_y_agregar(Grilla, Path, GrillaNueva, [], Suma, Puntaje):-
    Path = [H|T],
    T == [],
	nth1(H, Grilla, Elem),
	Puntaje is Suma+Elem,
	resultado_suma(Puntaje, NuevaPot),
    NBloc is 2**NuevaPot, 
	reemplazar_lista(H, Grilla, NBloc, G2),
	GrillaNueva = G2.

/**
 * siguiente_bloque(+Grilla, +Path, +NumOfColumns, -BloqueResultado)
 * Predicado SHELL que se encarga de mapear el Path convirtiendosn las ubicaciones [Fila|Columna] en posiciones dentro de una lista,
 * con primer elemento 1; Luego calcula cual seria la potencia de 2 acorde a la suma de los elementos de la grilla ubicados en las posiciones
 * indicadas por el Path mapeado.
 */ 
siguiente_bloque(Grilla, Path, NumOfColumns, BloqueResultado):-
	mapear_path(Path, NumOfColumns, PathFINAL),
	ver_siguiente_bloque(Grilla, PathFINAL, 0, BloqueResultado).

/**
 * ver_siguiente_bloque(+Grilla, +Path, +Suma, -BloqueResultado)
 * Calcula la menor potencia de 2 mayor o igual a la suma de los elementos de la grilla ubicados en las posiciones indicadas por 
 * el Path. 
 */                                           
ver_siguiente_bloque(Grilla, [H|T], Suma, BloqueResultado):-
	T == [],
	nth1(H, Grilla, Elem),
	SumaAux is Suma+Elem,
	resultado_suma(SumaAux, NuevaPot),
	BloqueResultado is 2**NuevaPot.
ver_siguiente_bloque(Grilla, Path,Suma, BloqueResultado):-
	Path = [H|T],
	nth1(H, Grilla, Elem),
	SumaAux is Suma+Elem,
	ver_siguiente_bloque(Grilla, T, SumaAux, BloqueResultado).
 
/**
<<<<<<< HEAD
 * checkPosition(+Fila, +Columna, +NumOfRows, +NumOfColumns)
 * TRUE si la ubicacion (Fila, Columna) corresponde a la de una grilla de primer elemento (1, 1)
=======
 * check_position(+Fila, +Columna, +NumOfRows, +NumOfColumns)
 * TRUE si la ubicacion (Fila, Columna) corresponde a la de una grilla de primer elemento (0, 0)
>>>>>>> 3dea4b4a1120b3236d77d9ce1c2acadf9a28390c
 * y con cierta cantidad de filas(NumOfRows) y de columnas(NumOfColumns).
 */ 
check_position(I, J, NumOfRows, NumOfColumns) :-
	I > 0, J > 0, I =< NumOfRows, J =< NumOfColumns.

/**
 * adyacentes(+Grid, +NumOfColumns, +NumOfRows, +Fila, +Columna, -Lista_de_abyacentes)
 * Arma una lista con las posiciones de los elementos abyacentes y del elemento obicado en (Fila, Columna) dentro de la grilla.
 * Dichos elementos deben ser iguales.
 */ 
adyacentes(Grid, NumOfColumns, NumOfRows, X, Y, List) :-
	check_position(X, Y, NumOfRows, NumOfColumns),
	Pos1 is (X-1) * NumOfColumns + Y,
	nth1(Pos1, Grid, Elem),
	findall(Pos, 
				(between(-1, 1, DIStep),
				between(-1, 1, DJStep),
				XAdj is X + DIStep,
				YAdj is Y + DJStep,
				check_position(XAdj, YAdj, NumOfRows, NumOfColumns),          
				Pos is (XAdj-1) * NumOfColumns + YAdj,          
				nth1(Pos, Grid, Elem2),
				Elem == Elem2), 
			List).

/**
 * adyacentes_path(+Grid, +Path, +Visitados, +NumOfColumns, +NumOfRows, -RList)
 * Recorre los elementos ubicados en las posicione sindicadas por el Path buscando sus abyacentes que sean iguales,
 * luego repite sobre estos elementos encontrados armando una lista con la ubicacion de todos los elementos de la grilla que
 * sean iguales y se encuentren a una distancia de 1. Utiliza una lista de visitados para recordar cuales fueron las posiciones ya iteradas
 * y asi evitar repetir elementos.
 */ 
adyacentes_path(_, [], _, _, _, []).
adyacentes_path(Grid, Path, Visitados, NumOfColumns, NumOfRows, RList):-
	Path = [H|T],
	not(member(H, Visitados)),
	append([H],Visitados, Visitados2),
	PosXAux is H div NumOfColumns,
	PosYAux is H mod NumOfColumns,
	(PosYAux == 0 -> PosY is NumOfColumns; PosY is PosYAux ),
	(PosYAux == 0 -> PosX is PosXAux; PosX is PosXAux+1),
	adyacentes(Grid, NumOfColumns, NumOfRows, PosX, PosY, AdjList),
	append(T, AdjList, NewPath),
	adyacentes_path(Grid, NewPath, Visitados2, NumOfColumns, NumOfRows, RList2),
	(last(RList2, LastElem), LastElem < H -> append(RList2, [H], RList) ; append([H], RList2, RList)),
	!
	;
	Path = [_|T],
	adyacentes_path(Grid, T, Visitados, NumOfColumns, NumOfRows, RList).

/**
 * booster(+Grid, +NumOfColumns, -RGrids)
 * Predicado SHELL el cual se encarga de preparar el booster y luego generar los bloques nuevos y desplazar los 0s generados.
 */ 
booster(Grid, NumOfColumns, RGrids):- 
	length(Grid, CantElem),
	NumOfRows is CantElem/NumOfColumns,
	booster_shell(Grid, Grid, 1, [], ListOf0, NumOfColumns, NumOfRows, AuxRGrids),
	(AuxRGrids \== []->
		last(AuxRGrids, Grid0), 
		desplazar(Grid0, NumOfColumns, ListOf0, AuxRGrids, RGrids, 1)
		; append([Grid], [], RGrids)).

/**
 * booster_shell(Grid, GridNew, Index, Visitados, ListOf0, NumOfColumns, NumOfRows, RGrids).
 * Recorre cada elemento de la grilla buscando los elementos iguales a distancia 1 sumando y generando la potencia de 2
 * correspondiente colocandola en la ubicacion mas abajo a la derecha de entre las ubicaciones de los elementos sumados, al resto
 * de elementos sumados se los reemplaza por ceros.
 * Por cada iteracion se genera una grilla nueva y se agrega a la lista de grillas RGrids y se actualiza una lista de visitados
 * la cual indica cuales ya fueron utilizados en alguna suma para ignorarlos.
 */ 
booster_shell(Grid, _, Index, _, [], _, _,[]):- 
	length(Grid, CantElem),
	Index > CantElem.
booster_shell(Grid, GridNew, Index, Visitados, ListOf0, NumOfColumns, NumOfRows, RGrids):-
	not(member(Index, Visitados)),
	Index2 is Index + 1,
	adyacentes_path(Grid, [Index], [], NumOfColumns, NumOfRows, AdjPath),
	append(AdjPath, Visitados, VisitadosNew),
	(AdjPath \== [] ->
	sumar_y_agregar(GridNew, AdjPath, GridOf0, PathOf0, 0, _)
	; PathOf0 = [], GridOf0 = GridNew),
	booster_shell(Grid, GridOf0, Index2, VisitadosNew, ListOf0New, NumOfColumns, NumOfRows, RGridsNew),
	append(PathOf0, ListOf0New, ListOf0),
	(GridOf0 \== GridNew -> append([GridOf0], RGridsNew, RGrids); RGrids = RGridsNew) 
	;
	Index2 is Index+1,
	booster_shell(Grid, GridNew, Index2, Visitados, ListOf0, NumOfColumns, NumOfRows, RGrids).

/**
 * mapear_path(+PathOrigin, +NumOfColumns, -PathMapeado)
 * Mapea los elementos del Path definidos como [Fila, Columna] dentro de una grilla a un indice de una lista con primer elemento 1.
 */ 
mapear_path([], _, []).
mapear_path(PathOrigin, NumOfColumns, PathMapeado):-
	PathOrigin = [H|T],
	H = [X|Y],
	Pos is X * NumOfColumns + Y + 1,
	mapear_path(T, NumOfColumns, PathAux),
	append([Pos], PathAux, PathMapeado).


%-----------------------Boton max camino-------------------------------------

adyacentes_mayores_o_iguales(Grid, NumOfColumns, NumOfRows, X, Y, List) :-
	check_position(X, Y, NumOfRows, NumOfColumns),
	Pos1 is (X-1) * NumOfColumns + Y,
	nth1(Pos1, Grid, Elem),
	findall(Pos, 
				(between(-1, 1, DIStep),
				between(-1, 1, DJStep),
				XAdj is X + DIStep,
				YAdj is Y + DJStep,
				check_position(XAdj, YAdj, NumOfRows, NumOfColumns),          
				Pos is (XAdj-1) * NumOfColumns + YAdj,          
				nth1(Pos, Grid, Elem2),
				Elem3 is Elem*2,
				(Elem2 == Elem; Elem2 == Elem3)), 
			List).

adyacentes_mejor_path(_, [], _, _, _, 0, []).
adyacentes_mejor_path(Grid, Path, Visitados, NumOfColumns, NumOfRows, Mejor_Suma, RList):-
	Path = [H|T],
	not(member(H, Visitados)),
	append([H],Visitados, Visitados2),
	PosXAux is H div NumOfColumns,
	PosYAux is H mod NumOfColumns,
	(PosYAux == 0 -> PosY is NumOfColumns; PosY is PosYAux ),
	(PosYAux == 0 -> PosX is PosXAux; PosX is PosXAux+1),
	adyacentes_mayores_o_iguales(Grid, NumOfColumns, NumOfRows, PosX, PosY, AdjList),
	adyacentes_mejor_path(Grid, AdjList, Visitados2, NumOfColumns, NumOfRows, Mejor_Suma_1, RList_1),
	adyacentes_mejor_path(Grid, T, Visitados2, NumOfColumns, NumOfRows, Mejor_Suma_2, RList_2),
	nth1(H, Grid, Elem),
	Suma_H is Elem + Mejor_Suma_1,
	resultado_suma(Suma_H, Pot_1),
	resultado_suma(Mejor_Suma_2, Pot_2),
	(Pot_1 >= Pot_2 -> append([H], RList_1, RList), Mejor_Suma is Suma_H;  
	RList = RList_2, Mejor_Suma is Mejor_Suma_2)
	;   
	Path = [_|T],
	adyacentes_mejor_path(Grid, T, Visitados, NumOfColumns, NumOfRows, Mejor_Suma, RList).

movida_maxima(Grid, _, _, Index, OldPath, PathResultado) :-
	length(Grid, Largo),
	Index > Largo,
	PathResultado = OldPath.
movida_maxima(Grid, NumOfColumns, NumOfRows, Index, OldPath, PathResultado) :-
	PosXAux is Index div NumOfColumns,
	PosYAux is Index mod NumOfColumns,
	(PosYAux == 0 -> PosY is NumOfColumns; PosY is PosYAux ),
	(PosYAux == 0 -> PosX is PosXAux; PosX is PosXAux+1),
	adyacentes(Grid, NumOfColumns, NumOfRows, PosX, PosY, AdjList),
	length(AdjList, LargoAdj),
	LargoAdj > 1,
	adyacentes_mejor_path(Grid, AdjList, [Index], NumOfColumns, NumOfRows, Mejor_Suma, Nuevo_Path),
	nth1(Index, Grid, Elem),
	Suma_Index is Elem + Mejor_Suma,
	ver_siguiente_bloque(Grid, OldPath, 0, BloqueResultado),
	resultado_suma(Suma_Index, PotSuma),
	(2**PotSuma >= BloqueResultado ->   
		append([Index], Nuevo_Path, Path_Grande),
		Index_New is Index+1,
		movida_maxima(Grid, NumOfColumns, NumOfRows, Index_New, Path_Grande, PathResultado)
		;
		Index_New is Index+1,
		movida_maxima(Grid, NumOfColumns, NumOfRows, Index_New, OldPath, PathResultado))
	;   
	Index_New is Index+1,
	movida_maxima(Grid, NumOfColumns, NumOfRows, Index_New, OldPath, PathResultado).

ayuda_movida_maxima(Grid, NumOfColumns, RPath) :-
	length(Grid, CantElem),
	NumOfRows is CantElem/NumOfColumns,
	movida_maxima(Grid, NumOfColumns, NumOfRows, 1, [1], RPathAux),
	mapear_path_a_coordenadas(RPathAux, NumOfColumns, RPath).


mapear_path_a_coordenadas([],_,[]).
mapear_path_a_coordenadas(IndexPath, NumOfColumns, PathMapeado) :-
    IndexPath = [H|T],
    RealIndex is H - 1,
    PosX is RealIndex div NumOfColumns,
	PosY is RealIndex mod NumOfColumns,
    mapear_path_a_coordenadas(T, NumOfColumns, PathMapeadoAux),
    append([[PosX,PosY]], PathMapeadoAux, PathMapeado).

%-------------------------------------------------------------


%---------------boton maximo adyacente------------------------
/*
ayuda_maximos_iguales_adyacentes(Grid, NumOfColumns, RPath):-
	listar_bloques_iguales_mayorAmenor(Grid, 1, RLists),


	
listar_bloques_iguales_mayorAmenor(Grid, RLists):-
	metodo_burbuja(),
	crear_listas_iguales(0, ListSort, 1, RLists).
*/
%FUNCA
crear_listas_iguales(IndexElemAnt, ListSort, Index, [[IndexElemAnt]]):-
	length(ListSort, Long),
	Index > Long.
crear_listas_iguales(IndexElemAnt, ListSort, Index, RLists):-
	nth1(Index, ListSort, Elem),
    nth1(IndexElemAnt, ListSort, ElemAnt),
	(ElemAnt == Elem->
		Index2 is Index + 1,
		crear_listas_iguales(Index, ListSort, Index2, RListsAux),
		RListsAux = [H|T],		
		append([IndexElemAnt], H, SameList),
		append([SameList], T, RLists)
	;
		Index2 is Index + 1,
		crear_listas_iguales(Index, ListSort, Index2, RListsAux),        		
		append([[IndexElemAnt]], RListsAux, RLists)).

%Corrobora que luego de aplicar la gravedad en un espacio acotad(columnas a 1 de distancia)
%la ultima posicion del Path este a distancia 1 del bloque. 
%Si check es 0 entonces se encontro camino, sino no es correcto el camino.
%FUNCA
simular_gravedad_y_chequear1Elem(Grid, Path, NumOfColumns, IndexElem, Check):-
    last(Path, Index_NewBlock),
    length(Grid, Long),
    NumOfRows is Long div NumOfColumns,
	PosYAux is Index_NewBlock mod NumOfColumns,
	(PosYAux == 0 -> PosYNBlock is NumOfColumns; PosYNBlock is PosYAux),
    PosYAux2 is IndexElem mod NumOfColumns,
	(PosYAux2 == 0 -> PosYElem is NumOfColumns; PosYElem is PosYAux2),
    %Caso 1, resultado fuera de rango
	Distancia is PosYNBlock - PosYElem,
    Distancia >= -1,
    Distancia =< 1,
    %caso 2, checkear en gravedad acotada
    sumar_y_agregar(Grid, Path, Grilla0, Path0, 0, _), 
	acotar_path(Path0, NumOfColumns, PosYElem, PathAcotado), 
	length(PathAcotado, LongAcotado),
	(LongAcotado \== 0 ->    	
        obtener_indices_nuevos(Grilla0, [Index_NewBlock], NumOfColumns, NumOfRows, List_NewIndex_NewBlock),        
    	obtener_indices_nuevos(Grilla0, [IndexElem], NumOfColumns, NumOfRows, List_NewIndex),
        List_NewIndex_NewBlock = [NewIndex_NB],
        List_NewIndex = [NewIndexElem],
        es_adyacente(NewIndex_NB, NewIndexElem, NumOfColumns, Check)
		; 
    	es_adyacente(Index_NewBlock, IndexElem, NumOfColumns, Check)		
		),
    !
    ;   %Fallo caso 1
    Check is 1.
    


%FUNCA
acotar_path([], _, _, []).
acotar_path(Path, NumOfColumns, PosYElem, PathAcotado):-
	Path = [H|T],
	acotar_path(T, NumOfColumns, PosYElem, PathAcotadoAux),
	PosYAux is H mod NumOfColumns,
	(PosYAux == 0 -> PosY is NumOfColumns; PosY is PosYAux ),
	Distancia is PosYElem - PosY,
	(Distancia =< 1, Distancia >= -1 -> append([H], PathAcotadoAux, PathAcotado)
                                       ; PathAcotado = PathAcotadoAux).


%check =0 es OK
%FUNCA
simular_gravedad_y_chekearElems(Grid, Path, NumOfColumns, ListIndex, Check) :-
    last(Path, IndexPos),
    length(Grid, Long),
    NumOfRows is Long div NumOfColumns,
    sumar_y_agregar(Grid, Path, Grilla0, NewPath, 0, _),
    simular_desplazar(Grilla0, NumOfColumns, NewPath, GridDes, 1),
    obtener_indices_nuevos(Grilla0, ListIndex, NumOfColumns, NumOfRows, NewListIndex),
    obtener_indices_nuevos(Grilla0, [IndexPos], NumOfColumns, NumOfRows, NewIndex),
    chequear_adyacencias(GridDes, NumOfColumns, NumOfRows, NewListIndex, NewIndex, Check),
    !
    .


%FUNCA
es_adyacente(Index1, Index2, NumOfColumns, Check):-
    PosFilAux is Index1 div NumOfColumns,
	PosColAux is Index1 mod NumOfColumns,
	(PosColAux == 0 -> PosCol is NumOfColumns; PosCol is PosColAux ),
	(PosColAux == 0 -> PosFil is PosFilAux; PosFil is PosFilAux+1), 
    PosFilAux2 is Index2 div NumOfColumns,
	PosColAux2 is Index2 mod NumOfColumns,
	(PosColAux2 == 0 -> PosCol2 is NumOfColumns; PosCol2 is PosColAux2 ),
	(PosColAux2 == 0 -> PosFil2 is PosFilAux2; PosFil2 is PosFilAux2+1), 
    DistanciaCol is PosCol - PosCol2,
    DistanciaCol =< 1,
    DistanciaCol >= -1,
    DistanciaFil is PosFil - PosFil2,
    DistanciaFil =< 1,
    DistanciaFil >= -1,
    Check is 0,
    !
    ;   
    Check is 1.
%FUNCA
chequear_adyacencias(_, _, _, [], _, 1).
chequear_adyacencias(Grid, NumOfColumns, NumOfRows, ListIndex, IndexElem, Check) :-
    ListIndex = [H|_],
    PosXAux is IndexElem div NumOfColumns,
	PosYAux is IndexElem mod NumOfColumns,
	(PosYAux == 0 -> PosY is NumOfColumns; PosY is PosYAux ),
	(PosYAux == 0 -> PosX is PosXAux; PosX is PosXAux+1), 
    adyacentes(Grid, NumOfColumns, NumOfRows, PosX, PosY, AdjList),
    member(H, AdjList),
    Check is 0,
    !
    ;   
    ListIndex = [_|T],
    chequear_adyacencias(Grid, NumOfColumns, NumOfRows, T, IndexElem, Check)
    .
                 
                 
%FUNCA
obtener_indices_nuevos(_, [], _, _, []).
obtener_indices_nuevos(Grid, [Index|T], NumOfColumns, NumOfRows, NewListIndex) :-
    PosXAux is Index div NumOfColumns,
	PosYAux is Index mod NumOfColumns,
	(PosYAux == 0 -> PosY is NumOfColumns; PosY is PosYAux ),
	(PosYAux == 0 -> PosX is PosXAux; PosX is PosXAux+1), 
    contarCeros(Grid, NumOfColumns, NumOfRows, 0, PosX, PosY, NewFil),
    IndexPos is (NewFil-1)*NumOfColumns + PosY,
    obtener_indices_nuevos(Grid, T, NumOfColumns, NumOfRows, NewListIndexAux),
    append([IndexPos], NewListIndexAux, NewListIndex),
    !.
%FUNCA
contarCeros(_, _, NumOfRows, Index, Fil, _, Fil) :-
    Check is Fil+Index,
    Check > NumOfRows.
contarCeros(Grid, NumOfColumns, NumOfRows, Index, Fil, Col, NewFil) :-
	IndexPos is (Fil+Index-1)*NumOfColumns + Col,
    nth1(IndexPos, Grid, Elem),
    Elem == 0,
    IndexAux is Index + 1,
    contarCeros(Grid, NumOfColumns, NumOfRows, IndexAux, Fil, Col, NewFilAux),
    NewFil is NewFilAux + 1
    ;  
    IndexAux is Index + 1,
    contarCeros(Grid, NumOfColumns, NumOfRows, IndexAux, Fil, Col, NewFil).
    
%FUNCA
simular_desplazar(G, _, _, G, 0).
simular_desplazar(Grid, NumOfColumns, Path, NewGrid, A):-    
	generar_desplazamientos(Grid, Path, NextPath2, NumOfColumns, GridDes),
	Grid \== GridDes,    
	simular_desplazar(GridDes, NumOfColumns, NextPath2, NewGrid, A),
	!
	;
	simular_desplazar(Grid, _, _, NewGrid, 0).


%FUNCA
adyacentes_path_acotado(_, [], _, _, _, A, A, []).
adyacentes_path_acotado(Grid, Path, Visitados, NumOfColumns, NumOfRows, Suma, Corte, RList):-
	Path = [H|T],
	not(member(H, Visitados)),
	append([H],Visitados, Visitados2),
	PosXAux is H div NumOfColumns,
	PosYAux is H mod NumOfColumns,
	(PosYAux == 0 -> PosY is NumOfColumns; PosY is PosYAux ),
	(PosYAux == 0 -> PosX is PosXAux; PosX is PosXAux+1),
	nth1(H, Grid, Elem),
	SumaAux is Elem + Suma,
	resultado_suma(SumaAux, Pot_1),
	SigSuma is 2**Pot_1,
	SigSuma =< Corte,
	adyacentes_mayores_o_iguales(Grid, NumOfColumns, NumOfRows, PosX, PosY, AdjList),
	(adyacentes_path_acotado(Grid, AdjList, Visitados2, NumOfColumns, NumOfRows,  SigSuma, Corte, RList_1)->
		append([H], RList_1, RList)
		;
		(adyacentes_path_acotado(Grid, T, Visitados2, NumOfColumns, NumOfRows, Suma, Corte, RList_2)->
			append([H], RList_2, RList))
	),
    !
	;  
	Suma =< Corte,
    Path = [_|T],
	adyacentes_path_acotado(Grid, T, Visitados, NumOfColumns, NumOfRows, Suma, Corte, RList).


	%findall(RList,(between(1, 9, Index), adyacentes_path_acotado(Grilla, [Index], [], 3, 3, 0, 16, RList)),RLists) 