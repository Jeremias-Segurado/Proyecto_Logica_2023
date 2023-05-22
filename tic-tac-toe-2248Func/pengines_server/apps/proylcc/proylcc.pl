:- module(proylcc, 
	[  
		join/4
	]).


use_module(library(random)).
/**
 * join(+Grid, +NumOfColumns, +Path, -RGrids) 
 * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
 */ 

join(Grid, _NumOfColumns, _Path, RGrids):-
	Grid = [N | Ns],	% La implementación actual es simplemente a modo de muestra, y no tiene sentido, debe reepmplazarla
	N2 is N * 2,		% por una implementación válida.
	RGrids = [[0 | Ns], [N2 | Ns]].


%-----------------------------------------------------
%------------> consult("pengines_server/apps/proylcc/proylcc.pl").

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

%reemplazar_elem(G1, Fila, Col, NuevoValor, G2):-
%	copiarGrilla(G1, G3),
%	nth1(Fila, G3, FilaGrilla),
%	replace_nth(Col, FilaGrilla,  NuevoValor, FilaResul),
%	replace_nth(Fila, G3, FilaResul, G3),
%	!.
%copiarGrilla(G1, G2):-
%	maplist(_CopyTemp,G1 , G2).

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
generar_desplazar(G1, [H|T], NumColum, G2):-
	Sig is H-NumColum,
	nth1(Sig, G1, ElemSig),
	nth1(H, G1, ElemAnt),
	replace_nth(Sig, G1, ElemAnt, G3),
	replace_nth(H, G3, EllemSig, G4),
	generar_desplazar(G4, T, NumColum, G2).

%ToDo:: Sin uso
copy_list([],[]).
copy_list([H|T], [H|Z]):-
	copy_list(T, Z).	
%generar0(+Grilla1, +PosicionesAEliminar, -Grilla2).
%	TRUE si la segunda grilla es igual a la primera con las posiciones puestas en 0.
generar0(G, [], G).
generar0(G1, [H|T], G2):-
	replace_nth(H, G1, 0, G3),
	generar0(G3, T, G2),
	!.

