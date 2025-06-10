:- use_module(piezas).
%! no poner espacios entre def y argumentos.
sublista(Descartar , Tomar , Lista , Resultado) :- 
length(Inicio, Descartar),
append(Inicio, Fin , Lista), 
length(Resultado,Tomar), 
append(Resultado,_,Fin).

%cambiar a columnas
tablero(Filas,[Uno,Dos,Tres,Cuatro,Cinco]):-
length(Uno, Filas),
length(Cinco, Filas),
length(Cuatro, Filas),
length(Tres, Filas),
length(Dos, Filas).
%! puedo usar maplist.




cantSoluciones(Poda,Columnas,N):-
findall(T, llenarTablero(Poda, Columnas, T), TS),
length(TS,N).








tamanio(Tablero , Filas, Columnas):-
length(Tablero , Filas), append([Uno], _, Tablero), length(Uno, Columnas).





coordenadas(Tablero, (I , J)):-
append([Algo|_],Resto,Tablero),
length(Resto, Menos), I is 5 - Menos,
append([_|_],Resto2,Algo), 
length(Algo, In),
length(Resto2,Menos2), J is In - Menos2.


kPiezas(0, []).
kPiezas(Largo, Lista):-
nombrePiezas(Piezas), sublistaLargo(Piezas,Largo, Lista).




%sublista([],[]).
%sublista([K|R],[K|Rec]):- sublista(R,Rec).
%sublista([_|R], Rec):- sublista(R, Rec).

sublistaLargo(_,0,[]).
sublistaLargo([K|R],L,[K|Rec]):- length([K|R],J), J>=L ,N is L - 1,sublistaLargo(R,N,Rec).
sublistaLargo([_|R],L,Rec):- L > 0, sublistaLargo(R,L,Rec).



%largo de t es alto y largo de cada uno es ancho.
seccionTablero(T,Alto , Ancho, (I,J), Resultado):-
DFilas is I - 1, DCol is J - 1,
sublista(DFilas,Alto,T,Rec),forTablero(Rec,Ancho,DCol,Resultado).



forTablero([],_,_,[]).
forTablero([K|Resto],Ancho,J,[Rec1|Rec2]):- 
sublista(J,Ancho,K,Rec1),forTablero(Resto,Ancho,J,Rec2).



ubicarPieza(Tablero, Pieza):-
pieza(Pieza,E), tamanio(E, F,C),
coordenadas(Tablero,(I,J)),
seccionTablero(Tablero,F,C,(I,J),E).




poda(sinPoda, _).



ubicarPiezas(Tablero,Poda,[]):-poda(Poda,Tablero).
ubicarPiezas(Tablero,Poda,[Pieza|Resto]):-
poda(Poda,Tablero),
ubicarPieza(Tablero,Pieza),
ubicarPiezas(Tablero,Poda,Resto).




llenarTablero(Poda, Columnas, T):-
tablero(Columnas,T),
kPiezas(Columnas,Piezas),
ubicarPiezas(T,Poda,Piezas).





%   ?- time(cantSoluciones(sinPoda,3,N)).
%   25,070,185 inferences, 1.227 CPU in 1.270 seconds (97% CPU, 20439761 Lips)
%   N = 28.


%   ?- time(cantSoluciones(sinPoda,4,N)).
%   938,775,383 inferences, 45.631 CPU in 47.293 seconds (96% CPU, 20573024 Lips)
%   N = 200.

