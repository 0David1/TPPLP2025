:- use_module(piezas).

sublista(Descartar, Tomar , Lista, Resultado) :- 
append(Izquierda, Derecha, Lista), 
length(Izquierda, Descartar),
append(Resultado,_,Derecha),
length(Resultado, Tomar).




tablero(Columnas, [Uno, Dos,Tres, Cuatro, Cinco]) :- 
length(Uno, Columnas),
length(Dos, Columnas),
length(Tres, Columnas),
length(Cuatro, Columnas),
length(Cinco, Columnas).


tamaño(Matriz, Filas , Columnas) :-
length(Matriz , Filas),
append([I],_,Matriz),
length(I , Columnas).



coordenadas(Tablero, (I,J)):-
length(Tablero,Filas),
append([Izq|_],Der, Tablero),
length(Der,Queda), 
I is Filas - Queda,
length(Izq, Columnas),
append([_|_],Col, Izq),
length(Col, Resta),
J is Columnas - Resta.





Kpiezas(0,[]).
kPiezas(K,Res):-
length(Rec,K),
nombrePiezas(Piezas),
subset(Piezas , Rec),
list_to_set(Rec,Res),
length(Res,K).






