/************************************************************************************************************/
%                                                 SOKOBAN               
/************************************************************************************************************/

/* Se desea construir programa que permita al androide trazar un plan para llevar las dos cajas (a las       */
/* que llamaremos C1 y C2) a las casillas sombreadas, que como puede verse en la figura, son L8 y L9.        */
/* Téngase en cuenta que en el estado meta se considerará irrelevante cual de las cajas esté ocupando        */
/* cualquiera de las casillas destino siempre y cuando ambas casillas estén cubiertas con una de las cajas.  */
/* Para realizar esta tarea, el androide puede realizar dos operaciones: moverse y empujar una caja. En lo   */
/* que se refiere a la operación de desplazamiento, el androide sólo puede moverse cada vez a UNA casilla    */
/* vacía que sea adyacente a la casilla correspondiente a su posición actual en una de las siguientes        */
/* direcciones: (N)orte, (S)ur, (E)ste y (O)este.                                                            */


/************************************************************************************************************/
/*                        Tablero 3x3 con dos cajas que deben ser empujadas hacia las X.                    */
/************************************************************************************************************/
/*                      +------+------+------+                      +------+------+------+                  */
/*                      |      |      |      |                      |  l1  |  l2  |  l3  |                  */
/*                      +------+------+------+                      +------+------+------+                  */
/*                      |      | caja | caja |          --->        |  l4  |  l5  |  l6  |                  */
/*                      +------+------+------+                      +------+------+------+                  */
/*                      |   S  |   x  |   x  |                      |  l7  |  l8  |  l9  |                  */
/*                      +------+------+------+                      +------+------+------+                  */
/*                                                                                                           */
/************************************************************************************************************/                    

/************************************************************************************************************/
%                                 1. Definición de relaciones posicionales
/************************************************************************************************************/

% 1.1. Relaciones entre celdas (lugares numerados de 1-9).
encima(l7,l4).
encima(l4,l1).
encima(l8,l5).
encima(l5,l2).
encima(l9,l6).
encima(l6,l3).
dcha(l1, l2).
dcha(l2, l3).
dcha(l4, l5).
dcha(l5, l6).
dcha(l7, l8).
dcha(l8, l9).

% 1.2. Posiciones iniciales.
caja(l5).
caja(l6).     
solucion(l8).
solucion(l9). 
sokoban(l7).

% 1.3.Direcciones permitidas por el juego
direc(arriba).
direc(abajo).
direc(izquierda).
direc(derecha).

% 1.4.Posiciones de celdas vecinas 
vecina(Lugar1, Lugar2, arriba) :- encima(Lugar1, Lugar2).
vecina(Lugar1, Lugar2, abajo) :- encima(Lugar2, Lugar1).
vecina(Lugar1, Lugar2, derecha) :- dcha(Lugar1, Lugar2).
vecina(Lugar1, Lugar2, izquierda) :- dcha(Lugar2, Lugar1).


/************************************************************************************************************/
%                                         2. Restricciones del problema
/************************************************************************************************************/
% 2.1. Se definen esquinas, con el objetivo de no llevar las cajas hacia ellas.
%     Una celda no es esquina, si tiene otras por encima y debajo o tiene celdas a ambos lados.
corner(X) :- 
    \+ noncorner(X).
noncorner(X) :- encima(_,X),encima(X,_).
noncorner(X) :- dcha(_,X),dcha(X,_).

% 2.2. Una caja se hallará en posición ilegal si se trata de una esquina.
ilegal(X) :-
     corner(X),\+ solucion(X)
    .
% 2.3. No se puede mover una caja a lugares de donde no se puedan mover,
%      a menos que sean solucion.
ilegal(X, Y) :-
    (dcha(X,Y); dcha(Y,X)),
    (\+ solucion(X); \+ solucion(Y)),
    (\+ encima(X,_); \+ encima(_,X)),
    (\+ encima(Y,_); \+ encima(_,Y)).
ilegal(X, Y) :-
    (encima(X,Y); encima(Y,X)),
    (\+ solucion(X); \+ solucion(Y)),
    (\+ dcha(X,_); \+ dcha(_,X)),
    (\+ dcha(Y,_); \+ dcha(_,Y)).

/************************************************************************************************************/
%                         3. Restricciones aplicadas al movimiento del Sokoban.
/************************************************************************************************************/
% 3.1. Puede quedarse en la misma celda
alcanzable(Lugar1, Lugar1, _Cajas, _Visitado).

% 3.2. Puede moverse a una celda vecina, siempre que no haya una caja
alcanzable(Lugar1, Lugar2, Cajas, _Visitado) :-
    vecina(Lugar1, Lugar2, _),
    \+ member(Lugar2, Cajas).
    %write('Sokoban2 a: '),
    %write(Lugar1),nl.

% 3.3. Puede moverse a una celda vecina de vecina, si se cumple que:
%    1. No ha estado alli antes (se evitan ciclos).
%    2. No hay una caja en el lugar
%    3. La celda 'vecina de vecina' es alcanzable desde alguna vecina.
alcanzable(Lugar1, Lugar2, Cajas, Visitado) :-
    vecina(Lugar1, Lugar3, _),
    Lugar3 \== Lugar2,
    \+ member(Lugar3, Visitado),
    \+ member(Lugar3, Cajas),
    write('Buscando...'),
    write(Lugar3),nl,
    alcanzable(Lugar3, Lugar2, Cajas, [Lugar3|Visitado]).

/************************************************************************************************************/
%                            4. Consideraciones adicionales en el movimiento
/************************************************************************************************************/
% 4.1. No se moveran las cajas a un lugar donde:
%    1. Ya exista otra caja.
%    2. No sea un movimiento definido como ilegal.
evalua_movimiento(X, Cajas) :-
    \+ member(X, Cajas),
    \+ ilegal(X),
    foreach(member(Caja, Cajas), \+ ilegal(X, Caja)).

% 4.2. El movimiento se realizará si:
%        1. El sitio desde donde se empuja la caja, es alcanzable
%        2. Ese sitio no está ocupado por una caja
%        3. El movimiento no es ilegal y tiene en cuenta las consideraciones
%        4. El siguiente lugar que ocupa la caja es vecino de la celda actual
movimiento(state(Sokoban, Cajas), mueve(Caja, Dir)) :-
    select(Caja, Cajas, CajasResto),
    vecina(Caja, SiguienteLugar, Dir),
    evalua_movimiento(SiguienteLugar, CajasResto), 
    vecina(PushPosition, Caja, Dir),
    alcanzable(Sokoban, PushPosition, Cajas, []),
    \+ member(PushPosition, Cajas).

%4.3. Actualiza el estado
actualiza(state(_Sokoban, Cajas), mueve(Caja, Dir), state(NuevoSokoban, NuevaCajas)) :-
    NuevoSokoban = Caja,
    subtract(Cajas, [Caja], TempList),
    vecina(Caja, NuevaPos, Dir),
    append(TempList, [NuevaPos], NuevaCajas).

/************************************************************************************************************/
%                        5.Algoritmo de búsqueda en profundidad (Deep-First Search)
/************************************************************************************************************/
initial_state(state(l7, [l5, l6])).

%Resolucion reciclada de "deep first search"
solve_dfs(State, _History, []) :-
    final_state(State).
                                
solve_dfs(State, History, [Move|Moves]) :-
    movimiento(State, Move),
    actualiza(State, Move, NewState),
    \+ member(NewState, History), 
    solve_dfs(NewState, [NewState|History], Moves).

/************************************************************************************************************/
/*                                    6. Resolución del problema                                            */
/************************************************************************************************************/

% 6.1. El problema queda resuelto cuando todas las cajas se encuentran en
%          posición de solución.
final_state(state(_Sokoban, Cajas)) :-
    todas_sol(Cajas),
    write('Solución alcanzada!'),!.
todas_sol([]).
todas_sol([Caja|Cajas]) :-
    solucion(Caja),
    todas_sol(Cajas).

% 6.2. Resuelve.
solve_problem(Solution) :-
    initial_state(Initial),
    format('Inicio: ~w~n', Initial),
    solve_dfs(Initial, [Initial], Solution).
