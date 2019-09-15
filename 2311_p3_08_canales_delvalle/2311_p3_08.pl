%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	PRACTICA 3: PROLOG
%%
%%	Grupo 2311
%%
%%	Leyre Canales
%%	Gloria del Valle
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	Ejercicio1
%%	duplica(L,L1)
%%
%%	Cierto si la lista L1 contiene 
%%	los elementos duplicados de L
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

duplica([],[]).
duplica([X|L1],[X,X|L2]) :-
	duplica(L1,L2).

%%------------------------------------------
%%	Auxiliar (Ejercicio2)
%%	concatena(L1,L2,L)
%%
%%	Satisface cuando el tercer
%%	argumento es el resultado de
%%	concatenar L1 y L2
%%------------------------------------------

concatena([],L,L).
concatena([X|L1],L2,[X|L3]) :-
	concatena(L1,L2,L3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	Ejercicio2
%%	invierte(L,R)
%%
%%	Satisface cuando R contiene los
%%	elementos de L en orden inverso
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

invierte([],[]).
invierte([X|L1],L2) :-
	invierte(L1,L3),concatena(L3,[X],L2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	Ejercicio3
%%	palindromo(L)
%%
%%	Satisface cuando L es palindroma
%%	se lee de la misma manera de
%%	derecha a izquierda que de
%%	izquierda a derecha
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

palindromo(L) :-
	invierte(L,L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	Ejercicio4
%%	divide(L,N,L1,L2)
%%
%%	Satisface cuando L1 contiene los
%%	primeros N elementos de L
%%	L2 contiene el resto
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

divide(L,0,[],L).
divide([H|T],X,L1,Rest) :-
	Xn is X - 1,
	concatena([H],L2,L1),
	divide(T,Xn,L2,Rest),
	!.

%%------------------------------------------
%%	Auxiliar (Ejercicio5)
%%	es_lista(L)
%%
%%	Satisface cuando el argumento
%%	de entrada es una lista
%%------------------------------------------

es_lista([]).
es_lista([_|L]) :-
	es_lista(L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	Ejercicio5
%%	aplasta(L,L1)
%%
%%	Satisface si uno de los elementos
%%	de L es una lista, sera reemplazada
%%	por sus elementos
%%
%%	Si X no es lista, L1 es la lista
%%		que solo tiene como elemento X
%%	Verificamos recursivamente si Ln es
%%		la lista obtenida reemplazando
%%		cada Ln-1 por sus elementos
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

aplasta(X,[X]) :-
	\+	es_lista(X).
aplasta([],[]).
aplasta([X|Lini],Lsal) :-
	aplasta(X,L1),
	aplasta(Lini,L2),
	concatena(L1,L2,Lsal).

%%------------------------------------------
%%	Auxiliar (Ejercicio6)
%%	divisor_N(X,N)
%%
%%	Satisface cuando X es el menor
%%	divisor de N, mayor o igual que 2
%%------------------------------------------

divisor_N(X,N) :-
	M is floor(sqrt(N)),
	between(2,M,X),
	N mod X =:= 0,
	!.
divisor_N(N,N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	Ejercicio6
%%	primos(N,L)
%%
%%	Satisface cuando L contiene los
%%	factores primos del numero N
%%	
%%	Verificamos si L es la
%%		descomposicion en factores 
%%		primos que queremos
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

primos(1,[]).
primos(N,[X|L]) :-
	N > 1,
	divisor_N(X,N),
	Rest is N/X,
	primos(Rest,L),
	!.

%%------------------------------------------
%%	Auxiliar (Ejercicio7)
%%	longitud(L,N)
%%
%%	Satisface cuando N es la longitud
%%	de la lista L
%%------------------------------------------

longitud([],0).
longitud([_|L],N) :-
	longitud(L,X),
	N is X + 1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	Ejercicio7
%%	run_length(L,L1)
%%
%%	Satisface si la lista L contiene
%%	N terminos consecutivos iguales a
%%	X, codificados como par [N,X]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%------------------------------------------
%%	Ejercicio 7.1
%%	cod_primero(X,L,Lrem,Lfront)
%%
%%	Satisface cuando Lrem contiene
%%	todas las copias de X que se
%%	encuentran al comienzo de L,
%%	incluso X; Lrem es la lista de
%%	elementos restantes
%%------------------------------------------

cod_primero(X,[],[],[X]).
cod_primero(X,[X|Laux],Lrem,[X|Lfront]) :-
	!,
	cod_primero(X,Laux,Lrem,Lfront).
cod_primero(X,[T|Laux],[T|Laux],[X]).

%%------------------------------------------
%%	Ejercicio 7.2
%%	cod_all(L,L1)
%%
%%	Aplica cod_primero(X,L,Lrem,Lfront)
%%	a toda la lista L
%%------------------------------------------

cod_all([],[]).
cod_all([X|L],[L1|Laux]) :-
	cod_primero(X,L,Lrem,L1),
	cod_all(Lrem,Laux).

%%------------------------------------------
%%	Ejercicio 7.3
%%	1) cod_length(L,L1)
%%	2) run_length(L,L1)
%%
%%	1) Transforma cada una de las listas
%%		a la codificacion como par [N,X]
%%	2) Aplica cod_all(L,L1) a toda la lista
%%------------------------------------------

cod_length([],[]).
cod_length([[X1|X2]|L],[[N,X1]|L1]) :-
	longitud([X1|X2],N),
	cod_length(L,L1).

run_length(L,L1) :-
	cod_all(L,Laux),
	cod_length(Laux,L1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	Ejercicio8
%%	build_tree(List,Tree)
%%
%%	Transforma una lista de pares de 
%%	elementos ordenados en una version del
%%	arbol de Huffman
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_tree(X-_,tree(X,nil,nil)).
build_tree([X|Rest],tree(1,Left,nil)) :-
	Rest=[],
	build_tree(X,Left).
build_tree([E1,E2|Rest],tree(1,Left,Right)) :-
	Rest=[],
	build_tree(E1,Left),
	build_tree(E2,Right).
build_tree([X|Rest],tree(1,Left,Right)) :-
	Rest=[_,_|_],
	build_tree(X,Left),
	build_tree(Rest,Right).

%%------------------------------------------
%%	Ejercicio 8.1
%%	encode_elem(X1,X2,Tree)
%%
%%	Codifica el elemento X1 en X2 basandose
%%	en la estructura del arbol de Huffman
%%------------------------------------------

encode_elem(X1,[],tree(X1,nil,nil)).
encode_elem(X1,[0],tree(1,tree(X1,nil,nil),_)).
encode_elem(X1,X2,tree(1,_,Right)) :-
	encode_elem(X1,Eval,Right),
	concatena([1],Eval,X2).

%%------------------------------------------
%%	Ejercicio 8.2
%%	encode_list(L1,L2,Tree)
%%
%%	Codifica la lista L1 en L2 basandose en
%%	la estructura del arbol de Huffman
%%------------------------------------------

encode_list([],[],_).
encode_list([L1|R],L2,Tree) :-
	encode_list(R,Erest,Tree),
	encode_elem(L1,Eval,Tree),
	concatena([Eval],Erest,L2).

%%------------------------------------------
%%	Auxiliar (Ejercicio8.3)
%%	pertenece(X,L)
%%
%%	Satisface cuando X es elemento de L
%%------------------------------------------
pertenece(X,[X|_]).
pertenece(X,[_|Res]) :- 
	pertenece(X,Res).

%%------------------------------------------
%%	Auxiliar (Ejercicio8.3)
%%	pertenece_lista(L1,L2)
%%
%%	Satisface cuando los elementos de L1
%%  son elementos de L2
%%------------------------------------------
pertenece_lista([],[_|_]).
pertenece_lista([X|Res],L) :- 
	pertenece(X,L),
	pertenece_lista(Res,L).

%%------------------------------------------
%%	Auxiliar (Ejercicio8.3)
%%	formar_par(L1,L2)
%%
%%	Satisface cuando los argumentos del
%%  arbol tienen la forma [tipo-num_veces]
%%------------------------------------------
formar_par([],[]) :- !.
formar_par([[X1,X2|_]|Res],[[X2-X1]|Laux]) :-
	formar_par(Res,Laux).

%%------------------------------------------
%%	Auxiliar (Ejercicio8.3)
%%	insertar_lista(L1,L2,L3)
%%
%%	Inserta en una lista el valor despues
%%	del guion del par [tipo-num_veces]
%%	L2 es nuestra cola auxiliar
%%------------------------------------------
insertar_lista([X1-X2],[],[X1-X2]).
insertar_lista([X1-X2],[L1-L2|Res],[X1-X2,L1-L2|Res]) :-
	X2 =< L2.
insertar_lista([X1-X2],[L1-L2|Res],[L1-L2|Res2]) :-
	insertar_lista([X1-X2],Res,Res2),
	X2 > L2.

%%------------------------------------------
%%	Auxiliar (Ejercicio8.3)
%%	ordenar_lista(L1,L2)
%%
%%	Satisface si encuentra la lista ordenada
%%	por el par [tipo-num_veces]
%%------------------------------------------
ordenar_lista([],[]).
ordenar_lista([[X1-X2]|Res],Res2) :-
	ordenar_lista(Res,Cola),
	insertar_lista([X1-X2],Cola,Res2).

%%------------------------------------------
%%	Ejercicio 8.3
%%	encode(L1,L2)
%%
%%	Codifica la lista L1 en L2 usando el
%%	predicado diccionario	
%%------------------------------------------
dictionary([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]).
encode([],nil).
encode(L1,Ldest) :-
	sort(0, @=<, L1, Lini),
	dictionary(D),
	pertenece_lista(Lini,D),
	run_length(Lini,Lrest),
	formar_par(Lrest,Lrest2),
	ordenar_lista(Lrest2,Lrest3),
	invierte(Lrest3,Lrest4),
	build_tree(Lrest4,Tree),
	encode_list(L1,Ldest,Tree).