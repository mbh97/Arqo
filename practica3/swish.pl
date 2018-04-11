%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% pertenece(X, L)
%% Comprueba si un elemento X esta en una lista L
%% Input:
%%	X: elemento a comprobar la pertenencia
%%	L: lista de elementos
%% Returns: true o false,
%% 			X, L para los que se satisface

pertenece(X, [X|_]).
pertenece(X, [_|Rs]) :- pertenece(X, Rs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Ejemplos
	%% pertenece(1, [2, 1, 3, 1]).
		% true
		% true
		% false
	%% pertenece(X, [2, 1, 3, 1]).
		% X = 2
		% X = 1
		% X = 3
		% X = 1
		% false
	%% pertenece(1, L).
		% L = [1|_1266]
		% L = [_1084, 1|_1092]
		% L = [_1084, _1090, 1|_1098]
		% L = [_1084, _1090, _1096, 1|_1104]
		% L = [_1084, _1090, _1096, _1102, 1|_1110]
		% L = [_1084, _1090, _1096, _1102, _1108, 1|_1116]
		% L = [_1084, _1090, _1096, _1102, _1108, _1114, 1|_1122]
		% etc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% pertenece_m(X, L)
%% Comprueba si un elemento X esta en una lista L, 
%% o en alguna de sus sublistas
%% Input:
%%	X: elemento a comprobar la pertenencia
%%	L: lista de elementos
%% Returns: true o false,
%% 			X, L para los que se satisface

pertenece_m(X, [X|_]) :- X \= [_|_]. 
pertenece_m(X, [E|_]) :- pertenece_m(X,E).
pertenece_m(X, [_|L]) :- pertenece_m(X,L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Ejemplos
	%% pertenece_m(X, [2,[1,3],[1,[4,5]]]).
		% X = 2
		% X = 1
		% X = 3
		% X = 1
		% X = 4
		% X = 5
		% false
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% concatena(L1, L2, L3)
%% Funcion que se satisface si L3 es el resultado de
%% concatenar L1 y L2
%% Input:
%%	L1: primera lista a concatenar
%%	L2: segunda lista a concatenar
%%	L3: concatenacion de L1 y L2
%% Returns: evaluacion de L3

concatena([], L, L).
concatena([X|L1], L2, [X|L3]) :-
    concatena(L1, L2, L3).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Ejemplos
	%% concatena([], [1, 2, 3], L).
		% L = [1, 2, 3]
		
	%% concatena([1, 2, 3], [4, 5], L).
		% L = [1, 2, 3, 4, 5]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% invierte(L, R)
%% Funcion que se satisface si R contiene los elementos
%% de L en orden inverso
%% Input:
%%	L: lista de elementos
%%	R: lista inversa de L
%% Returns: evaluacion de R

invierte([],[]).
invierte([X|R], L) :- invierte(R, IR), concatena(IR, [X], L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Ejemplos
	%% invierte([1, 2], L).
		% L = [2, 1]
	%% invierte([], L).
		% L = []
	%% invierte([1, 2], L).
		%L = [2, 1]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% menor(P, X-Q)
%% Funcion que si P es menor que Q
%% Input:
%%	P: posicion a comparar
%%	X-Q: elemento donde X es el nombre
%%						Q es la posicion a comparar
%% Returns: true o false

menor(P, _-Q) :- P<Q.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% insert(X-P, L, R)
%% Funcion que si R es la lista formada tras la insercion
%% del elemento X-P en la lista ordenada L
%% Input:
%%	X-P: elemento a insertar
%%	L: lista ordenada de elementos
%%	R: resultado de la inserción
%% Returns: evaluacion de R

insert([],L,L).
insert([X-P], L, R) :- L=[], concatena([X-P], L, R).
insert([X-P], [Z|T], R) :- menor(P,Z), concatena([X-P],[Z], CR), concatena(CR, T, R).
insert([X-P], [Z|L], R) :- not(menor(P,Z)), insert([X-P], L, CR), concatena([Z],CR,R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Ejemplos
	%% insert([a-6], [], X).
		% X = [a-6]
		% false
	%% insert([a-6], [p-0], X).
		% X = [p-0, a-6]
		% false
	%% insert([a-6], [p-0, g-7], X).
		% X = [p-0, a-6, g-7]
		% false
	%% insert([a-6], [p-0, g-7, t-2], X).
		% X = [p-0, a-6, g-7, t-2]
		% false
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% elem_count(X, L, Xn)
%% Funcion se satisface cuando el elemento X aparece Xn
%% veces en la lista L
%% Input:
%%	X: elemento a contar
%%	L: lista de elementos
%%	Xn: veces que aparece el elemento X
%% Returns: evaluacion de Xn					

elem_count(_, [], 0).
elem_count(X, [X|R], Xn) :- elem_count(X, R, A), Xn is(1+A).
elem_count(X, [Y|R], Xn) :- X\=Y, elem_count(X,R,Xn).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Ejemplos
	%% elem_count(b, [b,a,b,a,b], Xn).
		% Xn = 3
		% false
	%% elem_count(a, [b,a,b,a,b], Xn).
		% Xn = 2
		% false
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% list_count(L1, L2, L3)
%% Funcion se satisface cuando la lista L3 contiene 
%% las ocurrencias de los elementos de L1 en L2 en
%% forma de par
%% Input:
%%	L1: lista de elementos a contar
%%	L2: lista de elementos
%%	L3: lista contador 
%% Returns: evaluacion de L3					
	
list_count([], _, []).
list_count([X|L1], L2, Xn) :- list_count(L1, L2, L), elem_count(X, L2, C), concatena([X-C],L,Xn).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Ejemplos
	%% list_count([b], [b,a,b,a,b], Xn).
		% Xn = [b-3]
		% false
	%% list_count([b,a], [b,a,b,a,b], Xn).
		% Xn = [b-3, a-2]
		% false
	%% list_count([b,a,c], [b,a,b,a,b], Xn).
		% Xn = [b-3, a-2, c-0]
		% false
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sort_list(L1, L2)
%% Funcion se satisface cuando la lista L2 contiene 
%% los pares de elementos de la lista L1 en orden
%% Input:
%%	L1: lista de elementos a ordenar
%%	L2: lista de elementos ordenada
%% Returns: evaluacion de L2

sort_list([], []).
sort_list([X|R], L2) :- sort_list(R,L), insert([X], L, L2). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Ejemplos
	%% sort_list([p-0, a-6, g-7, t-2], X).
		% X = [p-0, t-2, a-6, g-7]
		% false
	%% sort_list([p-0, a-6, g-7, p-9, t-2], X).
		% X = [p-0, t-2, a-6, g-7, p-9]
		% false
	%% sort_list([p-0, a-6, g-7, p-9, t-2, 9-99], X).
		% X = [p-0, t-2, a-6, g-7, p-9, 9-99]
		% false
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% build_tree(List, Tree)
%% Funcion se satisface cuando Tree es la transformacion
%% de List en una version simplificada del arbol de Huffman
%% Input:
%%	List: lista de pares de elementos ordenados
%%	Tree: arbol de Huffman
%% Returns: evaluacion de Tree

build_tree([], tree(1, nil, nil)).
build_tree(X-_, tree(X, nil, nil)).
build_tree([X|R], tree(1,Q,T)) :- build_tree(X, Q), build_tree(R, T), sort_list([X|R],_).

%% Ejemplos
	%% build_tree([p-0, a-6, g-7, p-9, t-2, 9-99], X).
		% X = tree(1, tree(p, nil, nil), tree(1, tree(a, nil, nil), tree(1, tree(g, nil, nil),
		% tree(1, tree(p, nil, nil), tree(1, tree(t, nil, nil), tree(9, nil, nil))))))
		% false
	%% build_tree([p-55, a-6, g-7, p-9, t-2, 9-99], X).
		% X = tree(1, tree(p, nil, nil), tree(1, tree(a, nil, nil), tree(1, tree(g, nil, nil),
		% tree(1, tree(p, nil, nil), tree(1, tree(t, nil, nil), tree(9, nil, nil))))))
		% False
	%% build_tree([p-55, a-6, g-2, p-1], X).
		% X = tree(1, tree(p, nil, nil), tree(1, tree(a, nil, nil), tree(1, tree(g, nil, nil),
		% tree(p, nil, nil))))
		% False
	%% build_tree([a-11, b-6, c-2, d-1], X).
		% X = tree(1, tree(a, nil, nil), tree(1, tree(b, nil, nil), tree(1, tree(c, nil, nil),
		% tree(d, nil, nil))))
		% False


