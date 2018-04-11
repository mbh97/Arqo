
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	Ejercicio 1
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

%%%
%% EJEMPLOS
	% pertenece(1, [2, 1, 3, 1]).
		% true
		% true
		% false
	% pertenece(X, [2, 1, 3, 1]).
		% X = 2
		% X = 1
		% X = 3
		% X = 1
		% false
	% pertenece(1, L).
		% L = [1|_1266]
		% L = [_1084, 1|_1092]
		% L = [_1084, _1090, 1|_1098]
		% L = [_1084, _1090, _1096, 1|_1104]
		% L = [_1084, _1090, _1096, _1102, 1|_1110]
		% L = [_1084, _1090, _1096, _1102, _1108, 1|_1116]
		% L = [_1084, _1090, _1096, _1102, _1108, _1114, 1|_1122]
		% etc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

%%%
%% EJEMPLOS
	% pertenece_m(X, [2,[1,3],[1,[4,5]]]).
		% X = 2
		% X = 1
		% X = 3
		% X = 1
		% X = 4
		% X = 5
		% false
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	Ejercicio 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

%%%
%% EJEMPLOS
	% concatena([], [1, 2, 3], L).
		% L = [1, 2, 3]
		
	% concatena([1, 2, 3], [4, 5], L).
		% L = [1, 2, 3, 4, 5]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% invierte(L, R)
%% Funcion que se satisface si R contiene los elementos
%% de L en orden inverso
%% Input:
%%	L: lista de elementos
%%	R: lista inversa de L
%% Returns: evaluacion de R

invierte([],[]).
invierte([X|R], L) :- invierte(R, IR), concatena(IR, [X], L).

%%%
%% EJEMPLOS
	% invierte([1, 2], L).
		% L = [2, 1]
	% invierte([], L).
		% L = []
	% invierte([1, 2], L).
		% L = [2, 1]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	Ejercicio 3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% menor(P, X-Q)
%% Funcion que si P es menor que Q
%% Input:
%%	P: posicion a comparar
%%	X-Q: elemento donde X es el nombre
%%						Q es la posicion a comparar
%% Returns: true o false

menor(P, _-Q) :- P<Q.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

%%%
%% EJEMPLOS
	% insert([a-6], [], X).
		% X = [a-6]
		% false
	% insert([a-6], [p-0], X).
		% X = [p-0, a-6]
		% false
	% insert([a-6], [p-0, g-7], X).
		% X = [p-0, a-6, g-7]
		% false
	% insert([a-6], [p-0, g-7, t-2], X).
		% X = [p-0, a-6, g-7, t-2]
		% false
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	Ejercicio 4
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 4.1
%%
%% elem_count(X, L, Xn)
%% Funcion que se satisface cuando el elemento X aparece Xn
%% veces en la lista L
%% Input:
%%	X: elemento a contar
%%	L: lista de elementos
%%	Xn: veces que aparece el elemento X
%% Returns: evaluacion de Xn					

elem_count(_, [], 0).
elem_count(X, [X|R], Xn) :- elem_count(X, R, A), Xn is(1+A).
elem_count(X, [Y|R], Xn) :- X\=Y, elem_count(X,R,Xn).

%%%
%% EJEMPLOS
	% elem_count(b, [b,a,b,a,b], Xn).
		% Xn = 3
		% false
	% elem_count(a, [b,a,b,a,b], Xn).
		% Xn = 2
		% false
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 4.2
%%
%% list_count(L1, L2, L3)
%% Funcion que se satisface cuando la lista L3 contiene 
%% las ocurrencias de los elementos de L1 en L2 en
%% forma de par
%% Input:
%%	L1: lista de elementos a contar
%%	L2: lista de elementos
%%	L3: lista contador 
%% Returns: evaluacion de L3					
	
list_count([], _, []).
list_count([X|L1], L2, Xn) :- list_count(L1, L2, L), elem_count(X, L2, C), concatena([X-C],L,Xn).

%%%
%% EJEMPLOS
	% list_count([b], [b,a,b,a,b], Xn).
		% Xn = [b-3]
		% false
	% list_count([b,a], [b,a,b,a,b], Xn).
		% Xn = [b-3, a-2]
		% false
	% list_count([b,a,c], [b,a,b,a,b], Xn).
		% Xn = [b-3, a-2, c-0]
		% false
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	Ejercicio 5
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sort_list(L1, L2)
%% Funcion que se satisface cuando la lista L2 contiene 
%% los pares de elementos de la lista L1 en orden
%% Input:
%%	L1: lista de elementos a ordenar
%%	L2: lista de elementos ordenada
%% Returns: evaluacion de L2

sort_list([], []).
sort_list([X|R], L2) :- sort_list(R,L), insert([X], L, L2). 

%%%
%% EJEMPLOS
	% sort_list([p-0, a-6, g-7, t-2], X).
		% X = [p-0, t-2, a-6, g-7]
		% false
	% sort_list([p-0, a-6, g-7, p-9, t-2], X).
		% X = [p-0, t-2, a-6, g-7, p-9]
		% false
	% sort_list([p-0, a-6, g-7, p-9, t-2, 9-99], X).
		% X = [p-0, t-2, a-6, g-7, p-9, 9-99]
		% false
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	Ejercicio 6
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% build_tree(List, Tree)
%% Funcion que se satisface cuando Tree es la transformacion
%% de List en una version simplificada del arbol de Huffman
%% Input:
%%	List: lista de pares de elementos ordenados
%%	Tree: arbol de Huffman
%% Returns: evaluacion de Tree


build_tree(X-_, tree(X,nil,nil)).
build_tree([X|R], tree(1,I,nil)) :- R=[], build_tree(X,I).
build_tree([X1,X2|R], tree(1,I,D)) :- R=[], build_tree(X1,I), build_tree(X2,D).
build_tree([X|R], tree(1,I,D)) :- R=[_,_|_], build_tree(X,I), build_tree(R,D).

%%%
%% EJEMPLOS
	% build_tree([p-0, a-6, g-7, p-9, t-2, 9-99], X).
		% X = tree(1, tree(p, nil, nil), tree(1, tree(a, nil, nil), tree(1, tree(g, nil, nil),
		% tree(1, tree(p, nil, nil), tree(1, tree(t, nil, nil), tree(9, nil, nil))))))
		% false
	% build_tree([p-55, a-6, g-7, p-9, t-2, 9-99], X).
		% X = tree(1, tree(p, nil, nil), tree(1, tree(a, nil, nil), tree(1, tree(g, nil, nil),
		% tree(1, tree(p, nil, nil), tree(1, tree(t, nil, nil), tree(9, nil, nil))))))
		% False
	% build_tree([p-55, a-6, g-2, p-1], X).
		% X = tree(1, tree(p, nil, nil), tree(1, tree(a, nil, nil), tree(1, tree(g, nil, nil),
		% tree(p, nil, nil))))
		% False
	% build_tree([a-11, b-6, c-2, d-1], X).
		% X = tree(1, tree(a, nil, nil), tree(1, tree(b, nil, nil), tree(1, tree(c, nil, nil),
		% tree(d, nil, nil))))
		% False
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	Ejercicio 7
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 7.1
%%
%% encode_elem(X1, X2, Tree)
%% Funcion se codifica el elemento X1 en X2
%% basandose en la estructura del arbol Tree
%% Input:
%%	X1: elemento a codificar
%%	X2: elmento codificado
%%	Tree: arbol de Huffman
%% Returns: evaluacion de X2

encode_elem(E, [], tree(E, nil, nil)).
encode_elem(E, [0], tree(1, tree(E,nil,nil), _)).
encode_elem(E, X, tree(1, _, D)) :- encode_elem(E, A, D), concatena([1], A, X).

%%%
%% EJEMPLOS
	% encode_elem(a, X, tree(1, tree(a, nil, nil), tree(1, tree(b, nil, nil), 
	% tree(1, tree(c, nil, nil), tree(d, nil, nil))))).
		% X = [0]
		% false
	% encode_elem(b, X, tree(1, tree(a, nil, nil), tree(1, tree(b, nil, nil), 
	% tree(1, tree(c, nil, nil), tree(d, nil, nil))))).
		% X = [1, 0]
		% false
	% encode_elem(c, X, tree(1, tree(a, nil, nil), tree(1, tree(b, nil, nil), 
	% tree(1, tree(c, nil, nil), tree(d, nil, nil))))).
		% X = [1, 1, 0]
		% false
	% encode_elem(d, X, tree(1, tree(a, nil, nil), tree(1, tree(b, nil, nil), 
	% tree(1, tree(c, nil, nil), tree(d, nil, nil))))).
		% X = [1, 1, 1]
		% false
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 7.2
%%
%% encode_list(X1, X2, Tree)
%% Funcion se codifica la lista L1 en L2
%% basandose en la estructura del arbol Tree
%% Input:
%%	L1: lista a codificar
%%	l2: lista codificada
%%	Tree: arbol de Huffman
%% Returns: evaluacion de L2

encode_list([], [], _).
encode_list([E|R], X, T) :- encode_list(R, A, T), encode_elem(E, B, T), concatena(A, [B], X).

%%%
%% EJEMPLOS
	% encode_list([a], X, tree(1, tree(a, nil, nil), tree(1, tree(b, nil, nil), tree(1,
	% tree(c, nil, nil), tree(d, nil, nil))))).
		% X = [[0]]
		% false
	% encode_list([a,a], X, tree(1, tree(a, nil, nil), tree(1, tree(b, nil, nil), tree(1,
	% tree(c, nil, nil), tree(d, nil, nil))))).
		% X = [[0], [0]]
		% false
	% encode_list([a,d,a], X, tree(1, tree(a, nil, nil), tree(1, tree(b, nil, nil),
	% tree(1, tree(c, nil, nil), tree(d, nil, nil))))).
		% X = [[0], [1, 1, 1], [0]]
		% false
	% encode_list([a,d,a,q], X, tree(1, tree(a, nil, nil), tree(1, tree(b, nil, nil),
	% tree(1, tree(c, nil, nil), tree(d, nil, nil))))).
		% false.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	Ejercicio 8
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% encode(L1, L2)
%% Funcion que codifica la lista L1 en L2, haciendo
%% uso del predicado diccionario
%% Input:
%%	L1: lista a codificar
%%	L2: lista codificada
%% Returns: evaluacion de L2


%%%
%% EJEMPLOS
	% dictionary([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]).
	% encode([i,n,t,e,l,i,g,e,n,c,i,a,a,r,t,i,f,i,c,i,a,l],X).
		% X = [[0], [1, 1, 1, 0], [1, 1, 0], [1, 1, 1, 1, 1, 0], [1, 1, 1, 1, 0], [0], [1, 1, 1,
		% 1, 1, 1, 1, 1, 0], [1, 1, 1, 1, 1, 0], [1, 1, 1, 0], [1, 1, 1, 1, 1, 1, 0], [0], [1,
		% 0], [1, 0], [1, 1, 1, 1, 1, 1, 1, 0], [1, 1, 0], [0], [1, 1, 1, 1, 1, 1, 1, 1, 1, 0],
		% [0], [1, 1, 1, 1, 1, 1, 0], [0], [1, 0], [1, 1, 1, 1, 0]]
		% False
	% encode([i,a],X).
		% X = [[0], [1, 0]]
		% False
	% encode([i,2,a],X).
		% false







