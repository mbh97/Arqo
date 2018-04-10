pertenece(X, [X|_]).
pertenece(X, [_|Rs]) :- pertenece(X, Rs).

pertenece_m(X, [X|_]) :- X \= [_|_].
pertenece_m(X, [E|_]) :- pertenece_m(X,E).
pertenece_m(X, [_|L]) :- pertenece_m(X,L).

concatena([], L, L).
concatena([X|L1], L2, [X|L3]) :-
    concatena(L1, L2, L3).
    
invierte([],[]).
invierte([X|R], L) :- invierte(R, IR), concatena(IR, [X], L).

%invierte([1, 2], L).
%L = [2, 1]
%invierte([], L).
%L = []
%invierte([1, 2], L).
%L = [2, 1]

menor(P, _-Q) :- P<Q.

insert([],L,L).
insert([X-P], L, R) :- L=[], concatena([X-P], L, R).
insert([X-P], [Z|T], R) :- menor(P,Z), concatena([X-P],[Z], CR), concatena(CR, T, R).
insert([X-P], [Z|L], R) :- not(menor(P,Z)), insert([X-P], L, CR), concatena([Z],CR,R).


elem_count(_, [], 0).
elem_count(X, [X|R], Xn) :- elem_count(X, R, A), Xn is(1+A).
elem_count(X, [Y|R], Xn) :- X\=Y, elem_count(X,R,Xn).

list_count([], _, []).
list_count([X|L1], L2, Xn) :- list_count(L1, L2, L), elem_count(X, L2, C), concatena([X-C],L,Xn).

sort_list([], []).
sort_list([X|R], L2) :- sort_list(R,L), insert([X], L, L2). 



%build_tree(List,Tree)


build_tree([], tree(1, nil, nil)).
build_tree(X-_, tree(X, nil, nil)).
build_tree([X-_|R], tree(1,Q,T)) :- T = build_tree(R, build_tree(X, U)).


%tree(1, X, build_tree(R, Tree))