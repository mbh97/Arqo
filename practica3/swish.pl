pertenece(X, [X|_]).
pertenece(X, [_|Rs]) :- pertenece(X, Rs).

pertenece_m(X, [X|_]) :- X \= [_|_].
pertenece_m(X, [E|_]) :- pertenece_m(X,E).
pertenece_m(X, [_|L]) :- pertenece_m(X,L).
    

    