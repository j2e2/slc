/*
    ops.pl
 
        Switching logic circuit
        slc interpreter and operators
 
    (c) 2019, xae. Juan José Eraso Escalona

    20190612
*/

:- use_module(library(slc)).

% Definitional method models

% a (x, zvke, vke)
a(1, 0, 0).
a(1, 1, 1).
a(0, _, 0).

% an (x, zvke, vke)
an(0, 0, 0).
an(0, 1, 1).
an(1, _, 0).

% o (x, zvke, vke)
o(0, 1, 1).
o(0, 0, 0).
o(1, _, 1).

% on (x, zvke, vke)
on(1, 1, 1).
on(1, 0, 0).
on(0, _, 1).

% x (x, zvke, vke)
x(1, 1, 0).
x(1, 0, 1).
x(0, 0, 0).
x(0, 1, 1).

% xn (x, zvke, vke)
xn(1, 1, 1).
xn(1, 0, 0).
xn(0, 0, 1).
xn(0, 1, 0).


% On lists

a(Xs, Q) :-
    foldl(a, Xs, 1, Q).
an(Xs, Q) :-
    foldl(an, Xs, 1, Q).
o(Xs, Q) :-
    foldl(o, Xs, 0, Q).
on(Xs, Q) :-
    foldl(on, Xs, 0, Q).
x(Xs, Q) :-
    foldl(x, Xs, 0, Q).
xn(Xs, Q) :-
    foldl(x, Xs, 0, ZQ),
    an(ZQ, 1, Q).
 

% Operators
:- op(100, fx, [a, an, o, on, x, xn, =, \=]).
