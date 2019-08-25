/*
    ops.pl

    Switching logic circuit
    slc interpreter and operators

   (c) 2019, xae. Juan José Eraso Escalona

   20190612
*/

:- use_module(library(slc)).
:- use_module(library(clpfd)).

% a (x, zvke, vke)
a(X, ZVKE, VKE) :-
    [X, ZVKE, VKE] ins 0 .. 1, VKE #= X * ZVKE.

% an (x, zvke, vke)
an(X, ZVKE, VKE) :-
    [X, ZVKE, VKE] ins 0 .. 1, VKE #= ZVKE - X * ZVKE.

% o (x, zvke, vke)
o(X, ZVKE, VKE) :-
    [X, ZVKE, VKE] ins 0 .. 1, VKE #= X + ZVKE - X * ZVKE.

% on (x, zvke, vke)
on(X, ZVKE, VKE) :-
    [X, ZVKE, VKE] ins 0 .. 1, VKE #= 1 - X  + X * ZVKE.

% x (x, zvke, vke)
x(X, ZVKE, VKE) :-
    [X, ZVKE, VKE] ins 0 .. 1, VKE #= X + ZVKE - 2 * X * ZVKE.
% xn (x, zvke, vke)
xn(X, ZVKE, VKE) :-
    [X, ZVKE, VKE] ins 0 .. 1, VKE #= 1 - X - ZVKE + 2 * X * ZVKE.


% Operators
:- op(100, fx, [a, an, o, on, x, xn, =, \=]).
