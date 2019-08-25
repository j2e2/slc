/*
   ops.pl

   Switching logic circuit
   slc interpreter and operators

   (c) 2019, xae. Juan José Eraso Escalona

   20190612
*/

:- use_module(library(slc)).
:- use_module(library(clpb)).

% a (x, zvke, vke)
a(X, ZVKE, VKE) :-
    sat(VKE =:= (X * ZVKE)).

% an (x, zvke, vke)
an(X, ZVKE, VKE) :-
    sat(VKE =:= (~X * ZVKE)).

% o (x, zvke, vke)
o(X, ZVKE, VKE) :-
    sat(VKE =:= (X + ZVKE)).

% on (x, zvke, vke)
on(X, ZVKE, VKE) :-
    sat(VKE =:= (~X + ZVKE)).

% x (x, zvke, vke)
xae_slc:x(X, ZVKE, VKE) :-
    sat(VKE =:= X # ZVKE).

% xn (x, zvke, vke)
xae_slc:xn(X, ZVKE, VKE) :-
    sat(VKE =\= X # ZVKE).

% Operators
:- op(100, fx, [a, an, o, on, x, xn, =, \=]).
