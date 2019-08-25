/*
    ops.pl
 
    Switching logic circuit
    slc interpreter and operators
 
    (c) 2019, xae. Juan José Eraso Escalona

    20190612
*/

:- use_module(slc).

% Definitional method models

% a (x, zvke, vke)
a(0, _, 0).
a(1, 0, 0).
a(1, 1, 1).
% an (x, zvke, vke)
an(1, _, 0).
an(0, 0, 0).
an(0, 1, 1).

% o (x, zvke, vke)
o(1, _, 1).
o(0, 1, 1).
o(0, 0, 0).
% on (x, zvke, vke)
on(0, _, 1).
on(1, 1, 1).
on(1, 0, 0).

% x (x, zvke, vke)
x(X, X, 0).
x(1, 0, 1).
x(0, 1, 1).
% xn (x, zvke, vke)
xn(X, X, 1).
xn(1, 0, 0).
xn(0, 1, 0).


% Operators
:- op(100, fx, [a, an, o, on, x, xn, =, \=]).
