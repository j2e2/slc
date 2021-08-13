/*
    extra.pl

        Switching logic circuit
        slc interpreter additional operators and terms
     
            s/2, s/1: set
            r/2, r/1: reset

            p: rising edge
            n: falling edge

            set: RLO = 1
            clr: RLO = 0

   (c) 2017, xae. Juan José Eraso Escalona

   20170816
*/

:- use_module(library(slc)).

% Extending
:- op(100, fx, [ p, n, s, r ]).

% Models
% s (in, zq, q)
s(IN, ZQ, Q) :-
    o(IN, ZQ, Q).
% r (in, zq, q)
r(IN, ZQ, Q) :-
    an(IN, ZQ, Q).

% Prototypes 
s(ZQ, Q) :-
    o(1, ZQ, Q).
r(ZQ, Q) :-
    an(1, ZQ, Q).

s(Q) :-
    o(1, _ZQ, Q).
r(Q) :-
    an(1, _ZQ, Q).

% Extending
% s 
slc:slc(s (ZOUT, OUT), (RLO, _FC), (RLO, 0)) :-
    s(RLO, ZOUT, OUT).

% r 
slc:slc(r (ZOUT, OUT), (RLO, _FC), (RLO, 0)) :- 
    r(RLO, ZOUT, OUT).

       
% Edge evaluation

% p (in, zq, q)
p(IN, ZIN, Q) :-
    an(ZIN, IN, Q).

% n (in, zq, q)
n(IN, ZIN, Q) :-
    an(IN, ZIN, Q).

% Prototypes 
p(IN, Q) :-
    an(_ZIN, IN, Q).

n(IN, Q) :-
    an(IN, _ZIN, Q).


% p 
slc:slc(p ZIN, (RLO, _FC), (OUT, 1)) :-
    p(RLO, ZIN, OUT).
slc:slc(p, (RLO, _FC), (OUT, 1)) :-
    p(RLO, _ZIN, OUT).
% n 
slc:slc(n ZIN, (RLO, _FC), (OUT, 1)) :- 
    n(RLO, ZIN, OUT).
slc:slc(n, (RLO, _FC), (OUT, 1)) :- 
    n(RLO, _ZIN, OUT).

% Setting/Clearing RLO

% set
slc:slc(set, (_RLO, _FC), (1, 1)).

% clr
slc:slc(clr, (_RLO, _FC), (0, 1)).

