%%%
%%% extra.pl
%%%
%%% Switching logic circuit
%%%   slc interpreter additional operators and terms
%%%     
%%%     x, xn: xor(n)
%%%
%%%     s/2, s/1: set
%%%     r/2, r/1: reset
%%%
%%%     p/2, p/1: rising edge
%%%     n/2, n/1: falling edge
%%%
%%% (c) 2017, xae. Juan José Eraso Escalona
%%%
%%% 20170816
%%%

:- use_module('slc').

%%%    
%%% Extends slc via operator
%%%    Teach slc to use xor
%%%

%%% Model
%% x (x, zvke, vke)
x(1, 0, 1).
x(0, 1, 1).
x(0, 0, 0).
x(1, 1, 0).
%% xn (x, zvke, vke)
xn(1, 0, 0).
xn(0, 1, 0).
xn(0, 0, 1).
xn(1, 1, 1).

%%% Extends interpreter
:- op(100, fx, [ x, xn ]).
%% x
xae_slc:slc([x IN|Ts], (RLO, 1)) :-
        x(IN, RLO, Q), xae_slc:slc(Ts, (Q, 1)).
%% xn
xae_slc:slc([xn IN|Ts], (RLO, 1)) :-
        xn(IN, RLO, Q), xae_slc:slc(Ts, (Q, 1)).

%%%        
%%% Extending slc via a Prolog term
%%%

%%% Models
%% s (in, zq, q)
s(IN, ZQ, Q) :-
    xae_slc:o(IN, ZQ, Q).
%% r (in, zq, q)
r(IN, ZQ, Q) :-
    xae_slc:an(IN, ZQ, Q).

%%% Prototypes for slc  
s(_ZOUT, _OUT).
r(_ZOUT, _OUT).

s(_OUT).
r(_OUT).

%%% Extending
%% s 
xae_slc:slc([s(ZOUT, OUT)|Ts], (RLO, _FC)) :-
    s(RLO, ZOUT, OUT), xae_slc:slc(Ts, (RLO, 0)).
xae_slc:slc([s(OUT)|Ts], (RLO, _FC)) :-
    s(RLO, _ZOUT, OUT), xae_slc:slc(Ts, (RLO, 0)).
%% r 
xae_slc:slc([r(ZOUT, OUT)|Ts], (RLO, _FC)) :- 
    r(RLO, ZOUT, OUT), xae_slc:slc(Ts, (RLO, 0)).
xae_slc:slc([r(OUT)|Ts], (RLO, _FC)) :- 
    r(RLO, _ZOUT, OUT), xae_slc:slc(Ts, (RLO, 0)).

%%%        
%%% Edge evaluation
%%%

%%% Models
%% p (in, zq, q)
p(IN, ZQ, Q) :-
    xae_slc:an(ZQ, 1, VKE),
    xae_slc:a(VKE, IN, Q).
%% n (in, zq, q)
n(IN, ZQ, Q) :-
    xae_slc:an(IN, 1, VKE),
    xae_slc:a(VKE, ZQ, Q).

%%% Prototypes for slc  
p(_ZOUT, _OUT).
n(_ZOUT, _OUT).

p(_OUT).
n(_OUT).

%%% Extending
%% p 
xae_slc:slc([p(ZOUT, OUT)|Ts], (RLO, _FC)) :-
    p(RLO, ZOUT, OUT), xae_slc:slc(Ts, (RLO, 0)).
xae_slc:slc([p(OUT)|Ts], (RLO, _FC)) :-
    p(RLO, _ZOUT, OUT), xae_slc:slc(Ts, (RLO, 0)).
%% n 
xae_slc:slc([n(ZOUT, OUT)|Ts], (RLO, _FC)) :- 
    n(RLO, ZOUT, OUT), xae_slc:slc(Ts, (RLO, 0)).
xae_slc:slc([n(OUT)|Ts], (RLO, _FC)) :- 
    n(RLO, _ZOUT, OUT), xae_slc:slc(Ts, (RLO, 0)).
