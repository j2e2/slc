%%%
%%% extra.pl
%%%
%%% Switching logic circuit
%%%   slc interpreter additional operators and terms
%%%     
%%%     x[], xn: xor(n)
%%%
%%%     s/2, s/1: set
%%%     r/2, r/1: reset
%%%
%%%     p: rising edge
%%%     n: falling edge
%%%
%%% (c) 2017, xae. Juan José Eraso Escalona
%%%
%%% 20170816
%%%

:- use_module(slc).

:- use_module(clpb).

%%%    
%%% Extends slc via operator
%%%    Teach slc to use xor
%%%

%%% Model
%% x (x, zvke, vke)
xae_slc:x(X, ZVKE, VKE) :-
    sat(VKE =:= X # ZVKE).
%% xn (x, zvke, vke)
xae_slc:xn(X, ZVKE, VKE) :-
    sat(VKE =\= X # ZVKE).

%%% Extends interpreter
:- op(100, fx, [ x, xn ]).

%% x/x[]
xae_slc:slc([x IN|Ts], (RLO, 1, SEGS)) :-
    xae_slc:aoboa(IN, Ts, RLO, SEGS, xae_slc:x).

%% xn
xae_slc:slc([xn IN|Ts], (RLO, 1, SEGS)) :-
        xae_slc:xn(IN, RLO, Q), xae_slc:slc(Ts, (Q, 1, SEGS)).


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
xae_slc:slc([s(ZOUT, OUT)|Ts], (RLO, _FC, SEGS)) :-
    s(RLO, ZOUT, OUT), xae_slc:slc(Ts, (RLO, 0, SEGS)).
xae_slc:slc([s(OUT)|Ts], (RLO, _FC, SEGS)) :-
    s(RLO, _ZOUT, OUT), xae_slc:slc(Ts, (RLO, 0, SEGS)).
%% r 
xae_slc:slc([r(ZOUT, OUT)|Ts], (RLO, _FC, SEGS)) :- 
    r(RLO, ZOUT, OUT), xae_slc:slc(Ts, (RLO, 0, SEGS)).
xae_slc:slc([r(OUT)|Ts], (RLO, _FC, SEGS)) :- 
    r(RLO, _ZOUT, OUT), xae_slc:slc(Ts, (RLO, 0, SEGS)).

%%%        
%%% Edge evaluation
%%%

%%% Models
%% p (in, zq, q)
p(IN, ZIN, Q) :-
    xae_slc:an(ZIN, IN, Q).

%% n (in, zq, q)
n(IN, ZIN, Q) :-
    xae_slc:an(IN, ZIN, Q).

%%% Extending

:- op(100, fx, [ p, n ]).

%% p 
xae_slc:slc([p ZIN|Ts], (RLO, _FC, SEGS)) :-
    p(RLO, ZIN, OUT), xae_slc:slc(Ts, (OUT, 1, SEGS)).
xae_slc:slc([p |Ts], (RLO, _FC, SEGS)) :-
    p(RLO, _ZIN, OUT), xae_slc:slc(Ts, (OUT, 1, SEGS)).
%% n 
xae_slc:slc([n ZIN|Ts], (RLO, _FC, SEGS)) :- 
    n(RLO, ZIN, OUT), xae_slc:slc(Ts, (OUT, 1, SEGS)).
xae_slc:slc([n |Ts], (RLO, _FC, SEGS)) :- 
    n(RLO, _ZIN, OUT), xae_slc:slc(Ts, (OUT, 1, SEGS)).

%%%
%%% Setting/Clearing RLO
%%%

%%% Prototype
set.

clr.

%% set
xae_slc:slc([set|Ts], (_RLO, _FC, SEGS)) :-
    xae_slc:slc(Ts, (1, 1, SEGS)).

%% clr
xae_slc:slc([clr|Ts], (_RLO, _FC, SEGS)) :-
    xae_slc:slc(Ts, (0, 1, SEGS)).
