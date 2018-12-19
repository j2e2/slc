%%%%
%%%% slc.pl
%%%%
%%%% Switching logic circuit
%%%%   slc interpreter and operators
%%%%
%%%% (c) 2016, xae. Juan José Eraso Escalona
%%%% 20160813
%%%% 20161913: Fixed,
%%%%             FC in head of = should not care not to be 1.
%%%% 20170823
%%%%

:- module('xae_slc',
            [ slc/1
              , op(100, fx, [a, an, o, on, =, \=])
            ]
).

%%% Definitional method models
%% a (x, zvke, vke)
a(0, _, 0).
a(1, X, X).

%% an (x, zvke, vke)
an(1, _, 0).
an(0, X, X).

%% o (x, zvke, vke)
o(1, _, 1).
o(0, X, X).

%% on (x, zvke, vke)
on(0, _, 1).
on(1, X, X).

%%% Operators
:- op(100, fx, [a, an, o, on, =, \=]).

%%% SLC interpreter
%%% Accumulator based,
%%% no explicit support for and/or before or/and.
%%%
%%%    STATE(RLO, FC)
%%%         RLO, Result of Last Operation
%%%         FC, First Consult

:- multifile slc/2.

%% Base cases
slc(Ts) :- 
    ! , slc(Ts, (_RLO, 0)).
slc([], _ZSTATE).
%% Load
slc([a IN|Ts], (_RLO, 0)):-
    slc(Ts, (IN, 1)).
slc([an IN|Ts], (_RLO, 0)):-
    an(IN, 1, Q), slc(Ts, (Q, 1)).
%% a
slc([a IN|Ts], (RLO, 1)) :-
    a(IN, RLO, Q), slc(Ts, (Q, 1)).
slc([an IN|Ts], (RLO, 1)) :-
    an(IN, RLO, Q), slc(Ts, (Q, 1)).
%% o
slc([o IN|Ts], (RLO, 1)) :-
    o(IN, RLO, Q), slc(Ts, (Q, 1)).
slc([on IN|Ts], (RLO, 1)) :-
    on(IN, RLO, Q), slc(Ts, (Q, 1)).
%% =
slc([= OUT|Ts], (RLO, _FC)) :-
    a(1, RLO, OUT), slc(Ts, (RLO, 0)).
%% \=
slc([\= OUT|Ts], (RLO, _FC)) :-
    an(RLO, 1, OUT), slc(Ts, (RLO, 0)).
