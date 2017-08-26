%%%
%%% slc.pl
%%%
%%% Switching logic circuit
%%%   slc interpreter and operators
%%%
%%% (c) 2016, xae. Juan José Eraso Escalona
%%% 20160813
%%% 20161913: Fixed,
%%%             FC in head of = should not care not to be 1.
%%% 20170823
%%%

:- module('xae_slc',
            [ slc/1
              , op(100, fx, [a, an, o, on, =])
            ]
).

%%% Definitional method models
%% a (x, zvke, vke)
a(1, 1, 1).
a(0, 1, 0).
a(0, 0, 0).
a(1, 0, 0).
%% an (x, zvke, vke)
an(0, 1, 1).
an(1, 1, 0).
an(1, 0, 0).
an(0, 0, 0).
%% o (x, zvke, vke)
o(0, 0, 0).
o(1, 0, 1).
o(1, 1, 1).
o(0, 1, 1).
%% on (x, zvke, vke)
on(1, 0, 0).
on(0, 1, 1).
on(1, 1, 1).
on(0, 0, 1).

%%% Operators
:- op(100, fx, [a, an, o, on, =]).

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
    a(RLO, 1, OUT), slc(Ts, (RLO, 0)).
