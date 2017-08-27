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
              , op(100, fx, [jmp, jmpc, jmpn])
              , op(100, xf, [:])
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
:- op(100, fx, [jmp, jmpc, jmpn]).
:- op(100, xf, [:]).

%%% SLC interpreter
%%% Accumulator based,
%%% no explicit support for and/or before or/and.
%%% supports jumps
%%%
%%%    STATE(RLO, FC)
%%%         RLO, Result of Last Operation
%%%         FC, First Consult
%%%         SEGS, Complete program for jump search
%%%
%%%         The jump implementation is straighforward, the state is 
%%%         supplemented with a verbatim copy of the complete program, SEGS,
%%%         so when a jump is exercised a search for the target label is made
%%%         over this program copy.
%%%

:- multifile slc/2.

%% Base cases
slc(Ts) :- 
    ! , slc(Ts, (_RLO, 0, Ts)).
slc([], _ZSTATE).
%% Load
slc([a IN|Ts], (_RLO, 0, SEGS)) :-
    slc(Ts, (IN, 1, SEGS)).
slc([an IN|Ts], (_RLO, 0, SEGS)) :-
    an(IN, 1, Q), slc(Ts, (Q, 1, SEGS)).
%% a
slc([a IN|Ts], (RLO, 1, SEGS)) :-
    a(IN, RLO, Q), slc(Ts, (Q, 1, SEGS)).
slc([an IN|Ts], (RLO, 1, SEGS)) :-
    an(IN, RLO, Q), slc(Ts, (Q, 1, SEGS)).
%% o
slc([o IN|Ts], (RLO, 1, SEGS)) :-
    o(IN, RLO, Q), slc(Ts, (Q, 1, SEGS)).
slc([on IN|Ts], (RLO, 1, SEGS)) :-
    on(IN, RLO, Q), slc(Ts, (Q, 1, SEGS)).
%% =
slc([= OUT|Ts], (RLO, _FC, SEGS)) :-
    a(RLO, 1, OUT), slc(Ts, (RLO, 0, SEGS)).

%%% Jumps
%% labels
slc([_LBL :|Ts], (RLO, 0, SEGS)) :-
   slc(Ts, (RLO, 0, SEGS)).
%% Inconditional jump
slc([jmp LBL|_Ts], (RLO, FC, SEGS)) :-
   lists:append(_BEFORE, [LBL :|AFTER], SEGS),
   slc(AFTER, (RLO, FC, SEGS)).
%% Conditional jumps
slc([jmpc LBL|_Ts], (1, _FC, SEGS)) :-
   lists:append(_BEFORE, [LBL :|AFTER], SEGS),
   slc(AFTER, (1, 0, SEGS)).
slc([jmpc _LBL|Ts], (0, _FC, SEGS)) :-
   slc(Ts, (0, 0, SEGS)).
slc([jmpn LBL|_Ts], (0, _FC, SEGS)) :-
   lists:append(_BEFORE, [LBL :|AFTER], SEGS),
   slc(AFTER, (0, 0, SEGS)).
slc([jmpn _LBL|Ts], (1, _FC, SEGS)) :-
   slc(Ts, (1, 0, SEGS)).


