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
%%% 20181812: Fixed, multiple solutions in no parens
%%%				aoboa only in list case
%%%

:- module('xae_slc',
            [ slc/1
              , op(100, fx, [a, an, o, on, =, \=])
              , op(100, fx, [jmp, jmpc, jmpn])
              , op(100, xf, [:])
            ]
).

:- use_module(clpb).

a(X, ZVKE, VKE) :-
    sat(VKE =:= (X * ZVKE)).

an(X, ZVKE, VKE) :-
    sat(VKE =:= (~X * ZVKE)).

o(X, ZVKE, VKE) :-
    sat(VKE =:= (X + ZVKE)).

on(X, ZVKE, VKE) :-
    sat(VKE =:= (~X + ZVKE)).

%%% Operators
:- op(100, fx, [a, an, o, on, =, \=]).
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
%% a/a[]
xae_slc:slc([a IN|Ts], (RLO, 1, SEGS)) :-
    is_list(IN), aoboa(IN, Ts, RLO, SEGS, xae_slc:a).
%% o/o[]
xae_slc:slc([o IN|Ts], (RLO, 1, SEGS)) :-
    is_list(IN), aoboa(IN, Ts, RLO, SEGS, xae_slc:o).
%% an
slc([an IN|Ts], (RLO, 1, SEGS)) :-
    an(IN, RLO, Q), slc(Ts, (Q, 1, SEGS)).
%% on
slc([on IN|Ts], (RLO, 1, SEGS)) :-
    on(IN, RLO, Q), slc(Ts, (Q, 1, SEGS)).
%% =
slc([= OUT|Ts], (RLO, _FC, SEGS)) :-
    a(RLO, 1, OUT), slc(Ts, (RLO, 0, SEGS)).
%% \=
slc([\= OUT|Ts], (RLO, _FC, SEGS)) :-
    an(RLO, 1, OUT), slc(Ts, (RLO, 0, SEGS)).

%%% Metapredicate, parens evaluation
%% IN,
%%    as list is appended with an auxiliar variable assigned to 
%%    its proper value, ARLO.
%% OP,
%%    selects and/or semantics.
aoboa(IN, Ts, RLO, SEGS, OP) :-
    lists:append(IN, [= ARLO], Ys),
    slc(Ys, (0, 0, SEGS)), 
    call(OP, RLO, ARLO, Q),
    slc(Ts, (Q, 1, SEGS)).

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


