%%%
%%% slc.pl
%%%
%%% Switching logic circuit
%%%   slc interpreter and operators
%%%   runs on clpfd library
%%%
%%% (c) 2018, xae. Juan José Eraso Escalona
%%% 20180204
%%%

:- module('xae_slc',
            [ slc/1
              , op(100, fx, [a, an, o, on, =, \=])
            ]
).

:- use_module(library(clpfd)).

a(X, ZVKE, VKE) :-
    [X, ZVKE, VKE] ins 0 .. 1, VKE #= X * ZVKE.

an(X, ZVKE, VKE) :-
    [X, ZVKE, VKE] ins 0 .. 1, VKE #= ZVKE - X * ZVKE.

o(X, ZVKE, VKE) :-
    [X, ZVKE, VKE] ins 0 .. 1, VKE #= X + ZVKE - X * ZVKE.

on(X, ZVKE, VKE) :-
    [X, ZVKE, VKE] ins 0 .. 1, VKE #= 1 - X  + X * ZVKE.


%%% Operators
:- op(100, fx, [a, an, o, on, =, \=]).

%%% SLC interpreter
%%% Accumulator based,
%%% supports parens evaluation.
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
%% a/a[]
xae_slc:slc([a IN|Ts], (RLO, 1)) :-
    aoboa(IN, Ts, RLO, xae_slc:a).

%% o/o[]
xae_slc:slc([o IN|Ts], (RLO, 1)) :-
    aoboa(IN, Ts, RLO, xae_slc:o).
%% an
slc([an IN|Ts], (RLO, 1)) :-
    an(IN, RLO, Q), slc(Ts, (Q, 1)).
%% on
slc([on IN|Ts], (RLO, 1)) :-
    on(IN, RLO, Q), slc(Ts, (Q, 1)).
%% =
slc([= OUT|Ts], (RLO, _FC)) :-
    a(RLO, 1, OUT), slc(Ts, (RLO, 0)).
%% \=
slc([\= OUT|Ts], (RLO, _FC)) :-
    an(RLO, 1, OUT), slc(Ts, (RLO, 0)).

%%% Metapredicate, parens evaluation
%% IN,
%%    as list is appended with an auxiliar variable assigned to 
%%    its proper value, ARLO.
%% OP,
%%    selects and/or semantics.
aoboa(IN, Ts, RLO, OP) :-
    is_list(IN) 
    ->  lists:append(IN, [= ARLO], Ys),
        slc(Ys, (0, 0)), 
        call(OP, RLO, ARLO, Q),
        slc(Ts, (Q, 1))
    ;   call(OP, IN, RLO, Q),
        slc(Ts, (Q, 1)).
