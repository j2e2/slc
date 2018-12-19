%%%
%%% slc.pl
%%%
%%% Switching logic circuit
%%%   slc interpreter and operators
%%%   runs on clpb library, using a local copy.
%%%   A local copy is used as the supplied with Debian 9 is outdated.
%%%
%%% (c) 2017, xae. Juan José Eraso Escalona
%%% 20170825
%%% 20170826: Fixed,
%%%             aoboa file deleted,
%%%                 parens integrated on slc file, as clpb inhibits
%%%                 the fail mechanism used to select the term doing paren eval
%%% 20181812: Fixed, multiple solutions in no parens
%%%				aoboa only in list case
%%%

:- module('xae_slc',
            [ slc/1
              , op(100, fx, [a, an, o, on, =, \=])
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
    is_list(IN), aoboa(IN, Ts, RLO, xae_slc:a).

%% o/o[]
xae_slc:slc([o IN|Ts], (RLO, 1)) :-
    is_list(IN), aoboa(IN, Ts, RLO, xae_slc:o).
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
    lists:append(IN, [= ARLO], Ys),
     slc(Ys, (0, 0)), 
     call(OP, RLO, ARLO, Q),
     slc(Ts, (Q, 1)).
