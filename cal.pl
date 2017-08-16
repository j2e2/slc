%%%
%%% cal.pl
%%%
%%% Switching logic circuit
%%%   extend slc via arbitrary prolog terms
%%%   provides IEC CAL semantics
%%%
%%% (c) 2017, xae. Juan José Eraso Escalona
%%% 20170814
%%%

:- use_module('slc').

%%% Extends slc with cal(ling) a prolog term
%%% Use cases,
%%%    ..., cal functor(args),...
%%%    ..., calc functor(args), ...
%%%    ..., caln functor(args), ...

%%% Extending interpreter
:- op(100, fx, [ cal, calc, caln ]).

%% cal
xae_slc:slc([cal IN|Ts], (RLO, FC)) :-
    do_cal(IN),
    xae_slc:slc(Ts, (RLO, FC)).

%% calc
xae_slc:slc([calc _IN|Ts], (0, _FC)) :-
    xae_slc:slc(Ts, (0, 0)).
xae_slc:slc([calc IN|Ts], (1, _FC)) :-   
    do_cal(IN),
    xae_slc:slc(Ts, (1, 0)).

%% caln
xae_slc:slc([caln _IN|Ts], (1, _FC)) :-
    xae_slc:slc(Ts, (1, 0)).
xae_slc:slc([caln IN|Ts], (0, _FC)) :-
    do_cal(IN),
    xae_slc:slc(Ts, (0, 0)).

% do_cal
do_cal(IN) :-
    IN =.. [Functor|Args],
    apply(Functor, Args).
