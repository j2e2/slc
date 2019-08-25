/*
    cal.pl

    Switching logic circuit
    extend slc via arbitrary prolog terms
    provides IEC CAL semantics

    (c) 2017, xae. Juan José Eraso Escalona
    20170814
*/


% Extends slc with cal(ling) a prolog term
% Use cases,
%    ..., cal functor(args),...
%    ..., calc functor(args), ...
%    ..., caln functor(args), ...

% Extending interpreter
:- op(100, fx, [ cal, calc, caln ]).

% cal
slc:slc(cal IN, STATE, STATE) :-
    slc:do_cal(IN).

% calc
slc:slc(calc _IN, (0, _FC), (0, 0)).
slc:slc(calc IN, (1, _FC), (1, 0)) :-   
    slc:do_cal(IN).

% caln
slc:slc(caln _IN, (1, _FC), (1, 0)).
slc:slc(caln IN, (0, _FC), (0, 0)) :-
    slc:do_cal(IN).

% do_cal
slc:do_cal(IN) :-
    IN =.. [Functor | Args],
    apply(Functor, Args).
