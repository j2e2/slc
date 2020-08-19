/*
    slc.pl

        Switching logic circuit
        slc interpreter and operators

   (c) 2016, xae. Juan José Eraso Escalona
   
   20160813
   20161913: Fixed,
             FC in head of = should not care not to be 1.
   20170823
*/

:- module('slc',
            [ slc/1
            , op(100, fx, [a, an, o, on, x, xn, =, \=])
            ]
).

/*
    SLC interpreter
    Accumulator based,
    no explicit support for and/or before or/and.

     STATE(RLO, FC)
          RLO, Result of Last Operation
          FC, First Consult
*/


% Interpreter

% slc/1
% slc(+Ts)
%    Ts, a program
%    
%    Toplevel term
slc(Ts) :-
    slc(Ts, (_RLO, 0)).    
% slc/2
% slc(+Ts, ?STATE)
%     A valid programs starts at state (1, 0)
%     and ends with (_, 0)
slc(Ts, STATE) :- 
    foldl(slc, Ts, (1, 0), STATE).


% Operators

% slc/3
% slc(:OP, +ZSTATE, -STATE))
%    Operators terms

:- multifile 
       slc/3.

% Load
slc(a IN, (_RLO, 0), (IN, 1)).
slc(an IN, (_RLO, 0), (Q, 1)) :-
    an(IN, 1, Q).

% a/a[]
slc(a IN, (RLO, 1), (Q, 1)) :-
    aoboa(IN, RLO, Q, a).
slc(an IN, (RLO, 1), (Q, 1)) :-
    an(IN, RLO, Q).

% o/o[]
slc(o IN, (RLO, 1), (Q, 1)) :-
    aoboa(IN, RLO, Q, o).
slc(on IN, (RLO, 1), (Q, 1)) :-
    on(IN, RLO, Q).

% x/x[]
slc(x IN, (RLO, 1), (Q, 1)) :-
    aoboa(IN, RLO, Q, x).

% xn
slc(xn IN, (RLO, 1), (Q, 1)) :-
    xn(IN, RLO, Q).

% =
slc(= OUT, (RLO, _FC), (RLO, 0)) :-
    a(RLO, 1, OUT).

% \=
slc(\= OUT, (RLO, _FC), (RLO, 0)) :-
    an(RLO, 1, OUT).

/*
    aoboa, extends slc with parens, squared, evaluation
    Use cases,
       ..., a [.,.], ...
       ..., o [.,.], ...
       ..., x [.,.], ...
    
    Metapredicate
    IN,
       as list is evaluated as a slc program
    OP,
       selects operator semantics.
*/

aoboa(IN, RLO, Q, OP) :-
    is_list(IN) 
    ->  slc(IN, (ARLO, 1)), 
        call(OP, ARLO, RLO, Q)
    ;   call(OP, IN, RLO, Q).
