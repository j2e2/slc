%%%
%%% slc-ng.pl
%%%
%%% Switching logic circuit
%%%   slc interpreter and operators
%%%
%%% (c) 2016, xae. Juan José Eraso Escalona
%%%
%%% 20160813: Starts as a verbatim copy of file 'slc.pl'.
%%% 20161913: Fixed,
%%%             FC in head of = should not care not to be 1.
%%% 20170702: Added,
%%%             x operator, s/r terms.
%%%             Parens evaluation via aoboa/4 term.
%%%           Fixed,
%%%             slc/2 is not more a multifile predicate.   
%%% 20170709: Fixed,
%%%             aoboa, call to slc with IN as in list case should have
%%%             initial state (0, 0), as a complete program.
%%% 20170714: Added,
%%%             s/1, r/1; syntactic sugar when previous state doesn't matters.
%%%             p/2, n/2, p/1, n/1; edge evaluation.
%%% 20170813: Added,
%%%             xn operator.
%%%             cal(c,n) operators.
%%%           Fixed,
%%%             suppress warnings making slc/2 discontiguous.
%%%

:- module('xae_slc',
            [ slc/1
            , op(100, fx, [a, an, o, on, =,
                           x, xn,
                           cal, calc, caln])    % Operators
            , s/2, r/2, s/1, r/1                % Set/Reset
            , p/2, n/2, p/1, n/1                % Edge
            ]
).

:- discontiguous slc/2.

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

%%%    
%%% Extends slc via operator
%%%    Teach slc to use xor
%%%

%%% Model
%% x (x, zvke, vke)
x(1, 0, 1).
x(0, 1, 1).
x(0, 0, 0).
x(1, 1, 0).
%% xn (x, zvke, vke)
xn(1, 0, 0).
xn(0, 1, 0).
xn(0, 0, 1).
xn(1, 1, 1).

%%% Extends interpreter
:- op(100, fx, [ x, xn ]).
%% x
slc([x IN|Ts], (RLO, 1)) :-
        x(IN, RLO, Q), slc(Ts, (Q, 1)).
%% xn
slc([xn IN|Ts], (RLO, 1)) :-
        xn(IN, RLO, Q), slc(Ts, (Q, 1)).

%%%        
%%% Extending slc via a Prolog term
%%%

%%% Models
%% s (in, zq, q)
s(IN, ZQ, Q) :-
    o(IN, ZQ, Q).
%% r (in, zq, q)
r(IN, ZQ, Q) :-
    an(IN, ZQ, Q).

%%% Prototypes for slc  
s(_ZOUT, _OUT).
r(_ZOUT, _OUT).

s(_OUT).
r(_OUT).

%%% Extending
%% s 
slc([s(ZOUT, OUT)|Ts], (RLO, _FC)) :-
    s(RLO, ZOUT, OUT), slc(Ts, (RLO, 0)).
slc([s(OUT)|Ts], (RLO, _FC)) :-
    s(RLO, _ZOUT, OUT), slc(Ts, (RLO, 0)).
%% r 
slc([r(ZOUT, OUT)|Ts], (RLO, _FC)) :- 
    r(RLO, ZOUT, OUT), slc(Ts, (RLO, 0)).
slc([r(OUT)|Ts], (RLO, _FC)) :- 
    r(RLO, _ZOUT, OUT), slc(Ts, (RLO, 0)).
    
%%%    
%%% Extends slc with parens, squared, evaluation
%%% Use cases,
%%%    ..., a [.,.], ...
%%%    ..., o ([.,.]), ...
%%%

%%% Extending interpreter

%% a/a[]
slc([a IN|Ts], (RLO, 1)) :-
    aoboa(IN, Ts, RLO, a).

%% o/o[]
slc([o IN|Ts], (RLO, 1)) :-
    aoboa(IN, Ts, RLO, o).
    
%%% Metapredicate
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
        
%%%        
%%% Edge evaluation
%%%

%%% Models
%% p (in, zq, q)
p(IN, ZQ, Q) :-
    an(ZQ, 1, VKE),
    a(VKE, IN, Q).
%% n (in, zq, q)
n(IN, ZQ, Q) :-
    an(IN, 1, VKE),
    a(VKE, ZQ, Q).

%%% Prototypes for slc  
p(_ZOUT, _OUT).
n(_ZOUT, _OUT).

p(_OUT).
n(_OUT).

%%% Extending
%% p 
slc([p(ZOUT, OUT)|Ts], (RLO, _FC)) :-
    p(RLO, ZOUT, OUT), slc(Ts, (RLO, 0)).
slc([p(OUT)|Ts], (RLO, _FC)) :-
    p(RLO, _ZOUT, OUT), slc(Ts, (RLO, 0)).
%% n 
slc([n(ZOUT, OUT)|Ts], (RLO, _FC)) :- 
    n(RLO, ZOUT, OUT), slc(Ts, (RLO, 0)).
slc([n(OUT)|Ts], (RLO, _FC)) :- 
    n(RLO, _ZOUT, OUT), slc(Ts, (RLO, 0)).
    

%%% Extends slc with cal(ling) a prolog term
%%% Use cases,
%%%    ..., cal functor(args),...
%%%    ..., calc functor(args), ...
%%%    ..., caln functor(args), ...

%%% Extending interpreter
:- op(100, fx, [ cal, calc, caln ]).

%% cal
slc([cal IN|Ts], (RLO, FC)) :-
    do_cal(IN),
    slc(Ts, (RLO, FC)).

%% calc
slc([calc _IN|Ts], (0, _FC)) :-
    slc(Ts, (0, 0)).
slc([calc IN|Ts], (1, _FC)) :-   
    do_cal(IN),
    slc(Ts, (1, 0)).

%% caln
slc([caln _IN|Ts], (1, _FC)) :-
    slc(Ts, (1, 0)).
slc([caln IN|Ts], (0, _FC)) :-
    do_cal(IN),
    slc(Ts, (0, 0)).

% do_cal
do_cal(IN) :-
    IN =.. [Functor|Args],
    apply(Functor, Args).


