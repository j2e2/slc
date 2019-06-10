%%%
%%% aoboa.pl
%%%
%%% Switching logic circuit
%%%   and/or before or/and
%%%
%%% (c) 2016, xae. Juan José Eraso Escalona
%%% 20160826
%%% 20170709: Fixed,
%%%             aoboa, call to slc with IN as in list case should have
%%%             initial state (0, 0), as a complete program.
%%%

:- use_module(slc).

%%% Extends slc with parens, squared, evaluation
%%% Use cases,
%%%    ..., a [.,.], ...
%%%    ..., o [.,.], ...

%%% Extending interpreter

%% a/a[]
xae_slc:slc([a IN|Ts], (RLO, 1)) :-
    aoboa(IN, Ts, RLO, xae_slc:a).

%% o/o[]
xae_slc:slc([o IN|Ts], (RLO, 1)) :-
    aoboa(IN, Ts, RLO, xae_slc:o).
    
%%% Metapredicate
%% IN,
%%    as list is appended with an auxiliar variable assigned to 
%%    its proper value, ARLO.
%% OP,
%%    selects and/or semantics.
aoboa(IN, Ts, RLO, OP) :-
    is_list(IN) 
    ->  lists:append(IN, [= ARLO], Ys),
        xae_slc:slc(Ys, (0, 0)), 
        call(OP, RLO, ARLO, Q),
        xae_slc:slc(Ts, (Q, 1))
    ;   call(OP, IN, RLO, Q),
        xae_slc:slc(Ts, (Q, 1)).
    
