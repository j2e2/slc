/*   mark.pl

        Mark functions

   (c) 2020, xae. Juan José Eraso Escalona
 
   20200811
*/
 
:- module( 'tvl_mark',
           [ on_set/3           % on_set(+Index, +TVL, -Result)
           , off_set/3          % off_set(+Index, +TVL, -Result)
           , on_mark/3          % on_mark(+Index, +TVL, -Result)
           , off_mark/3         % off_mark(+Index, +TVL, -Result)
           , interval/2         % interval(ONSet, OFFSet)
           , minimal_subsets/3  % minimal_subsets(+ONSet, +OFFSet, -Sets)
           ] ).

           
:- use_module(library(tvl)).          

           
% on_set
% on_set/3
% on_set(+Index, +TVL, -Result)
on_set(Index, TVL, Result) :-
    mark(Index, 0, TVL, ON),  
    mark(Index, 1, TVL, OFF),
    difference(ON, OFF, Result).

% off_set
% off_set/3
% off_set(+Index, +TVL, -Result)
off_set(Index, TVL, Result) :-
    mark(Index, 0, TVL, ON),  
    mark(Index, 1, TVL, OFF),
    difference(OFF, ON, Result).

% on_mark
% on_mark/3
% on_mark(+Index, +TVL, -Result)
on_mark(Index, TVL, Result) :-
    mark(Index, 0, TVL, Result).

% off_mark
% off_mark/3
% off_mark(+Index, +TVL, -Result)
off_mark(Index, TVL, Result) :-
    mark(Index, 1, TVL, Result).

% mark
% mark/4
% mark(+Index, +Val, +TVL, -Result)
mark(Index, Val, TVL, Result) :-    
    dict_create(Dsor, _, [Index:Val]),
    quotient(TVL, Dsor, Q),
    complement(Q, Result).

% interval
% interval/2   
% interval(ONSet, OFFSet)
interval(ONSet, OFFSet) :-
    difference(ONSet, OFFSet, ON),
    difference(OFFSet, ONSet, OFF),
    complement(ON, CON),
    complement(OFF, COFF),
    union(CON, COFF, Unity),
    tautology(Unity).


% minimal_subsets
%    minimal u-determining subsets

% minimal_subsets/3
% minimal_subsets(+ONSet, +OFFSet, -Sets)
minimal_subsets(QAsd, QCAsd, Setsd) :-
    % Cast
    keys(QAsd, Keys),
    from_dict(QAsd, Keys, QAs),
    from_dict(QCAsd, Keys, QCAs),

    QAs = [H | _],
    functor(H, term, Arity),
    functor(Unity, term, Arity),
    numlist(1, Arity, Indexes),

    to_list(QAs, Qs),
    
    opposed_vars(QCAs, Qs, Indexes, Arity, [Unity], [HO | TOs]),
    
    foldl(intersection_t, TOs, HO, ZSets),
    absorb(ZSets, Sets),

    % Cast
    to_dict(Sets, Keys, Setsd).
   
opposed_vars([QCA | QCAs], Qs, Indexes, Arity, Unity, [Opposed | Result]) :-
    QCA =.. [term | Vars],
    opposed_term(Qs, Indexes, Vars, Arity, Unity, Opposed),
    opposed_vars(QCAs, Qs, Indexes, Arity, Unity, Result).
opposed_vars([], _Qs, _Indexes, _Arity, _Unity, []).

opposed_term([B | Bs], Indexes, Vars, Arity, ZAcc, Result) :-
    opposed(Indexes, Vars, B, Arity, Opposed),
    intersection_t(ZAcc, Opposed, Intersection ),
    absorb(Intersection, Acc),
    opposed_term(Bs, Indexes, Vars, Arity, Acc, Result).
opposed_term([], _Indexes, _Vars, _Arity, Result, Result).
    
% opposed/5
% opposed(+Indexes, +As, +Bs, +Arity, -Result)
opposed([Index | Indexes], [A | As], [B | Bs], Arity, [OUT | Result]) :-
    opposed(A, B),
      
    !,   
    functor(OUT, term, Arity),
    setarg(Index, OUT, 1),
    opposed(Indexes, As, Bs, Arity, Result).
opposed([_Index | Indexes], [_A | As], [_B | Bs], Arity, Result) :-
    opposed(Indexes, As, Bs, Arity, Result).
opposed([], [], [], _Arity, []).

% opposed/2
% opposed(+A, +B)
opposed(A, B) :-
    nonvar(A),
    nonvar(B),
    A =\= B.

