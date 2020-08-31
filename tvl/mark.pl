/*   mark.pl

        Mark functions

   (c) 2020, xae. Juan José Eraso Escalona
 
   20200811
*/
 

% on_set
% on_set/3
% on_set(+Index, +TVL, -Result)
on_set(Index, TVL, Result) :-
    mark(Index, 0, TVL, ON),  
    mark(Index, 1, TVL, OFF),
    difference(ON, OFF, ZResult),
    nearly_minimal(ZResult, Result).

% off_set
% off_set/3
% off_set(+Index, +TVL, -Result)
off_set(Index, TVL, Result) :-
    mark(Index, 0, TVL, ON),  
    mark(Index, 1, TVL, OFF),
    difference(OFF, ON, ZResult),
    nearly_minimal(ZResult, Result).

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
    complement(Q, ZResult),
    nearly_minimal(ZResult, Result).

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
    from_dict(Keys, QAsd, QAs),
    from_dict(Keys, QCAsd, QCAs),

    QAs = [H | _],
    functor(H, term, Arity),
    functor(Unity, term, Arity),

    to_list(QAs, Qs),
    
    maplist( [QCA, OUT] >> 
             (
               QCA =.. [term | Vars]
             , foldl( [B, V0, V1] >> 
                        (
                          opposed(Arity, Vars, B, Opposed)
                        , (
                             [] = Opposed
                          -> V1 = V0
                          ;  (
                               intersection_t(V0, Opposed, ZV1)
                             , absorb(ZV1, V1)
                             )
                          )
                       )
                    , Qs, [Unity], OUT)
            )
          , QCAs, [HO | TOs] ),
   foldl(intersection_t, TOs, HO, ZSets),
   absorb(ZSets, Sets),

   % Cast
   to_dict(Sets, Keys, Setsd).

% opposed/4
% opposed(+Arity, +As, +Bs, -Result)
opposed(Arity, As, Bs, Result) :-
    opposed_(As, Bs, 1, ZResult),
    maplist( [IN, OUT] >> (
                            functor(OUT, term, Arity)
                          , setarg(IN, OUT, 1)
                          )
           , ZResult, Result ).

% opposed_/4
% opposed_(+As, +Bs, +Index, -Result)
opposed_([A | As], [B | Bs], ZIndex, [ZIndex | Result]) :-
    nonvar(A),
    nonvar(B),
    A =\= B,

    Index is ZIndex + 1,

    !,
    opposed_(As, Bs, Index, Result).
opposed_([_A | As], [_B | Bs], ZIndex, Result) :-
    Index is ZIndex + 1,

    !,
    opposed_(As, Bs, Index, Result).
opposed_([], [], _Index, []).

