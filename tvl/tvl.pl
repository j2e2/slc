/*
    tvl.pl

    Ternary Vector List representation of booleans
      utilities and operators

	See the XBOOLE tool, http://www.informatik.tu-freiberg.de/xboole/

   (c) 2019, xae. Juan José Eraso Escalona
   
   20190615
*/

:- module( 'tvl',
           [ union/3            % union(+As, +Bs, -Unions) 
           , intersection/3     % intersection(+As, +Bs, -Intersections)
           , complement/2       % complement(+As, -Complements)
           , difference/3       % difference(+A, +B, -Difference)
           , orthogonal/2       % orthogonal(+TVL, -Ortogonal)
           , fact_to_tvl/2      % fact_to_tvl(:Fact, -TVL)
           , tvl_to_fact/2      % tvl_to_fact(+TVL, :Functor)
           , tvl_to_list/2      % tvl_to_list(+TVL, -Result)
           , karnaugh_map/2     % karnaugh_map(+ONSet, +OFFSet)
           , quotient/3         % quotient(+DEND, +DSOR, -QUOT) 
           , quotient_nth/4     % quotient_nth(+Index, +Val, +TVL, -Result)
           , edis/3             % edis(+TVL, +Term, -Result) 
           , edis_nth/3         % edis_nth(+Index, +TVL, -Result) 
           , econ/3             % econ(+TVL, +Term, -Result) 
           , econ_nth/3         % econ_nth(+Index, +TVL, -Result) 
           , absorb/2           % absorb(+TVL, -Result)
           , bcf/2              % bcf(+TVL, -Result)
           , nearly_minimal/2   % nearly_minimal(+TVL, -Result)
           , on_set/3           % on_set(+Index, +TVL, -Result)
           , off_set/3          % off_set(+Index, +TVL, -Result)
           , on_mark/3          % on_mark(+Index, +TVL, -Result)
           , off_mark/3         % off_mark(+Index, +TVL, -Result)
           , interval/2         % interval(ONSet, OFFSet)
           , minimal_subsets/3  % minimal_subsets(+ONSet, +OFFSet, -Sets)
           ] ).


:- use_module(library(yall)).

/*
    Utilities
*/

%   until / unless
%
%   A structured alternative to Prolog with simple
%   compositional semantics. ANTÓNIO PORTO. arXiv: 1107.5408
%
%   unless made myself, provably wrong

:- op(999, xfx, [until, unless]).


Solve until  Stop :- Solve, ( Stop, ! ; true ).

Solve unless Stop :- Solve, ( Stop, !, fail ; true ).

% =memberchk/3
% =memberchk(?Element, +List, -T) is det
%   Reified memberchk
=memberchk(Element, List, T) :-    
      memberchk(Element, List)
   ->
      T = 1
   ;
      T = 0.


/*
	A tvl is a list of type term compounds.
	
	Represents a conjuction/disjuction of boolean terms,
	is a boolean equation.  

    In this library a tvl represents a SOP, Sum Of Products,
    if no else stated.

    Don't cares as unbounded vars, true as 1 and false as 0

*/


% Operators

% Set operators

% Union

% union/3
% union(+As, +Bs, -Unions) is det
union([], Bs, Bs) :- !.
union(As, [], As) :- !.
union(As, Bs, Unions) :-
    append(As, Bs, ZUnions),
    absorb(ZUnions, Unions).

% Intersection
%             ___________
%              X _ |  X
%              X X |  X
%              X Y | [ ]
%
%    The operation unit is the term as an empty term could result

% intersection/3
% intersection(+As, +Bs, -Intersections) is det
%
%    Arguments as tvl's
intersection([], _Bs, []) :- !.
intersection(_As, [], []) :- !.
intersection(As, Bs, Intersections) :-
    tvl_to_list(As, VAs),
    tvl_to_list(Bs, VBs),
    intersection_(VAs, VBs, [], Intersections).

% intersection_/4
% intersection_(+As, +Bs, +Acc, -Intersections) is det
intersection_([], _Bs, Acc, Acc) :- !.
intersection_([A | As], Bs, ZAcc, Intersections) :-
	foldl( [B, V0, V1] >> ( 
                             intersect(A, B, Result)
                          -> V1 = [Result | V0]
                          ;  V1 = V0
                          )
         , Bs, ZAcc, Acc ),

    !,
	intersection_(As, Bs, Acc, Intersections).

% intersect/3
% intersect(+As, +Bs, -Result) is det
%     As, list
%     Bs, list
%     Result, term compound
%
%     fail as the empty set
intersect(As, Bs, Result) :-
	intersect_(As, Bs, ZResult)
  unless 
    [] = ZResult

  , Result =.. [term | ZResult].

% intersect_/4
% intersect_(+Xs, +Ys, +Acc, -Result) is det
intersect_([], [], []).
intersect_([X | Xs], [Y | Ys], [Y | ZResult]) :-
    var(X),

    !,
    intersect_(Xs, Ys, ZResult).
intersect_([X | Xs], [Y | Ys], [X | ZResult]) :-
    ( 
      var(Y)
    ; 
      X =:= Y
    ),

    !,
    intersect_(Xs, Ys, ZResult).

% Complement
%           _______
%            _ | _
%            0 | 1
%            1 | 0 

% complement/2
% complement(+As, -Complements) is det
%
%    Arguments as tvl's
complement([], _Complements) :- !, fail.
complement(As, Complements) :-
    As = [H | Ts],
    complement_(H, Unity),
    % Complements intersection
    foldl( [A, V0, V1] >> ( 
                             complement_(A, Results)
			              ,  intersection(V0, Results, V1)
                          )
         , Ts, Unity, Complements ).

% complement_/2
% complement_(+Vars, -Results) is det
%     Complements a term
complement_(Term, Results) :-
    Term =.. [term | Vars],
    % Term length
    length(Vars, Len),
    % Upto a concrete var
    findall(Head, append(Head, X, Vars) until X = [_], Heads), 
    % Complement var
    foldl( [H, V, V0, V1] >> (
                                var(V)
                             -> V1 = V0
                             ;  ( 
                                   do_complement(V, C)
                                ,  append(H, [C], HC)

                                % Extend upto term length 
 				                ,  append(HC, _, Args)
                                ,  length(Args, Len) 

                                ,  Complement =.. [term | Args]
                                ,  V1 = [Complement | V0]
                                )
                             )
         , Heads, Vars, [], Results ).

% do_complement/2
% do_complement(+Val, -Result) is det
%     Complements a three valued logic value 
%
%     Don't cares as nongrounded values
do_complement(0, 1) :- !.
do_complement(1, 0). 
 

% Symmetric difference

% difference/3
% difference(+A, +B, -Difference) is det
difference([], _B, []) :- !.
difference(A, [], A) :- !.
difference(A, B, Difference) :-
    complement(B, Complement),
    intersection(A, Complement, Difference).


% Orthogonalization

% orthogonal/2
% orthogonal(+TVL, -Orthogonal) is det
orthogonal([], []) :- !.
orthogonal(TVL, [TopTerm | OrthogonalTail]) :-
    predsort(compare_term_asc, TVL, TVLSet),
    % complement top term
    TVLSet = [TopTerm | Rest],
    complement_(TopTerm, TopTermComplement),
    % intersect
    intersection(TopTermComplement, Rest, Intersection),
    % sort on number of don't cares
    absorb(Intersection, OrthogonalTail).


% predsort

% compare_term
% In order of number of vars
%    des, first term of less number of vars
%    asc, last term of less number of vars

% asc
compare_term_asc(=, _E1, _E2) :-
    fail.
compare_term_asc(<, E1, E2) :-
    copy_term(E1, CE1),
    copy_term(E2, CE2),
    numbervars(CE1, 0, NE1),
    numbervars(CE2, 0, NE2),
    NE1 > NE2, !.
compare_term_asc(>, _E1, _E2).

%des
compare_term_des(=, _E1, _E2) :-
    fail.
compare_term_des(>, E1, E2) :-
    copy_term(E1, CE1),
    copy_term(E2, CE2),
    numbervars(CE1, 0, NE1),
    numbervars(CE2, 0, NE2),
    NE1 > NE2, !.
compare_term_des(<, _E1, _E2).


% Absorbtion

% absorb/2
% absorb(+TVL, -Result)
absorb([], []) :- !.
absorb(TVL, Result) :-
    predsort(compare_term_des, TVL, TVLS),
    TVLS = [H | Ts],
    absorb_([H], Ts, Result).

absorb_(Result, [], Result) :- !.   
absorb_(ZHs, [C | ZTs],  Result) :-
    exclude(absorbs(C), ZHs, Hs),
    exclude(absorbs(C), ZTs, Ts),

    !,
    absorb_([C | Hs], Ts, Result).

absorbs(A, B) :-
    subsumes_term(A, B).


% term_nth
% term_nth(+Index, +Val, +TVL, -Term)
term_nth(Index, Val, TVL, Term) :-
    TVL = [H | _],
    functor(H, term, Arity),
    functor(Term, term, Arity),
    setarg(Index, Term, Val).


% Blake canonical form

% bcf/2
% bcf(+TVL, -Result)
%     Successive extraction
bcf([], []) :- !.
bcf(TVL, Result) :-
    absorb(TVL, TVLA),
    bcf_(TVLA, Result).
  
bcf_(TVL, Result) :-
    TVL = [H | _],
    functor(H, term, Arity),
    Index is Arity - 1,
    numlist(0, Index, Is),
    foldl( [I, V0, V1] >> ( implicants(I, V0, Implicants)
                          , union(V0, Implicants, V1)
                          )
         , Is, TVL, Result ).

% implicants
% implicants/3
implicants(Index, [H | Ts], Result) :-
    H =.. [term | HVars],
    implicants_(Index, [HVars], Ts, [], Result).

% implicants_/5
implicants_(_I, _Hs, [], Result, Result) :- !.
implicants_(I, Hs, [C | ZTs], ZAcc, Result) :-
   C =.. [term | CVars],
   foldl( [H, V0, V1] >> (
                            consensus(I, CVars, H, Consensus)
                         -> V1 = [Consensus | V0]
                         ;  V1 = V0
                         )
        , Hs, [], Implicants ),
   
   append(ZAcc, Implicants, Acc),

   !,
   implicants_(I, [CVars | Hs], ZTs, Acc, Result). 

% consensus
% consensus/4
consensus(Index, AVars, BVars, Consensus) :-   
    nth0(Index, AVars, V0),
    nonvar(V0),
    nth0(Index, BVars, V1),
    nonvar(V1),
    V0 =\= V1,

    copy_term(AVars, DAVars),
    copy_term(BVars, DBVars),

    (
       length(X, Index)
    ,  append(X, [V0 | Y], DAVars)
    ,  append(X, [V1 | Y], DBVars)
    ),

    append(X, [_ | Y], Vars),
    duplicate_term(Vars, DVars),
    Consensus =.. [term | DVars]. 
   

% Tautology

% tautology/1
% tautology(TVL)
tautology([Term]) :-
    Term =.. [term | Vars]
 ->
    maplist(var, Vars).
tautology(TVL) :-
    complement(TVL, []).


% Quotient

% quotient/3
% quotient(+DEND, +DSOR, -QUOT) is det
quotient([], _B, []) :- !.
quotient(As, B, Quotient) :-
    B =.. [term | Bs], 
 	foldl( [A, V0, V1] >> (  do_quotient(A, Bs, Result)
                          -> V1 = [Result | V0]
                          ;  V1 = V0
                          )
         , As, [], ZQuotient ),

    absorb(ZQuotient, Quotient).

% do_quotient/3
% do_quotient(+As, +Bs, -Result) is det
%     As, tvl
%     Bs, tvl
%     Result, term compound
%
%     fail as the empty set
do_quotient(As, Ys, Result) :-
    As =.. [term | Xs],

      do_quotient_(Xs, Ys, ZResult)
    unless
      [] = ZResult

    , Result =.. [term | ZResult].

% do_quotient_/4
% do_quotient_(+Xs, +Ys, +Acc, -Result) is det
do_quotient_([], [], []).
do_quotient_([X | Xs], [Y | Ys], [X | Result]) :-
    var(Y),

    !,
    do_quotient_(Xs, Ys, Result).
do_quotient_([X | Xs], [Y | Ys], Result) :-
    ( 
      var(X)
    ; 
      X =:= Y
    ),

    !,
    do_quotient_(Xs, Ys, Result).

% quotient_nth/3
% quotient_nth(Index, Val, TVL, Result)
quotient_nth(Index, Val, TVL, Result) :-
    term_nth(Index, Val, TVL, Term),
    quotient(TVL, Term, Result).


% Conjunctive eliminant

% econ/3
% econ(+TVL, +Term, -Result) is det
econ([], _Term, []) :- !.
econ(TVL, Term, Result) :-
   Term =.. [term | TVars],
   bagof( Index, ( nth1(Index, TVars, EIndex)
                 , nonvar(EIndex)
                 )
        , Is ),
   bcf(TVL, BCF),
   exclude(econ_(Is), BCF, ZResult),
   edis(ZResult, Term, Result).

econ_(Is, IN) :-
    once( ( member(I, Is)
          , IN =.. [term | Vals]
          , nth1(I, Vals, Val), nonvar(Val)
          )
        ). 

% econ_nth/3
% econ_nth(+Index, +TVL, -Result)
econ_nth(Index, TVL, Result) :- 
    term_nth(Index, 1, TVL, Term),
    econ(TVL, Term, Result).   


% Disjunctive eliminant

% edis/3
% edis(+TVL, +Term, -Result) is det
edis([], _Term, []) :- !.
edis(TVL, Term, Result) :-
   Term =.. [term | TVars],
   foldl( [IN, V0, V1] >> (  
                             edis_(IN, TVars, ETerm)
                          -> V1 = [ETerm | V0]
                          ;  V1 = V0
                          )
        , TVL, [], ZResult),

   absorb(ZResult, Result).

% edis_/3
% edis_(+Term, +Eliminant, -Result) is det
%
%     fail as the empty set
edis_(Term, EVars, Result) :-
    Term =.. [term | TVars],

      do_edis_(TVars, EVars, Vars)
    unless 
      [] = Vars
      
    , Result =.. [term | Vars].


do_edis_([], [], []).
do_edis_([TVar | TVars], [EVar | EVars], [TVar | Result]) :-
    var(EVar),
    !,
    do_edis_(TVars, EVars, Result).
do_edis_([_TVar | TVars], [_EVar | EVars], Result) :-
    do_edis_(TVars, EVars, Result).


% edis_nth/3
% edis_nth(+Index, +TVL, -Result)
edis_nth(Index, TVL, Result) :- 
    term_nth(Index, 1, TVL, Term),
    edis(TVL, Term, Result).


% nearly_minimal

% nearly_minimal/2
% nearly_minimal(+TVL, -Result)
nearly_minimal([], []) :- !.
nearly_minimal(TVL, Result) :-
    bcf(TVL, BCFS),
    tvl_varx(BCFS, BCFSx), 
    foldl( [Termx, V0x, V1x] >> (
                                   selectchk(Termx, V0x, VTx)

                                   % vars again
                                ,  tvl_xvar(VTx, VT)            
                                ,  term_xvar(Termx, Term)
 
                                ,  quotient(VT, Term, Q)

                                ,  tautology(Q)
                                -> V1x = VTx
                                ;  V1x = V0x
                             )
         , BCFSx, BCFSx, ZResult),

    tvl_xvar(ZResult, Result).


% mark functions 

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
    quotient_nth(Index, Val, TVL, Q),
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
minimal_subsets(QAs, QCAs, Sets) :-
    QAs = [H | _],
    functor(H, term, Arity),
    functor(Unity, term, Arity),

    tvl_to_list(QAs, Qs),
    
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
                               intersection(V0, Opposed, ZV1)
                             , absorb(ZV1, V1)
                             )
                          )
                       )
                    , Qs, [Unity], OUT)
            )
          , QCAs, [HO | TOs] ),
   foldl(intersection, TOs, HO, ZSets),

   absorb(ZSets, Sets).

% opposed(+Arity, +As, +Bs, -Result)
opposed(Arity, As, Bs, Result) :-
    opposed_(As, Bs, 1, [], ZResult),
    maplist( [IN, OUT] >> (
                            functor(OUT, term, Arity)
                          , setarg(IN, OUT, 1)
                          )
           , ZResult, Result ).

% opposed_(+As, +Bs, +Index, +Acc, -Result)
opposed_([A | As], [B | Bs], ZIndex, ZAcc, Result) :-
    ( 
       (
          nonvar(A)
       ,  nonvar(B)
       ,  A =\= B
       )       
    -> Acc = [ZIndex | ZAcc]
    ;  Acc = ZAcc
    ),
    Index is ZIndex + 1, 

    !,
    opposed_(As, Bs, Index, Acc, Result).
opposed_([], [], _Index, Result, Result).
  
  
% Conversion

% Var mapping
% Bypass the unification of free vars

% swaps atom x and var
varx(X, x) :- var(X), !.
varx(1, 1).
varx(0, 0).

% TVL scoped
tvl_varx(TVL, Result) :-
    maplist(term_varx, TVL, Result).

tvl_xvar(TVL, Result) :-
    maplist(term_xvar, TVL, Result).
  
% On terms
term_varx(Term, Result) :-
    Term =.. [term | TVars],
    maplist(varx, TVars, Vars),
    Result =.. [term | Vars].

term_xvar(Term, Result) :-
    Term =.. [term | TVars],
    maplist( [IN, OUT] >> varx(OUT, IN)
           , TVars, Vars),
    Result =.. [term | Vars].

% tvl_to_list/2
% tvl_to_list(+TVL, -Result)
tvl_to_list(TVL, Results) :-
    maplist( [Term, Result] >> (Term =.. [term | Result])
           , TVL, Results ).

% fact_to_tvl/2
% fact_to_tvl(+Fact, -TVL) is det  
fact_to_tvl(Generator, TVL) :-
    findall( Term, ( 
                      clause(Generator, true)
                   ,  Generator =.. [_ | Args]
                   ,  Term =.. [term | Args]
                   )
           , ZTVL ),

    orthogonal(ZTVL, TVL).
 
% tvl_to_fact/2  
% tvl_to_fact(+TVL, :Functor) is det
%    Facts are generated in module tvl scope
tvl_to_fact(TVL, Functor) :-
    orthogonal(TVL, Orthogonal),
    forall( (  member(Term, Orthogonal)
            ,  Term =.. [term | Args]
            ,  Fact =.. [Functor | Args]
            )
          , assertz(Fact) ).


% Karnaugh-map

% karnaugh_map/2
% karnaugh_map(+ONSet, +OFFSet)
%     Writes a Karnaugh-map representation of the ON/OFF sets.
karnaugh_map(ONSet, OFFSet) :- 
    ONSet = [Term | _],
    functor(Term, term, Arity),
    karnaugh_map(ONSet, OFFSet, Arity).

% karnaugh_map/3
% karnaugh_map(+ONSet, +OFFSet, +Arity)
karnaugh_map(ON, OFF, Arity) :-
    VarRow is div(Arity, 2),
    VarCol is Arity - VarRow,
    Rows is (2 ^ VarRow) - 1,
    Cols is (2 ^ VarCol) - 1,
    numlist(0, Rows, RowBinary),
    numlist(0, Cols, ColBinary),
    maplist(binary_to_graychars(VarRow), RowBinary, RowChars),
    maplist(binary_to_graychars(VarCol), ColBinary, ColChars),

    % Header
    format( '~n~1+~*+', [VarRow]),
    forall( member(Val, ColChars)
          , format('~*+~1+~w~*+', [VarRow, Val, VarCol]) 
          ), 
    % Rows
    forall( member(RowChar, RowChars)
          , ( format( '~n~1+~w~*+'
                    , [RowChar, VarRow] )
            , forall( member(ColChar, ColChars)
                    , format( '~` t~*+~*+~@'
                            , [ VarRow, VarCol
                              , wmv(ColChar, RowChar, ON, OFF) ]
                            )
                    )
            )
          ).         
% wmv/4
% wmv(+Col, +Row, +ONSet, +OFFSet) 
%    Writes the map value
wmv(Col, Row, ONSet, OFFSet) :-
    atom_chars(Col, ColChars),
    atom_chars(Row, RowChars),
    append(ColChars, RowChars, CodeChars),
    maplist( [IN, OUT] >> number_chars(OUT, [IN])
           , CodeChars, Code ),

    Term =.. [term | Code], 

    =memberchk(Term, ONSet, ON),
    =memberchk(Term, OFFSet, OFF),
    wmv_(ON, OFF).

wmv_(1, 1) :- write(x).
wmv_(1, 0) :- write(1).
wmv_(0, 1) :- write(0).
wmv_(0, 0) :- write(-).


% binary_to_graychars/3
% binary_to_graychars(+Len, +Binary, -Gray)
binary_to_graychars(Len, Binary, Gray) :-
    AGray is Binary xor (div(Binary, 2)),
    binary_to_chars(Len, AGray, Gray).

binary_to_chars(Len, Binary, C) :-
    format( atom(C)
          , '~`0t~2r~*+'
          , [Binary, Len] ).
    

% Portray hook

:- dynamic   
       user:portray/1.

:- multifile
       user:portray/1.

% portray/1
% user:portray(+Term)
user:portray(Term) :-
   functor(Term, term, _Arity),
   copy_term(Term, CTerm),
   numbervars(CTerm, 0, _, [singletons(true)]),
   CTerm =.. [_ | Args],
   nl, write(Args).


