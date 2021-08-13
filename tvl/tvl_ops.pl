/*   tvl_ops.pl

        Operators

   (c) 2020, xae. Juan José Eraso Escalona

   20200811
*/


% Set operators

% Union

% union/3
% union(+As, +Bs, -Unions) is det
union([], Bs, Bs) :- !.
union(As, [], As) :- !.
union(As, Bs, Unions) :-
    % Keys union
    keys(As, AKeys),
    keys(Bs, BKeys),
    ord_union(AKeys, BKeys, Keys),

    % Match TVL's
    unity_dict(Keys, From),

    % Values union
    append(As, Bs, ZMs),
    absorb(ZMs, Ms),
    match(Ms, From, Unions).
    

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
    % Keys union
    keys(As, AKeys),
    keys(Bs, BKeys),
    ord_union(AKeys, BKeys, Keys),

    % Match TLV's
    unity_dict(Keys, From),

    match(As, From, MAs),
    match(Bs, From, MBs),

    % Intersection
    from_dict(MAs, Keys, As_t),
    from_dict(MBs, Keys, Bs_t),

    intersection_t(As_t, Bs_t, Intersections_t),
    
    absorb(Intersections_t, ZIntersections),

    to_dict(ZIntersections, Keys, Intersections).

% intersection_/4
% intersection_(+As, +Bs, +Acc, -Intersections) is det
intersection_([], _Bs, Acc, Acc).
intersection_([A | As], Bs, ZAcc, Intersections) :-
	intersection_term(Bs, A, ZAcc, Acc),
	intersection_(As, Bs, Acc, Intersections).

intersection_term([], _A, Acc, Acc).     
intersection_term([B | Bs], A, ZAcc, Acc) :-
     intersect(A, B, Result)
  -> 
     intersection_term(Bs, A, [Result | ZAcc], Acc) 
  ;
     intersection_term(Bs, A, ZAcc, Acc).

intersection_t([], _Bs, []) :- !.
intersection_t(_As, [], []) :- !.
intersection_t(As, Bs, Intersections) :-
    to_list(As, VAs),
    to_list(Bs, VBs),
    intersection_(VAs, VBs, [], Intersections).

% intersect/3
% intersect(+As, +Bs, -Result) is semidet
%     As, list
%     Bs, list
%     Result, term compound
%
%     fail as the empty set
intersect(As, Bs, Result) :-
	intersect_(As, Bs, ZResult),
    Result =.. [term | ZResult].

% intersect_/3
% intersect_(+Xs, +Ys, -Result) is semidet
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
intersect_([], [], []).


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
complement(TVL, Complements) :-
    % Cast
    keys(TVL, Keys),
    from_dict(TVL, Keys, As),

    As = [H | Ts],
    complement_t(H, Unity_t),

    % Complements intersection
    complement_intersection(Ts, Unity_t, Complements_t),
    to_dict(Complements_t, Keys, Complements).

complement_intersection([], Result, Result) :- !.
complement_intersection([A | As], ZAcc, Result) :-
    complement_t(A, CA),
    intersection_t(ZAcc, CA, Acc),
    complement_intersection(As, Acc, Result).
    
% complement_t/2
% complement_t(+Vars, -Results) is det
%     Complements a term
complement_t(Term, Results) :-
    Term =.. [term | Vars],
    complement_(Vars, [], Results).

% complement_/3
% complement_(+Heads, +Tails, -Results) is det
%     Complements a term variable
complement_([Var | Tails], ZHeads, Results) :-
    var(Var),
    append(ZHeads, [Var], Heads),

    !,
    complement_(Tails, Heads, Results).
complement_([Var | Tails], ZHeads, [Result | Results]) :-
    append(ZHeads, [Var], Heads),
    
    do_complement(Var, Complement),    

    length(Tails, Len),
    length(Abbutal, Len),
    append(ZHeads, [Complement | Abbutal], ZResult),
    Result =.. [term | ZResult],

    complement_(Tails, Heads, Results).
complement_([], _Heads, []).

% do_complement/2
% do_complement(+Val, -Result) is det
%     Complements a three valued logic value 
%
%     Don't cares as nongrounded values
do_complement(0, 1).
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
    complement([TopTerm], TopTermComplement),
    % intersect
    intersection(TopTermComplement, Rest, Intersection),
    % sort on number of don't cares
    absorb(Intersection, OrthogonalTail).


% Absorbtion

% absorb/2
% absorb(+TVL, -Result)
absorb([], []) :- !.
absorb([Term], [Term]) :- !.
absorb(TVL, Result) :-
    predsort(compare_term_des, TVL, TVLS),
    TVLS = [H | Ts],
    absorb_(Ts, [H], Result).

absorb_([], Result, Result) :- !.   
absorb_([C | ZTs], ZHs, Result) :-
    exclude(absorbs(C), ZHs, Hs),
    exclude(absorbs(C), ZTs, Ts),
    absorb_(Ts, [C | Hs], Result).

absorbs(A, B) :-
    subsumes_term(A, B).


% Quotient

% quotient/3
% quotient(+DEND, +DSOR, -QUOT) is det
quotient([], _B, []).
quotient([term(A) | As], B, [term(Result) | Quotient]) :-
    copy_term(B, CB),
    copy_term(A, CA),
    
    select_dict(CB, CA, Result),
    
    !,
    quotient_term(As, B, Quotient).    
quotient([term(_) | As], B, Quotient) :-
    quotient(As, B, Quotient).


% delete_term/3
% delete_term(+Term, +TVL, -Result)
%   Select term without grounding 
delete_term(Term, TVL, Result) :-
    copy_term(TVL, From),
    exclude(=(Term), From, Result).     
           
           
% slice/3
% slice(+Keys, +TVL, -Result)
%   Only values for Keys 
slice(Keys, TVL, Result) :-
    sort(Keys, BKeys),
    keys(TVL, AKeys),
    ord_symdiff(AKeys, BKeys, QKeys),
    unity_dict(QKeys, Q),
    quotient(TVL, Q, Result).


% Tautology

% tautology/1
% tautology(TVL)
tautology(TVL) :-
    complement(TVL, []).
    
    
% subtract/3    
% subtract(+Implementation, +Model, -NonConformes)
subtract(Implementation, Model, NonConformes) :-
    keys(Model, ModelKeys),
    keys(Implementation, ImplementationKeys),
    ord_union(ModelKeys, ImplementationKeys, Keys),

    % Match TLV's
    unity_dict(Keys, From),

    match(Model, From, MModel),
    match(Implementation, From, MImplementation),

    exclude( [term(Term)] >> (
                quotient(MModel, Term, Value)
              , tautology(Value)
                             )
           , MImplementation, NonConformes ).  

