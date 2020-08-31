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
    lists:union(AKeys, BKeys, Keys),

    % Match TVL's
    unity_dict(Keys, From),

    match(As, From, MAs),
    match(Bs, From, MBs),

    % Values union
    append(MAs, MBs, ZUnions),
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
    % Keys union
    keys(As, AKeys),
    keys(Bs, BKeys),
    lists:union(AKeys, BKeys, Keys),

    % Match TLV's
    unity_dict(Keys, From),

    match(As, From, MAs),
    match(Bs, From, MBs),

    % Intersection
    from_dict(Keys, MAs, As_t),
    from_dict(Keys, MBs, Bs_t),

    intersection_t(As_t, Bs_t, ZIntersections),

    to_dict(ZIntersections, Keys, Intersections).

% intersection_/4
% intersection_(+As, +Bs, +Acc, -Intersections) is det
intersection_([A | As], Bs, ZAcc, Intersections) :-
	foldl( [B, V0, V1] >> ( 
                             intersect(A, B, Result)
                          -> V1 = [Result | V0]
                          ;  V1 = V0
                          )
         , Bs, ZAcc, Acc ),

    !,
	intersection_(As, Bs, Acc, Intersections).
intersection_([], _Bs, Acc, Acc).

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
    absorb(TVL, TVLA),
    % Cast
    keys(TVLA, Keys),
    from_dict(TVLA, As),

    As = [H | Ts],
    complement_(H, Unity_t),

    % Complements intersection
    foldl( [A, V0, V1] >> ( 
                             complement_(A, Results)
			              ,  intersection_t(V0, Results, V1)
                          )
         , Ts, Unity_t, Complements_t ),
    to_dict(Complements_t, Keys, Complements).

% complement_/2
% complement_(+Vars, -Results) is det
%     Complements a term
complement_(Term, Results) :-
    Term =.. [term | Vars],
    complement_([], Vars, Results).

% complement_/3
% complement_(+Heads, +Tails, -Results) is det
%     Complements a term variable
complement_(ZHeads, [Var | Tails], Results) :-
    var(Var),
    append(ZHeads, [Var], Heads),

    !,
    complement_(Heads, Tails, Results).
complement_(ZHeads, [Var | Tails], [Result | Results]) :-
    append(ZHeads, [Var], Heads),
    
    do_complement(Var, Complement),    

    length(Tails, Len),
    length(Abbutal, Len),
    append(ZHeads, [Complement | Abbutal], ZResult),
    Result =.. [term | ZResult],

    !,
    complement_(Heads, Tails, Results).
complement_(_Heads, [], []).

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
    complement([TopTerm], TopTermComplement),
    % intersect
    intersection(TopTermComplement, Rest, Intersection),
    % sort on number of don't cares
    absorb(Intersection, OrthogonalTail).


% Absorbtion

% absorb/2
% absorb(+TVL, -Result)
absorb([], []) :- !.
absorb(TVL, Result) :-
    predsort(compare_term_des, TVL, TVLS),
    TVLS = [H | Ts],
    absorb_([H], Ts, Result).

absorb_(ZHs, [C | ZTs],  Result) :-
    exclude(absorbs(C), ZHs, Hs),
    exclude(absorbs(C), ZTs, Ts),

    !,
    absorb_([C | Hs], Ts, Result).
absorb_(Result, [], Result).   

absorbs(A, B) :-
    subsumes_term(A, B).


% Quotient

% quotient/3
% quotient(+DEND, +DSOR, -QUOT) is det
quotient([], _B, []) :- !.
quotient(As, B, Quotient) :-
    foldl( [term(A), V0, V1] >> ( 
                                  copy_term(B, CB)
                                , copy_term(A, CA)
                                ,  select_dict(CB, CA, Result)
                                -> V1 = [term(Result) | V0]
                                ;  V1 = V0
                                )
         , As, [], ZQuotient ),

    absorb(ZQuotient, Quotient).


