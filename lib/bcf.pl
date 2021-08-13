/*   bcf.pl

        Blake canonical form

   (c) 2020, xae. Juan José Eraso Escalona
 
   20200811
*/


:- module( 'tvl_bcf',
           [ bcf/2              % bcf(+TVL, -Result)
           , nearly_minimal/2   % nearly_minimal(+TVL, -Result)
           ] ).
           
           
:- use_module(library(tvl)).           

% bcf/2
% bcf(+TVL, -Result)
%     Successive extraction
bcf([], []) :- !.             
bcf(TVL, Result) :-
    absorb(TVL, TVLA),
    
    % Cast
    keys(TVLA, Keys),
    from_dict(TVLA, Keys, TVLA_t),

    bcf_(TVLA_t, ZResult),
    
    % Cast
    to_dict(ZResult, Keys, Result).
  
bcf_(TVL, Result) :-
    TVL = [H | _],
    functor(H, term, Arity),
    Index is Arity - 1,
    numlist(0, Index, Is),
    bcf_var(Is, TVL, Result).
    
bcf_var([I | Is], TVL, Result) :-
    implicants(I, TVL, Implicants),
    append(TVL, Implicants, ZAcc),
    absorb(ZAcc, Acc),
    bcf_var(Is, Acc, Result).
bcf_var([], Result, Result). 

% implicants
% implicants/3
implicants(Index, [H | Ts], Result) :-
   H =.. [term | HVars],
   implicants_(Ts, Index, [HVars], [], Result).

% implicants_/5
implicants_([C | ZTs], I, Hs, ZAcc, Result) :-
   C =.. [term | CVars],
   implicants_term(Hs, I, CVars, ZAcc, Acc),
   implicants_(ZTs, I, [CVars | Hs], Acc, Result). 
implicants_([], _I, _Hs, Result, Result).

implicants_term([H | Hs], I, CVars, Zero, [Consensus | Result]) :-
    consensus(I, CVars, H, Consensus),
    
    !,
    implicants_term(Hs, I, CVars, Zero, Result).
implicants_term([_H | Hs], I, CVars, Zero, Result) :-
    implicants_term(Hs, I, CVars, Zero, Result).
implicants_term([], _I, _CVars, Zero, Zero).

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


% nearly_minimal

% nearly_minimal/2
% nearly_minimal(+TVL, -Result)
nearly_minimal([], []) :- !.
nearly_minimal(TVL, Result) :-
    bcf(TVL, BCFS),

    nearly_minimal(BCFS, BCFS, Result).  
nearly_minimal([Term | BCFs], Acc, Result) :-
    delete_term(Term, Acc, VT),
    Term = term(ADSor),
    grounded_dict(ADSor, DSor),
    quotient(VT, DSor, Q),
    tautology(Q),
    
    !,
    nearly_minimal(BCFs, VT, Result).
nearly_minimal([_Term | BCFs], Acc, Result) :-
    nearly_minimal(BCFs, Acc, Result).
nearly_minimal([], Result, Result).


