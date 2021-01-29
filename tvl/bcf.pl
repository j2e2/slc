/*   bcf.pl

        Blake canonical form

   (c) 2020, xae. Juan José Eraso Escalona
 
   20200811
*/


% bcf/2
% bcf(+TVL, -Result)
%     Successive extraction
bcf([], []) :- !.
bcf(TVL, Result) :-
    absorb(TVL, TVLA),
    
    % Cast
    keys(TVLA, Keys),
    from_dict(TVLA, TVLA_t),

    bcf_(TVLA_t, ZResult),
    
    % Cast
    to_dict(ZResult, Keys, Result).
  
bcf_(TVL, Result) :-
    TVL = [H | _],
    functor(H, term, Arity),
    Index is Arity - 1,
    numlist(0, Index, Is),
    foldl( [I, V0, V1] >> ( 
                            implicants(I, V0, Implicants)
                          , append(V0, Implicants, ZV1)
                          , absorb(ZV1, V1)
                          )
         , Is, TVL, Result ).

% implicants
% implicants/3
implicants(Index, [H | Ts], Result) :-
   H =.. [term | HVars],
   implicants_(Index, [HVars], Ts, [], Result).

% implicants_/5
implicants_(I, Hs, [C | ZTs], ZAcc, Result) :-
   C =.. [term | CVars],
   foldl( [H, V0, V1] >> (
                            consensus(I, CVars, H, Consensus)
                         -> V1 = [Consensus | V0]
                         ;  V1 = V0
                         )
        , Hs, ZAcc, Acc ),


   !,
   implicants_(I, [CVars | Hs], ZTs, Acc, Result). 
implicants_(_I, _Hs, [], Result, Result).

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
tautology(TVL) :-
    complement(TVL, []).


% nearly_minimal

% nearly_minimal/2
% nearly_minimal(+TVL, -Result)
nearly_minimal([], []) :- !.
nearly_minimal(TVL, Result) :-
    bcf(TVL, BCFS),

    foldl( [Term, V0, V1] >> (
                               delete_term(Term, V0, VT)
                             , Term = term(ADsor)
                             , grounded_dict(ADsor, Dsor)
                             , quotient(VT, Dsor, Q)

                             , (
                                  tautology(Q)
                               -> V1 = VT
                               ;  V1 = V0
                               )                        
                             )
         , BCFS, BCFS, Result ).
      

