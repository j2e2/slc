/*   tvl_casting.pl

     Several casts       

   (c) 2020, xae. Juan José Eraso Escalona
 
   20200811
*/


% Conversion

% to_list/2
% to_list(+TVL, -Result)
to_list(TVL, Results) :-
    maplist( [Term, Result] >> (
                                 Term =.. [term | Result]
                               )
           , TVL, Results ).

% to_fact/3  
% to_fact(+Keys, +TVL, :Functor) is det
%    Facts are generated in module tvl scope
to_fact(Keys, TVL, Functor) :-
    orthogonal(TVL, ATVL),
    from_dict(Keys, ATVL, Orthogonal),
    forall( (
              member(Term, Orthogonal)
            , Term =.. [term | Args]
            , Fact =.. [Functor | Args]
            )
          , assertz(Fact) ).

% from_fact/3
% from_fact(+Keys, :Functor, -TVL) is det  
from_fact(Keys, Functor, TVL) :-
    length(Keys, Len),
    length(Vars, Len),
    Generator =.. [Functor | Vars],

    findall( Term, ( 
                      clause(Generator, true)
                   ,  Generator =.. [_ | Args]
                   ,  Term =.. [term | Args]
                   )
           , ZTVL ),
    to_dict(ZTVL, Keys, ATVL),

    orthogonal(ATVL, TVL).

