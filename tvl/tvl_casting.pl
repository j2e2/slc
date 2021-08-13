/*   tvl_casting.pl

     Several casts       

   (c) 2020, xae. Juan José Eraso Escalona
 
   20200811
*/


% Conversion


% from_dict
%   Maps an anonymous dict TVL to non dict

% from_dict/2
% from_dict(-TVL_d, +TVL_t)
from_dict([IN | TVL_d], [OUT | TVL_t]) :-
    values(IN, Values),
    OUT =.. [term | Values],
    from_dict(TVL_d, TVL_t).
from_dict([], []).

% from_dict/3
% from_dict(-TVL_d, +Keys, +TVL_t)
from_dict([IN | TVL_d], Keys, [OUT | TVL_t]) :-
    values(Keys, IN, Values),
    OUT =.. [term | Values],
    from_dict(TVL_d, Keys, TVL_t).
from_dict([], _Keys, []).



% to_dict
% from non dict to dict

% to_dict/3
% to_dict(+TVL_t, +Keys, -TVL_d)
to_dict([], _Keys, []) :- !.
to_dict(TVL_t, Keys, TVL_d) :-
     to_list(TVL_t, TVL_l),

     maplist( [IN, term(OUT)] >> 
                 (
                   to_dict_data(Keys, IN, Data)
                 , dict_create(OUT, _, Data)
                 )
            , TVL_l, TVL_d ).
     
to_dict_data( [Key | Keys], [Value | Values]
            , [Key:Value | Data]) :-
    to_dict_data(Keys, Values, Data).
to_dict_data([], [], []).
            

% to_list/2
% to_list(+TVL, -Result)
to_list([TVL | TVLs], [Args | Result]) :-
    TVL =.. [term | Args],
    to_list(TVLs, Result).
to_list([], []).


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

