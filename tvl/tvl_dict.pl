/*   tvl_dict.pl

       Dicts TVL's utilities

   (c) 2020, xae. Juan José Eraso Escalona
 
   20200811
*/


% from_dict
%   Maps an anonymous dict TVL to non dict

% from_dict/2
% from_dict(-TVL_d, +TVL_t)
from_dict(TVL_d, TVL_t) :-
     maplist( [IN, OUT] >> (
                             values([IN], Values)  
                           , OUT=..[term | Values]
                           )
            , TVL_d, TVL_t ).

% from_dict/3
% from_dict(+Keys, -TVL_d, +TVL_t)
from_dict(Keys, TVL_d, TVL_t) :-
     maplist( [IN, OUT] >> (
                             values(Keys, [IN], Values)  
                           , OUT=..[term | Values]
                           )
            , TVL_d, TVL_t ).

% to_dict
% from non dict to dict

% to_dict/3
% to_dict(+TVL_t, +Keys, -TVL_d)
to_dict([], _Keys, []) :- !.
to_dict(TVL_t, Keys, TVL_d) :-
     to_list(TVL_t, TVL_l),

     maplist( [Term, term(Dict)] >> 
               (
                 maplist( [Value, Key, Compound] >> 
                           (
                             Compound = Key:Value   
                           )
                        , Term, Keys, Data )
               , dict_create( Dict, _, Data)               
               )
            , TVL_l, TVL_d ).
     
     
% keys/Valued
% Dict keys/values

% keys/2
% keys(+TVL_d, -Keys)
keys([Term | _], Keys) :-
    Term =.. [term | [Vals]],
    findall(Key, get_dict(Key, Vals, _Value), Keys).

% values/2
% values(+TVL_d, -Values)
values([Term | _], Values) :-
    Term =.. [term | [Vals]],
    findall(Value, get_dict(_Key, Vals, Value), Values). 
                                                                       
% values/3
% values(+Keys, +TVL_d, -Values)
values(Keys, [Term | _], Values) :-
    Term =.. [term | [Vals]],
    maplist( [Key, Val] >> (
                             get_dict(Key, Vals, Val)
                           )
           , Keys, Values ). 
           

% grounded_dict   
% grounded_dict(+Dict, -Result)        
%   No vars as values

% grounded_dict/2
% grounded_dict(+Dict, -Result)
grounded_dict(Dict, Result) :-
    dict_pairs(Dict, _, Pairs),
    exclude( [(_Key-Val)] >> (
                              var(Val)
                             )
           , Pairs, Grounded ),
    dict_create(Result, _, Grounded).
    
    
                                                               
% match
%   Match a TVL with a dict
%   Used to extend the keys of a dict

% match/3
% match( +TVL, +From, -Matched)
match( TVL, From, Matched) :-
    maplist( [term(Term), OUT] >> (
                                    copy_term(From, CFrom)
                                  , Term :< CFrom
                                  , OUT =.. [term | [CFrom]]
                                  )
           , TVL, Matched ).    


% unity_dict
%   Dict of ungrounded values

% unity_dict/2
% unity_dict(+Keys, -Dict)
unity_dict(Keys, Dict) :-
    maplist( [Key, OUT] >> (
                             OUT = Key:_
                           )
           , Keys, Data ),
    dict_create(Dict, _, Data).      

