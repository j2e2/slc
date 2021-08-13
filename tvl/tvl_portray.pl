/*   tvl_portray.pl

        Portray hook

   (c) 2020, xae. Juan José Eraso Escalona
 
   20200811
   20210221: Dict version
*/

    
:- dynamic   
       user:portray/1.

:- multifile
       user:portray/1.

% portray/1
% user:portray(+Term)
user:portray(term(Term)) :-
   nl,
   forall( get_dict(Key, Term, Value), 
           (
              nonvar(Value)
           -> (
                 Value =:= 0  
              -> format(' ~~~w', Key)
              ;  format('  ~w', Key)
              )
           ;  true
           ) ).
           
user:portray(Term) :-
    functor(Term, term, _Arity),
    copy_term(Term, CTerm),
    numbervars(CTerm, 0, _, [singletons(true)]),
    CTerm =.. [_ | Args],
    nl, write(' '), write(Args).
    
    

