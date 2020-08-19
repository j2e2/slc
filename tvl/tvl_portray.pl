/*   tvl_portray.pl

        Portray hook

   (c) 2020, xae. Juan José Eraso Escalona
 
   20200811
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
                 Value = 1
              -> format(' ~w', Key)
              ;  format(' ~~~w', Key)
              )
           ;  true
           )
        ).

