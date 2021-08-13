/*   tvl_predsort.pl

        Predsort

   (c) 2020, xae. Juan José Eraso Escalona
 
   20200811
*/

% compare_term
% In order of number of vars
%    des, first term of less number of vars
%    asc, last term of less number of vars

% asc
compare_term_asc(<, E1, E2) :-
    copy_term(E1, CE1),
    copy_term(E2, CE2),
    numbervars(CE1, 0, NE1),
    numbervars(CE2, 0, NE2),
    NE1 > NE2, !.
compare_term_asc(>, _E1, _E2).

%des
compare_term_des(>, E1, E2) :-
    copy_term(E1, CE1),
    copy_term(E2, CE2),
    numbervars(CE1, 0, NE1),
    numbervars(CE2, 0, NE2),
    NE1 > NE2, !.
compare_term_des(<, _E1, _E2).
