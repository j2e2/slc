/*   
    until.pl

        A structured alternative to Prolog with simple
        compositional semantics. ANTÓNIO PORTO. arXiv: 1107.5408

        until / unless
 
        unless made myself, provably wrong

    (c) 2020, xae. Juan José Eraso Escalona
   
    20200804
*/

% Best with ensure_loded, operators are local to modules
%
%:- module('until',
%            [ 
%              op(999, xfx, [until, unless])
%            ]
%).

:- op(999, xfx, [until, unless]).

Solve until Stop :- Solve, ( Stop, ! ; true ).

Solve unless Stop :- Solve, ( Stop, !, fail ; true ).
