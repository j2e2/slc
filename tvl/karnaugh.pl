/*
    karnaugh.pl

        Bare bones Karnaugh map

   (c) 2020, xae. Juan José Eraso Escalona
   
   20200815
*/


% Karnaugh-map

% karnaugh_map/2
% karnaugh_map(+ONSet, +OFFSet)
%     Writes a Karnaugh-map representation of the ON/OFF sets.
karnaugh_map(ONSet, OFFSet) :- 
    keys(ONSet, Keys),

    karnaugh_map_(Keys, ONSet, OFFSet).

% karnaugh_map/3
% karnaugh_map(+Keys, +ONSet, +OFFSet)
%     Writes a Karnaughmap representation of the ON/OFF sets.
%     Keys order.
karnaugh_map(Keys, ONSet, OFFSet) :- 
    karnaugh_map_(Keys, ONSet, OFFSet).

% karnaugh_map_/3
% karnaugh_map_(+Keys, +ONSet, +OFSet)
karnaugh_map_(Keys, ONSet, OFFSet) :-
    % Cast
    from_dict(Keys, ONSet, ON),
    from_dict(Keys, OFFSet,OFF),
    
    nl, write(Keys),

    ON = [Term | _],
    functor(Term, term, Arity),

    VarRow is div(Arity, 2),
    VarCol is Arity - VarRow,
    Rows is (2 ^ VarRow) - 1,
    Cols is (2 ^ VarCol) - 1,
    numlist(0, Rows, RowBinary),
    numlist(0, Cols, ColBinary),
    maplist(binary_to_graychars(VarRow), RowBinary, RowChars),
    maplist(binary_to_graychars(VarCol), ColBinary, ColChars),

    % Header
    format( '~n~1+~*+', [VarRow]),
    forall( member(Val, ColChars)
          , format('~*+~1+~w~*+', [VarRow, Val, VarCol]) 
          ), 
    % Rows
    forall( member(RowChar, RowChars)
          , ( format( '~n~1+~w~*+'
                    , [RowChar, VarRow] )
            , forall( member(ColChar, ColChars)
                    , format( '~` t~*+~*+~@'
                            , [ VarRow, VarCol
                              , wmv(ColChar, RowChar, ON, OFF) ]
                            )
                    )
            )
          ).         


% wmv/4
% wmv(+Col, +Row, +ONSet, +OFFSet) 
%    Writes the map value
wmv(Col, Row, ONSet, OFFSet) :-
    atom_chars(Col, ColChars),
    atom_chars(Row, RowChars),
    append(ColChars, RowChars, CodeChars),
    maplist( [IN, OUT] >> number_chars(OUT, [IN])
           , CodeChars, Code ),

    Term =.. [term | Code], 

    =memberchk(Term, ONSet, ON),
    =memberchk(Term, OFFSet, OFF),
    wmv_(ON, OFF).

wmv_(1, 1) :- write(x).
wmv_(1, 0) :- write(1).
wmv_(0, 1) :- write(0).
wmv_(0, 0) :- write(-).

% =memberchk/3
% =memberchk(?Element, +List, -T) is det
%   Reified memberchk
=memberchk(Element, List, T) :-    
      memberchk(Element, List)
   ->
      T = 1
   ;
      T = 0.


% binary_to_graychars/3
% binary_to_graychars(+Len, +Binary, -Gray)
binary_to_graychars(Len, Binary, Gray) :-
    AGray is Binary xor (div(Binary, 2)),
    binary_to_chars(Len, AGray, Gray).

binary_to_chars(Len, Binary, C) :-
    format( atom(C)
          , '~`0t~2r~*+'
          , [Binary, Len] ).
    

