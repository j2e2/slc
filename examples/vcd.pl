
:- module( 'vcd',
           [ dumpvars/3
           ] ).
           
                     
dumpvars([(ZVar, Var, Id) | Vars], Time, Stream) :-
    put_attr(Var, vcd, (ZVar, Id, Time, Stream)),
    dumpvars(Vars, Time, Stream).
dumpvars([], _Time, _Stream).
 
 
attr_unify_hook((ZVar, Id, Time, Stream), Val) :-
   (  changed(Val, ZVar)
   -> vcd_write(Val, Id, Time, Stream)
   ;  true
   ).
   
changed(0, 1).
changed(1, 0).


vcd_write(Val, Id, Time, Stream) :-
    format(Stream, '#~w~n~w~w~n', [Time, Val, Id]).
       
    
 
