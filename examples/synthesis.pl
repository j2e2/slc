dv(_,0,0,0).
dv(_,0,1,1).
dv(0,1,_,0).
dv(1,1,_,1).

fsm(1,1,0,1,1).
fsm(1,1,1,0,1).
fsm(_,0,1,1,0).
fsm(0,1,0,0,1).
fsm(0,1,1,0,1).
fsm(0,1,1,1,0).
fsm(_,0,0,1,_).
fsm(_,0,0,0,1).

