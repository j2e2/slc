%%%
%%% synchronizer.pl
%%%
%%% Switching logic circuit
%%%   synchronizer example
%%%
%%% (c) 2016, xae. Juan José Eraso Escalona
%%% 20160814
%%%

:- module('xae_synchronizer',
            [ synchronizer/5 
            , security/2
            , liveness/2 
            ]
).

:- use_module('slc').

%%% LD circuit
%%%    Best with a fixed font
/*

// Blinker

                       clock
                    +------+
|      clock        | TON  |
+--------|/|--------|IN    |
|                   |      |
          t#blink---|PT    |
                    +------+

|      clock     blinker         blinker
+--------| |----+----|/|----+--------( )
|               |           |
|    blinker    |  clock    |
+--------| |----+----|/|----+
|


// D Latch
//        fault, lamp should be on 
//        synchro, fault synchronized with clock

|      clock      fault         synchro  
+--------| |--------| |----+--------( )
|                          |
|      clock    synchro    |
+--------|/|--------| |----+
|


// Lamp
// Runs synchronized with blinker

|    blinker    synchro           lamp 
+--------| |--------| |------------( )
|
                          
*/


%%% Extends slc via operator·
%%%    Teach slc to use xor

%%% Model
%% x (x, zvke, vke)
x(1, 0, 1).
x(0, 1, 1).
x(0, 0, 0).
x(1, 1, 0).

%%% Extends interpreter
:- op(100, fx, [ x ]).
%% x
xae_slc:slc([x IN|Ts], (RLO, 1)) :-
        x(IN, RLO, Q), xae_slc:slc(Ts, (Q, 1)).
    
%%% Circuit model
synchronizer( CLOCK
            , (ZBLINKER, BLINKER)
            , FAULT
            , (ZSYNCHRO, SYNCHRO)
            , LAMP ) :-
    slc([   %% Blinker
            a   CLOCK,
            x   ZBLINKER,
            =   BLINKER,
            %% Synchro
            a   CLOCK,
            a   FAULT,
            =   VKE0,
            an  CLOCK,
            a   ZSYNCHRO,
            =   VKE1,
            a   VKE0,
            o   VKE1,
            =   SYNCHRO,
            %% Lamp
            a   BLINKER,
            a   SYNCHRO,
            =   LAMP
        ]).
%% Only interested in blinker and lamp
%% Security properties testing
%%     Lamp and blinker are edge synchronized 
security( (ZBLINKER, BLINKER)
        , (ZLAMP, LAMP) ) :-
    synchronizer( _CLOCK_0
                , (_ZBLINKER_0, ZBLINKER)
                , _FAULT_0
                , (_ZSYNCHRO_0, SYNCHRO_0)
                , ZLAMP ),
    synchronizer( _CLOCK_1
                , (ZBLINKER, BLINKER)
                , _FAULT_1
                , (SYNCHRO_0, _SYNCHRO_1)
                , LAMP ).
%% Only interested in fault and lamp
%% Liveness properties testing
%%     If fault is always off then lamp should eventually go off
%%
%%     If fault is eventually always on then lamp should go on
%%     infinitely many times
liveness( FAULT
        , (ZLAMP, LAMP) ) :-
    synchronizer( _CLOCK_0
                , (_ZBLINKER_0, _BLINKER_0)
                , FAULT
                , (_ZSYNCHRO_0, _SYNCHRO_0)
                , ZLAMP ),
    synchronizer( _CLOCK_1
                , (_BLINKER_0, _BLINKER_1)
                , FAULT
                , (_SYNCHRO_0, _SYNCHRO_1)
                , LAMP ).
            
