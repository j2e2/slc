%%% mix-tank.pl
%%%
%%% Switching logic circuit
%%%  mix tank example
%%%  Verification of PLC programs, Richard Susta. 
%%%  http://susta.cz/fel/publications/sustathesis.pdf
%%%  Example 4.2
%%%
%%% (c) 2016, xae. Juan José Eraso Escalona
%%% 20160821
%%%

:- module('xae_mix_tank',
 [ mix_tank/11 
 , mix_tank_swrpmp/8
 ]
).

:- use_module(slc).

%%% Extending slc via a Prolog term

%%% Models
%% s (in, zq, q)
s(IN, ZQ, Q) :-
    xae_slc:o(IN, ZQ, Q).
%% r (in, zq, q)
r(IN, ZQ, Q) :-
    xae_slc:an(IN, ZQ, Q).

%%% Prototypes for slc  
s(_ZOUT, _OUT).
r(_ZOUT, _OUT).

s(_OUT).
r(_OUT).

%%% Extending

%% s 
xae_slc:slc([s(ZOUT, OUT)|Ts], (RLO, _FC)) :-
    s(RLO, ZOUT, OUT), xae_slc:slc(Ts, (RLO, 0)).
xae_slc:slc([s(OUT)|Ts], (RLO, _FC)) :-
    s(RLO, _ZOUT, OUT), xae_slc:slc(Ts, (RLO, 0)).
%% r 
xae_slc:slc([r(ZOUT, OUT)|Ts], (RLO, _FC)) :- 
    r(RLO, ZOUT, OUT), xae_slc:slc(Ts, (RLO, 0)).
xae_slc:slc([r(OUT)|Ts], (RLO, _FC)) :- 
    r(RLO, _ZOUT, OUT), xae_slc:slc(Ts, (RLO, 0)).


%%% LD circuit
%%%    Best with a fixed font
/*
// Network 1
// Positive transition of clr input

|        clr                   clredge
+--------| |---------|P|------------( )
|       

// Network 2
// Pump control

|         dn      swrspd           pmp  
+--------| |----+----|/|------------(S)
|               |             
|        dne    |  
+--------| |----+
|

// Network 3

|         up                       pmp  
+--------| |----+-------------------(R)
|               |             
|        upe    |  
+--------| |----+
|

// Network 4
// Swirler control

|      dn       dne    pmpspd      swr 
+-----|/|-------|/|-------|/|-------(S)
|

// Network 5

|         dn                       swr  
+--------| |----+-------------------(R)
|               |             
|        dne    |  
+--------| |----+
|

// Network 6
// Errors of level control

|        upe                      erru 
+--------| |------------------------(S)
|                        
           
// Network 7

|        dne                      errd 
+--------| |------------------------(S)
|                        
  
// Network 8
// Clearing errors

|    clredge                      erru 
+--------| |------------------+-----(R)
|                             |
                              |   errd  
                              + ----(R)
*/    

%%% PLC program model
%% Network 1
%% Positive transition of clr input
network_1(ZCLR, CLR, CLREDGE) :-
    slc([
            an  ZCLR,
            a   CLR,
            =  CLREDGE  
    ]).
    
%% Network 2
%% Pump control
network_2(DN, DNE, SWRSPD, ZPMP, PMP) :-
    slc([
            a   DN,
            o   DNE,
            an  SWRSPD,
            s(ZPMP, PMP)
    ]).
    
%% Network 3
network_3(UP, UPE, ZPMP, PMP) :-
    slc([
            a   UP,
            o   UPE,
            r(ZPMP, PMP)
            
    ]).

%% Network 4
%% Swirler control
network_4(DN, DNE, PMPSPD, ZSWR, SWR) :-
    slc([
            an  DN,
            an  DNE,
            an  PMPSPD,
            s(ZSWR, SWR) 
    ]).
   
%% Network 5
network_5(DN, DNE, ZSWR, SWR) :-
    slc([
            a   DN,
            o   DNE,
            r(ZSWR, SWR)
    ]).
    
%% Network 6
%% Errors of level control
network_6(UPE, ERRU) :-
    slc([
            a   UPE,
            s(ERRU)
    ]).
    
%% Network 7
network_7(DNE, ERRD) :-
    slc([
            a   DNE,
            s(ERRD)
    ]).
    
%% Network 8
%% Clearing errors
network_8(CLREDGE, ZERRD, ZERRU, ERRD, ERRU) :-
    slc([
            a   CLREDGE,
            r(ZERRD, ERRD),
            r(ZERRU, ERRU)
    ]).
    
%% Program
mix_tank( (ZCLR, CLR)
         , DN, DNE
         , UP, UPE
         , SWRSPD, PMPSPD
         , ERRD
         , ERRU
         , (ZSWR, SWR)
         , (ZPMP, PMP) ) :- 
   network_1(ZCLR, CLR, CLREDGE),
   network_2(DN, DNE, SWRSPD, ZPMP, PMP_2),
   network_3(UP, UPE, PMP_2, PMP),
   network_4(DN, DNE, PMPSPD, ZSWR, SWR_4),
   network_5(DN, DNE, SWR_4, SWR),
   network_6(UPE, ERRU_6),
   network_7(DNE, ERRD_7),
   network_8(CLREDGE, ERRD_7, ERRU_6, ERRD, ERRU).
   
%% Program with only PMP and SWR dependencies
mix_tank_swrpmp(  DN, DNE
                , UP, UPE
                , SWRSPD, PMPSPD
                , (ZSWR, SWR)
                , (ZPMP, PMP) ) :- 
   network_2(DN, DNE, SWRSPD, ZPMP, PMP_2),
   network_3(UP, UPE, PMP_2, PMP),
   network_4(DN, DNE, PMPSPD, ZSWR, SWR_4),
   network_5(DN, DNE, SWR_4, SWR).

