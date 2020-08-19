% Synthesis sample
% 
% 
consult(examples(mix_tank)).

swrpmp( DN, DNE
      , UP, UPE
      , SWRSPD ,PMPSPD
      , ZSWR, ZPMP
      , SWR, PMP ) :-
    r(PMP, SWR, SWR1),
    mix_tank_swrpmp( DN, DNE
                   , UP, UPE
                   , SWRSPD, PMPSPD
                   , (ZSWR, SWR1)
                   , (ZPMP, PMP) ).

swrpmp1( S1, R1,
         DN, DNE
       , UP, UPE
       , SWRSPD ,PMPSPD
       , ZSWR, ZPMP
       , SWR, PMP ) :-
    sr1(S1, R1, ZSWR, SWR),
    swrpmp( DN, DNE
          , UP, UPE
          , SWRSPD ,PMPSPD
          , ZSWR, ZPMP
          , SWR, PMP ).

swrpmp2( S1
       , DN, DNE
       , UP, UPE
       , SWRSPD ,PMPSPD
       , ZSWR, ZPMP
       , SWR, PMP ) :-
    o(DN, DNE, R1),
    swrpmp1( S1, R1
           , DN, DNE
           , UP, UPE
           , SWRSPD ,PMPSPD
           , ZSWR, ZPMP
           , SWR, PMP ).

