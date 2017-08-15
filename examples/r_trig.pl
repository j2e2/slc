%%%
%%% r_trig.pl
%%%
%%% Switching logic
%%%   cal example
%%%
%%%   r_trig function as IEC definition
%%%
%%% (c) 2017, xae. Juan José Eraso Escalona
%%% 20170814
%%%

:- use_module('slc').

%%% r_trig
%%%     rising edge evaluation


%%    Q true at rising edge in CLK, (ZCLK = 0, CLK = 1).
%%    ZCLK, previous state of CLK

r_trig(ZCLK, CLK, Q) :-
   xae_slc:slc([
                an ZCLK,
                a  CLK,
                =  Q
              ]).

%% Syntactic sugar, 
%% not interested in previous state.
r_trig(CLK, Q) :-
    r_trig(_ZCLK, CLK, Q).

