/*
    convoluted_p.pl

    Switching logic circuit
    equating circuits example

    Rising edge evaluation, circuit equating

   (c) 2017, xae. Juan José Eraso Escalona
   20170828
*/


:- use_module(library(slc)).

% Program in (pseudo) AWL,
%
%     UN   CLK
%     S    M
%     A    Q
%     R    M
%     NOP  0
%     U    M
%     U    CLK
%     =    Q 
%
%  Is really only a set/reset with an and gate



% convoluted_p
%     a convoluted rising edge evaluator
%
%     Variants of this program could be found 
%     on vintage Siemens S5.

%    Q true at rising edge in CLK.
%    ZQ, previous state of Q
%    ZCLK_F, CLK_F internal storage, retains the fact that CLK was false.

convoluted_p(CLK, ZCLK_F, CLK_F, ZQ, Q) :-
          slc([
                an  CLK
                s  (ZCLK_F, M),   % Store that CLK was false
                a   ZQ,
                r  (M, CLK_F),    % Clear on rising edge
                a   CLK,          % Evaluate edge value
                a   CLK_F,
                =   Q
              ]).



