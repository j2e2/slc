/*
    sequential.pl
      Sequential operators and devices

   (c) 2019, xae. Juan José Eraso Escalona
   
   20190704

*/

:- module( 'sequential',
           [ sr/4                          % sr(S, R, ZQ, Q)
           , sr/3                          % sr(S, R, Q)
           , sr1/4                         % sr1(S, R, ZQ, Q)
           , sr1/3                         % sr1(S, R, Q)
           , s1r/4                         % s1r(R, S, ZQ, Q)
           , s1r/3                         % s1r(S, R, Q)
           , jk/4                          % jk(J, K, ZQ, Q)
           , jk/3                          % jk(J, K, Q)
           , dv/4                          % dv(D, V, ZQ, Q)
           , dv/3                          % dv(D, V, Q)
           , t/3                           % t(T, ZQ, Q)
           , venjunctor/4                  % venjunctor(ZX, X, Y, Q)
           , venjunctor/3                  % venjunctor(X, Y, Q)
           ] ).


% SR latch

% sr/4
% sr(S, R, ZQ, Q)
sr(1, 0, _, 1).
sr(0, 1, _, 0).
sr(0, 0, X, X).
% sr/3
% sr(S, R, Q)
sr(S, R, Q) :-
  sr(S, R, _ZQ, Q).

%  Priority on reset

% sr1/4
% sr1(S, R, ZQ, Q)
sr1(_, 1, _, 0).
sr1(1, 0, _, 1).
sr1(0, 0, X, X).
% sr1/3
% sr1(S, R, Q)
sr1(S, R, Q) :-
  sr1(S, R, _ZQ, Q).

%  Priority on set

% s1r/4
% s1r(R, S, ZQ, Q)
s1r(1, _, _, 1).
s1r(0, 1, _, 0).
s1r(0, 0, X, X).
% s1r/3
% s1r(S, R, Q)
s1r(R, S, Q) :-
  s1r(R, S, _ZQ, Q).


% DV latch

% dv/4
% dv(D, V, ZQ, Q)
dv(_, 0, Q, Q).
dv(D, 1, _, D).
% dv/3
% dv(D, V, Q)
dv(D, V, Q) :-
  dv(D, V, _ZQ, Q).


% JK latch

% jk/4
% jk(J, K, ZQ, Q)
jk(0, 0, Q, Q).
jk(0, 1, _, 0).
jk(1, 0, _, 1).
jk(1, 1, 0, 1).
jk(1, 1, 1, 0).
% jk/3
% jk(J, K, Q)
jk(J, K, Q) :-
  jk(J, K, _ZQ, Q).


% T latch

% t/3
% t(T, ZQ, Q)
t(0, Q, Q).
t(1, 0, 1).
t(1, 1, 0).

 
% Venjunction

% venjunctor/4
% venjunctor(ZX, X, Y, Q)
venjunctor(0, 1, 1, 1).
venjunctor(_, _, 0, 0).
venjunctor(_, 0, 1, 0).
venjunctor(1, 1, 1, 1).
% venjunctor/3
% venjunctor(X, Y, Q)
venjunctor(X, Y, Q) :-
  venjunctor(_ZX, X, Y, Q).


