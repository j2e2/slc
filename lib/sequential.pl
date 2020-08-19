/*
    sequential.pl
      Sequential operators and devices

   (c) 2020, xae. Juan José Eraso Escalona
   
   20200704

*/

:- module( 'sequential',
           [ sr/4                          % sr(S, R, ZQ, Q)
           , sr/3                          % sr(S, R, Q)
           , sr/1                          % sr(_{ s, r, q })
           , sr1/4                         % sr1(S, R, ZQ, Q)
           , sr1/3                         % sr1(S, R, Q)
           , sr1/1                         % sr1(_{ s, r, zq, q })
           , s1r/4                         % s1r(R, S, ZQ, Q)
           , s1r/3                         % s1r(S, R, Q)
           , s1r/1                         % s1r(_{ r, s, zq, q })
           , jk/4                          % jk(J, K, ZQ, Q)
           , jk/3                          % jk(J, K, Q)
           , jk/1                          % jk(_{j, k, zq, q })
           , dv/4                          % dv(D, V, ZQ, Q)
           , dv/3                          % dv(D, V, Q)
           , dv/1                          % dv(_{ d, v, zq, q })
           , t/3                           % t(T, ZQ, Q)
           , t/1                           % t(_{ t, zq, q })
           , venjunctor/4                  % venjunctor(ZX, X, Y, Q)
           , venjunctor/3                  % venjunctor(X, Y, Q)
           , venjunctor/1                  % venjunctor(_{ zx, x, y, q })
           ] ).


% SR latch

% sr/4
% sr(S, R, ZQ, Q)
  sr(1, 0, _, 1).
  sr(0, 1, _, 0).
  sr(0, 0, 0, 0).
  sr(0, 0, 1, 1).
% sr/3
% sr(S, R, Q)
sr(S, R, Q) :-
  sr(S, R, _ZQ, Q).
% sr/1
% sr(_{ s:S, r:R, zq:ZQ q:Q })
sr(Dict) :- 
    Dict :< _{ s:S, r:R, zq:ZQ, q:Q },
    sr(S, R, ZQ, Q).

%  Priority on reset

% sr1/4
% sr1(S, R, ZQ, Q)
  sr1(_, 1, _, 0).
  sr1(1, 0, _, 1).
  sr1(0, 0, 0, 0).
  sr1(0, 0, 1, 1).
% sr1/3
% sr1(S, R, Q)
sr1(S, R, Q) :-
  sr1(S, R, _ZQ, Q).
% sr1/1
% sr1(_{ s:S, r:R, zq:ZQ, q:Q })
sr1(Dict) :-
    Dict :< _{ s:S, r:R, zq:ZQ, q:Q },
    sr1(S, R, ZQ, Q).

%  Priority on set

% s1r/4
% s1r(R, S, ZQ, Q)
  s1r(1, _, _, 1).
  s1r(0, 1, _, 0).
  s1r(0, 0, 0, 0).
  s1r(0, 0, 1, 1).
% s1r/3
% s1r(S, R, Q)
s1r(R, S, Q) :-
  s1r(R, S, _ZQ, Q).
% s1r/1
% s1r(_{ r:R, s:S, zq:ZQ, q:Q })
s1r(Dict) :-
    Dict :< _{ r:R, s:S, zq:ZQ, q:Q },
    s1r(R, S, ZQ, Q).


% DV latch

% dv/4
% dv(D, V, ZQ, Q)
  dv(_, 0, 0, 0).
  dv(_, 0, 1, 1).
  dv(0, 1, _, 0).
  dv(1, 1, _, 1).
% dv/3
% dv(D, V, Q)
dv(D, V, Q) :-
  dv(D, V, _ZQ, Q).
% dv/1
% dv(_{ d:D, v:V, zq:ZQ, q:Q })
dv(Dict) :-
    Dict :< _{ d:D, v:V, zq:ZQ, q:Q }, 
    dv(D, V, ZQ, Q).

 
% JK latch

% jk/4
% jk(J, K, ZQ, Q)
  jk(0, 0, 0, 0).
  jk(0, 0, 1, 1).
  jk(0, 1, _, 0).
  jk(1, 0, _, 1).
  jk(1, 1, 0, 1).
  jk(1, 1, 1, 0).
% jk/3
% jk(J, K, Q)
jk(J, K, Q) :-
  jk(J, K, _ZQ, Q).
% jk/1
% jk(_{j:J, k:K, zq:ZQ, q:Q })
jk(Dict) :-
    Dict :< _{j:J, k:K, zq:ZQ, q:Q },
    jk(J, K, ZQ, Q).


% T latch

% t/3
% t(T, ZQ, Q)
  t(0, 0, 0).
  t(0, 1, 1).
  t(1, 0, 1).
  t(1, 1, 0).
% t/1
% t(_{ t:T, zq:ZQ, q:Q })
t(Dict) :-
    Dict :< _{ t:T, zq:ZQ, q:Q },
    t(T, ZQ, Q).  

 
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
% venjunctor/1
% venjunctor(_{ zx:ZX, x:X, y:Y, q:Q })
venjunctor(Dict) :-
    Dict :< _{ zx:ZX, x:X, y:Y, q:Q },
    venjunctor(ZX, X, Y, Q).


 


