/*
    tvl.pl

        Ternary Vector List representation of booleans
         utilities and operators

	    See the XBOOLE tool, http://www.informatik.tu-freiberg.de/xboole/

    	A tvl is a list of type term compounds as anonymous dicts.
	
	    Represents a conjuction/disjuction of boolean terms,
	    is a boolean equation.  

        In this library a tvl represents a SOP, Sum Of Products,
        if no else stated.

        Don't cares as unbounded vars, true as 1 and false as 0


   (c) 2020, xae. Juan José Eraso Escalona
   
   20200615
*/


:- module( 'tvl',
           [ union/3            % union(+As, +Bs, -Unions) 
           , intersection/3     % intersection(+As, +Bs, -Intersections)
           , complement/2       % complement(+As, -Complements)
           , difference/3       % difference(+A, +B, -Difference)
           , orthogonal/2       % orthogonal(+TVL, -Ortogonal)
           , quotient/3         % quotient(+DEND, +DSOR, -QUOT) 
           , absorb/2           % absorb(+TVL, -Result)
           % bcf
           , bcf/2              % bcf(+TVL, -Result)
           , nearly_minimal/2   % nearly_minimal(+TVL, -Result)
           % mark
           , on_set/3           % on_set(+Index, +TVL, -Result)
           , off_set/3          % off_set(+Index, +TVL, -Result)
           , on_mark/3          % on_mark(+Index, +TVL, -Result)
           , off_mark/3         % off_mark(+Index, +TVL, -Result)
           , interval/2         % interval(ONSet, OFFSet)
           , minimal_subsets/3  % minimal_subsets(+ONSet, +OFFSet, -Sets)
           % tvl_casting
           , to_fact/3          % to_fact(+Keys, +TVL, :Functor)
           , from_fact/3        % from_fact(+Keys, :Functor, -TVL)  
           % karnaugh
           , karnaugh_map/2     % karnaugh_map(+ONSet, +OFFSet)
           , karnaugh_map/3     % karnaugh_map(+Keys, +ONSet, +OFFSet)
           ] ).


:- use_module(library(yall)).

:- ensure_loaded(library(tvl_ops)).
:- ensure_loaded(library(tvl_dict)).
:- ensure_loaded(library(tvl_predsort)).
:- ensure_loaded(library(tvl_portray)).
:- ensure_loaded(library(tvl_casting)).

:- ensure_loaded(library(mark)).
:- ensure_loaded(library(bcf)).
:- ensure_loaded(library(karnaugh)).


