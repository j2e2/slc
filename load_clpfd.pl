/*
    load.pl

    Switching logic circuit
    set up file


    (c) 2017, xae. Juan José Eraso Escalona
    20170815
*/

:- asserta(library_directory('tvl')).
:- asserta(library_directory('lib')).
:- working_directory(CWD, CWD),
   asserta(library_directory(CWD)).

:- assertz(file_search_path(clpfd,  'clpfd')).

:- ensure_loaded(clpfd(ops)).

% Additional operators and terms
:- ensure_loaded(extra).

% Calling prolog terms support
:- ensure_loaded(cal).

