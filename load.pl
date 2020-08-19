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

user:file_search_path(examples, 'examples').


:- ensure_loaded(library(until)).

:- ensure_loaded(ops).

% Additional operators and terms
:- ensure_loaded(extra).

% Calling prolog terms support
:- ensure_loaded(cal).

