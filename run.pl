% Standalone execution of the plRdf library.

:- if(current_prolog_flag(argv, ['--debug'|_])).
  :- ensure_loaded(debug).
:- else.
  :- set_prolog_flag(verbose, silent).
  :- ensure_loaded(load).
:- endif.

