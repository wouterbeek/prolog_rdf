% Load file for plRdf.

:- use_module(library(ansi_term)).

:- initialization(load_plRdf).

load_plRdf:-
  % Entry point.
  source_file(load_plRdf, ThisFile),
  file_directory_name(ThisFile, ThisDir),
  assert(user:file_search_path(plRdf, ThisDir)),
  
  % Prolog Library Collection (PLC).
  load_plc(plRdf).


load_plc(_):-
  user:file_search_path(plc, _), !.
load_plc(Project):-
  Spec =.. [Project,'Prolog-Library-Collection'],
  absolute_file_name(Spec, _, [access(read),file_type(directory)]), !,
  assert(user:file_search_path(plc, Spec)),
  ensure_loaded(plc(index)).
load_plc(_):-
  print_message(warning, no_plc).


:- multifile(prolog:message//1).

prolog:message(no_plc) -->
  [
    'The Prolog-Library-Collection submodule is not present.', nl,
    'Consider running the following from within the plRdf directory:', nl,
    '    git submodule init', nl,
    '    git submodule update'
  ].

