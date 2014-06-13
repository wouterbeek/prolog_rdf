% The load file for the plRdf project.

:- multifile(user:project/2).
   user:project(plRdf, 'Web-based Qualitative Reasoning engine.').

:- use_module(load_project).
:- load_project([
     plc-'Prolog-Library-Collection',
   ]).

