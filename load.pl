% The load file for the plRdf project.

:- dynamic(user:prolog/2).
:- multifile(user:project/2).
   user:project(plRdf, 'Web-based Qualitative Reasoning engine.').

:- use_module(load_project).
:- load_project(plRdf, [
     plc-'Prolog-Library-Collection',
     plGraph
   ]).

