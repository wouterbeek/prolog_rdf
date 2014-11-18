% The load file for the plRdf library.

:- dynamic(user:project/2).
:- multifile(user:project/2).
   user:project(plRdf, 'Web-based Qualitative Reasoning engine.').

:- use_module(load_project).
:- load_project([
     plc-'Prolog-Library-Collection',
     plDcg,
     plGraph,
     plHttp,
     plLangTag,
     plSet,
     plTms,
     plTree,
     plUri,
     plXml,
     plXsd
   ]).

