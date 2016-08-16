:- module(hdt_io, []).

/** <module> HDT plugin for Quine I/O

@author Wouter Beek
@version 2016/08
*/

:- use_module(library(debug_ext)).
:- use_module(library(hdt), []).
:- use_module(library(os/file_ext)).
:- use_module(library(q/q_io)).


:- dynamic
    hdt_graph0/2. % ?G, ?Hdt


:- multifile
    q_io:q_cache_extensions/2,
    q_io:q_cache2view_hook/2,
    q_io:q_store2cache_hook/2,
    q_io:q_view_graph_hook/2,
    q_io:q_view_rm_hook/2.


q_io:q_cache_extensions(hdt, [hdt]).


q_io:q_cache2view_hook(hdt, G) :-
  q_file_graph(HdtFile, hdt, G),
  indent_debug(in, q(q_io), "HDT → MEM", []),
  hdt:hdt_open(Hdt, HdtFile),
  assert(hdt_graph0(G, Hdt)).


q_io:q_store2cache_hook(hdt, G) :-
  q_file_graph(Source, ntriples, G),
  q_file_graph(Sink, hdt, G),
  create_file_directory(Sink),
  indent_debug_call(
    q(q_io),
    "N-Triples → HDT",
    hdt:hdt_create_from_file(Sink, Source, [])
  ).


q_io:q_view_graph_hook(hdt, G, false) :-
  hdt_graph0(G, _).


q_io:q_view_rm_hook(hdt, G) :-
  hdt_graph0(G, Hdt),
  hdt:hdt_close(Hdt),
  retract(hdt_graph0(G,Hdt)),
  indent_debug(out, q(q_io), "MEM → HDT", []).
