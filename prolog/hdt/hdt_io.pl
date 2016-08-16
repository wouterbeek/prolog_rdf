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
    q_io:q_cache_format_hook/2,
    q_io:q_cache2view_hook/2,
    q_io:q_store2cache_hook/2,
    q_io:q_view_graph_hook/2,
    q_io:q_view_rm_hook/2.


q_io:q_cache_format_hook(hdt, [hdt]).


q_io:q_cache2view_hook(hdt, G) :-
  q_file_graph(HdtFile, hdt, G),
  indent_debug(q_io(cache2view(hdt)), "HDT → MEM"),
  hdt:hdt_open(Hdt, HdtFile),
  assert(hdt_graph0(G, Hdt)).


q_io:q_store2cache_hook(hdt, G) :-
  q_file_graph(File1, ntriples, G),
  q_file_graph(File2, hdt, G),
  (   q_file_is_ready(File1, File2)
  ->  true
  ;   create_file_directory(File2),
      indent_debug_call(
        q_io(store2cache(hdt)),
        "N-Triples → HDT",
        hdt:hdt_create_from_file(File2, File1, [])
      ),
      q_file_touch_ready(File2)
  ).


q_io:q_view_graph_hook(hdt, G, false) :-
  hdt_graph0(G, _).


q_io:q_view_rm_hook(hdt, G) :-
  hdt_graph0(G, Hdt),
  hdt:hdt_close(Hdt),
  retract(hdt_graph0(G,Hdt)),
  indent_debug(q(view_rm(hdt)), "MEM → HDT").
