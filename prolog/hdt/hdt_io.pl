:- module(hdt_io, []).

/** <module> HDT plugin for Quine I/O

@author Wouter Beek
@version 2016/08, 2016/11-2016/12
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug_ext)).
:- use_module(library(hdt/hdt_ext)).
:- use_module(library(os/file_ext)).
:- use_module(library(q/q_fs)).
:- use_module(library(q/q_io)).


:- dynamic
    hdt_graph0/2. % ?G, ?Hdt


:- multifile
    q_io:q_cache_format_hook/2,
    q_io:q_cache_rm_hook/2,
    q_io:q_cache2view_hook/2,
    q_io:q_store2cache_hook/4,
    q_io:q_view_graph_hook/3,
    q_io:q_view_rm_hook/2.


q_io:q_cache_format_hook(hdt, [hdt]).


q_io:q_cache_rm_hook(hdt, G) :-
  q_file_graph(HdtFile, hdt, G),
  atomic_list_concat([HdtFile,index], ., IndexFile),
  maplist(q_delete_file, [HdtFile,IndexFile]).


q_io:q_cache2view_hook(hdt, G) :-
  q_file_graph(HdtFile, hdt, G),
  indent_debug(q_io(cache2view(hdt)), "HDT → MEM"),
  hdt:hdt_open(Hdt, HdtFile),
  assert(hdt_graph0(G, Hdt)).


q_io:q_store2cache_hook(hdt, File1, File2, G) :-
  hdt_remove_index(File2),
  create_file_directory(File2),
  dcg_with_output_to(string(Msg), deb_q_io("N-Triples", G, "HDT")),
  indent_debug_call(
    q_io(store2cache(hdt)),
    Msg,
    hdt:hdt_create_from_file(File2, File1, [])
  ).


q_io:q_view_graph_hook(hdt, G, false) :-
  hdt_graph0(G, _).


q_io:q_view_rm_hook(hdt, G) :-
  hdt_graph0(G, Hdt),
  hdt:hdt_close(Hdt),
  retract(hdt_graph0(G,Hdt)),
  indent_debug(q(view_rm(hdt)), "MEM → HDT").
