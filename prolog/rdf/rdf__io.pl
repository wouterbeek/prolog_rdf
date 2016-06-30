:- module(
  rdf__io,
  [
   %rdf__assert/4, % +S, +P, +O, +G
   %rdf__graph/1,  % ?G
    rdf__load/1,   % +G
    rdf__save/1,   % +G
    rdf__unload/1, % +G
    rdf2hdt/1      % +G
  ]
).
:- reexport(library(semweb/rdf11), [
     rdf_assert/4 as rdf__assert,
     rdf_graph/1 as rdf__graph
   ]).

/** <module> RDF I/O simplified API

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(hdt/hdt__io)).
:- use_module(library(q/q__io)).
:- use_module(library(semweb/rdf_turtle_write)).
:- use_module(library(semweb/rdf_zlib_plugin)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(zlib)).

:- rdf_meta
   rdf__assert(r, r, o, r),
   rdf__delete(r),
   rdf__graph(r),
   rdf__load(r),
   rdf__save(r),
   rdf__unload(r).





%! rdf__load(+G) is semidet.
%
% Load graph G into memory.

rdf__load(G) :-
  q_exists(G),
  q_graph_to_file(G, [nt,gz], File),
  rdf11:rdf_load(File, [format(ntriples),graph(G)]).



%! rdf__save(+G) is semidet.
%
% Save the contents of graph G as loaded in memory to the data
% store.

rdf__save(G) :-
  rdf__graph(G),
  q_graph_to_file(G, [nt,gz], File),
  setup_call_cleanup(
    gzopen(File, write, Out),
    rdf_turtle_write:rdf_save_ntriples(stream(Out), [graph(G)]),
    close(Out)
  ).



%! rdf__unload(+G) is semidet.
%
% Removes graph G from memory.

rdf__unload(G) :-
  rdf_unload_graph(G).



%! rdf2hdt(+G) is semidet.

rdf2hdt(G) :-
  rdf__save(rdf, G),
  hdt__load(G).
