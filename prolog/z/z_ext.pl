:- module(
  z_ext,
  [
    z_aggregate_all/3, % +Template, :Goal, -Result
    z_graph/2,         % ?M, ?G
    z_graph_to_file/3, % +G, +Comps, -File
    z_load_or_call/3,  % +M, :Goal_1, +G
    z_save/1           % +G
  ]
).

/** <module> Z extensions

Generic support for the Z abstraction layer.

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(debug)).
:- use_module(library(hdt/hdt_ext)).
:- use_module(library(rdf/rdfio)).
:- use_module(library(semweb/rdf11)).

:- meta_predicate
    z_aggregate_all(+, 0, -),
    z_load_or_call(+, 1, +).

:- rdf_meta
   z_aggregate_all(+, t, -),
   z_graph(?, r),
   z_load_or_call(+, :, r),
   z_save(r).

:- debug(z(ext)).





%! z_aggregate_all(+Template, :Goal, -Result) is det.

z_aggregate_all(Template, Goal, Result) :-
  aggregate_all(Template, Goal, Result).



%! z_graph(?M, ?G) is nondet.

z_graph(rdf, G) :-
  rdf_graph(G).
z_graph(hdt, G) :-
  hdt_graph(G).



%! z_graph_to_file(+G, +Comps, -HdtFile) is det.

z_graph_to_file(G, Comps, HdtFile) :-
  z_graph_to_base(G, Base),
  atomic_list_concat([Base|Comps], ., Local),
  absolute_file_name(data(Local), HdtFile, [access(write)]).



%! z_load_or_call(+M, :Goal_1, +G) is det.

z_load_or_call(M, Goal_1, G) :-
  (   z_graph_to_file(G, [nt,gz], File),
      exists_file(File)
  ->  (   M == rdf
      ->  rdf_load_file(File),
          debug(z(ext), "N-Triples → memory", [])
      ;   M == hdt
      ->  hdt_load(G)
      )
  ;   call(Goal_1, G),
      debug(z(ext), "Callled goal, hopefully written to N-Triples now...", []),
      z_load_or_call(M, Goal_1, G)
  ).



%! z_save(+G) is det.

z_save(G) :-
  z_graph_to_file(G, [nt,gz], File),
  rdf_write_to_sink(File, G, [compression(gzip),rdf_format(ntriples)]).





% HELPERS %

%! z_graph_to_base(+G, -Base) is det.

z_graph_to_base(G, Base) :-
  rdf_global_id(Alias:Local, G), !,
  atomic_list_concat([Alias,Local], '_', Base).
z_graph_to_base(G, Base) :-
  Base = G.
