:- module(
  rdf_graph,
  [
    file_rdf_graph/3,     % +Alias, +File, -G
    rdf_graph_age/2,      % ?G, -Age:between(0.0,inf)
    rdf_graph_is_fresh/1, % +G
    rdf_is_graph/1,       % +G
    rdf_new_graph/1,      % -G
    rdf_new_graph/2,      % +Name, -G
    rdf_stale_graph/2,    % ?G, +FLT:between(0.0,inf)
    rdf_tmp_graph/1,      % -G
    rdf_unload_empty_graphs/0
  ]
).

/** <module> RDF graph

@author Wouter Beek
@compat RDF 1.1 Semantics
@see http://www.w3.org/TR/2014/REC-rdf11-mt-20140225/
@version 2015/08-2017/01
*/

:- use_module(library(atom_ext)).
:- use_module(library(os/file_ext)).
:- use_module(library(rdf/rdf_alias), []).
:- use_module(library(semweb/rdf11)).
:- use_module(library(uri)).

:- rdf_meta
   rdf_graph_age(r, -),
   rdf_graph_is_fresh(r),
   rdf_is_graph(r),
   rdf_new_graph(r, -),
   rdf_stale_graph(r, ?),
   rdf_tmp_graph(r),
   rdf_unload_graph(r).





%! file_rdf_graph(+Alias, +File, -G) is det.

file_rdf_graph(Alias, File, G) :-
  file_base_name(File, Name),
  file_name_extension(Base, _, Name),
  rdf_global_id(Alias:Base, G).



%! rdf_graph_age(+G, -Age:between(0.0,inf)) is det.
%! rdf_graph_age(-G, -Age:between(0.0,inf)) is nondet.
%
% Succeeds if Age is the age (in seconds) of a currently loaded
% Graph.

rdf_graph_age(G, Age) :-
  rdf_graph_property(G, source_last_modified(LastMod)),
  get_time(Now),
  Age is Now - LastMod.



%! rdf_graph_is_fresh(+G) is semidet.

rdf_graph_is_fresh(G) :-
  rdf_graph_property(G, source_last_modified(Time1)),
  rdf_graph_property(G, source(Iri)),
  uri_file_name(Iri, File),
  time_file(File, Time2),
  Time1 >= Time2.



%! rdf_is_graph(+G) is semidet.
%
% `rdf_graph(+)` throws an exception if G is not an atom.
%
% This predicate fails silently instead.

rdf_is_graph(G) :-
  atom(G),
  rdf_graph(G).



%! rdf_new_graph(-G) is det.
%! rdf_new_graph(+Base:rdf_graph, -G) is det.

rdf_new_graph(G) :-
  rdf_new_graph(ex:unnamed, G).


rdf_new_graph(G1, G) :-
  rdf_global_id(Prefix:Local1, G1), !,
  (   rdf_new_graph_try(G1)
  ->  G = G1
  ;   new_atom(Local1, Local2),
      rdf_global_id(Prefix:Local2, G2),
      rdf_new_graph(G2, G)
  ).
rdf_new_graph(G1, G) :-
  uri_components(G1, uri_components(Scheme,Auth,Path1,_,_)),
  % Remove the query and fragment parts, if any.
  uri_components(G2, uri_components(Scheme,Auth,Path1,_,_)),
  (   rdf_new_graph_try(G2)
  ->  G = G2
  ;   new_atom(Path1, Path2),
      uri_components(G2, uri_components(Scheme,Auth,Path2,_,_)),
      rdf_new_graph(G2, G)
  ).
rdf_new_graph(G1, G) :-
  rdf_global_id(ex:G1, G2),
  rdf_new_graph(G2, G).

rdf_new_graph_try(G) :-
  with_mutex(rdf_graph, (
    \+ rdf_graph(G),
    rdf_create_graph(G)
  )).



%! rdf_stale_graph(+G, +FreshnessLifetime:between(0.0,inf)) is semidet.
%! rdf_stale_graph(-G, +FreshnessLifetime:between(0.0,inf)) is nondet.
%
% Succeeds for currently loaded graphs whose age is over the given
% FreshnessLifetime.

rdf_stale_graph(G, FLT) :-
  rdf_graph_age(G, Age),
  is_stale_age(Age, FLT).



%! rdf_tmp_graph(-G) is det.

rdf_tmp_graph(G) :-
  rdf_new_graph(ex:tmp, G).



%! rdf_unload_empty_graphs is det.

rdf_unload_empty_graphs :-
  forall(
    rdf_number_of_triples(G, 0),
    rdf_unload_graph(G)
  ).
