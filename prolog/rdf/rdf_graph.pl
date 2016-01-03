:- module(
  rdf_graph,
  [
    rdf_create_graph/1,		% ?G
    rdf_cp_graph/2,		% +FromG, +ToG
    rdf_expect_graph/1,		% @Term
    rdf_fresh_graph/2,		% ?G, +FreshnessLifetime:between(0.0,inf)
    rdf_graph/1,		% ?G
    rdf_graph_age/2,		% ?G, -Age:between(0.0,inf)
    rdf_graph_get_property/2,	% ?G, ?Property:compound
    rdf_graph_set_property/2,	% +G, +Property:compound
    rdf_is_graph/1,		% @Term
    rdf_mv_graph/2,		% +FromG, +ToG
    rdf_new_graph/1,		% -G
    rdf_new_graph/2,		% +Name, -G
    rdf_stale_graph/2,		% ?G, +FreshnessLifetime:between(0.0,inf)
    rdf_tmp_graph/1,		% -G
    rdf_unload_graph/1,		% +G
    rdf_unload_graphs/0
  ]
).

/** <module> RDF graph

@author Wouter Beek
@compat RDF 1.1 Semantics
@see http://www.w3.org/TR/2014/REC-rdf11-mt-20140225/
@version 2015/08, 2015/10, 2015/12-2016/01
*/

:- use_module(library(atom_ext)).
:- use_module(library(os/file_ext)).
:- use_module(library(rdf/id_store)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_update)).
:- use_module(library(semweb/rdf_db), [
     rdf_create_graph/1 as rdf_create_graph_id,
     rdf_graph/1 as rdf_graph_id,
     rdf_graph_property/2 as rdf_graph_id_get_property,
     rdf_set_graph/2 as rdf_graph_id_set_property,
     rdf_unload_graph/1 as rdf_unload_graph_id
   ]).
:- use_module(library(uri)).

:- rdf_meta(rdf_create_graph(r)).
:- rdf_meta(rdf_cp_graph(r,r)).
:- rdf_meta(rdf_expect_graph(r)).
:- rdf_meta(rdf_fresh_graph(r,+)).
:- rdf_meta(rdf_graph(r)).
:- rdf_meta(rdf_graph_age(r,-)).
:- rdf_meta(rdf_graph_get_property(r,?)).
:- rdf_meta(rdf_graph_set_property(r,?)).
:- rdf_meta(rdf_mv_graph(r,r)).
:- rdf_meta(rdf_new_graph(r,-)).
:- rdf_meta(rdf_stale_graph(r,?)).
:- rdf_meta(rdf_tmp_graph(r)).
:- rdf_meta(rdf_unload_graph(r)).





%! rdf_create_graph(+G) is det.
% Extends Semweb's rdf_create_graph/1 by applying RDF prefix expansion.

rdf_create_graph(G):-
  assign_graph_id(G, Gid),
  rdf_create_graph_id(Gid).



%! rdf_cp_graph(+FromG, +ToG) is det.

rdf_cp_graph(FromG, FromG):- !.
rdf_cp_graph(FromG, ToG):-
  rdf_cp(FromG, _, _, _, ToG).



%! rdf_expect_graph(@Term) is nondet.
% If Term is uninstantiated it is non-deterministically
% instantiated to existing RDF graphs.
% If Term is instantiated and does not denote an existing RDF graph
% this results in an exception.
%
% @throws existence_error

rdf_expect_graph(G):-
  var(G), !,
  % NONDET.
  rdf_graph(G).
rdf_expect_graph(G):-
  rdf_is_graph(G), !.
rdf_expect_graph(G):-
  existence_error(rdf_graph, G).



%! rdf_fresh_graph(+G, +FreshnessLifetime:between(0.0,inf)) is semidet.
%! rdf_fresh_graph(-G, +FreshnessLifetime:between(0.0,inf)) is nondet.
% Succeeds for currently loaded RDF graphs whose age is below
% the given FreshnessLifetime.

rdf_fresh_graph(G, FLT):-
  rdf_graph_age(G, Age),
  is_fresh_age(Age, FLT).



%! rdf_graph(+G) is semidet.
%! rdf_graph(-G) is multi.

rdf_graph(G):-
  var(G), !,
  rdf_graph_id(Gid),
  assign_graph_id(G, Gid).
rdf_graph(G):-
  assign_graph_id(G, Gid),
  rdf_graph_id(Gid).



%! rdf_graph_age(+G, -Age:between(0.0,inf)) is det.
%! rdf_graph_age(-G, -Age:between(0.0,inf)) is nondet.
% Succeeds if Age is the age (in seconds) of a currently loaded RDF Graph.

rdf_graph_age(G, Age):-
  rdf_graph_get_property(G, source_last_modified(LastMod)),
  get_time(Now),
  Age is Now - LastMod.



%! rdf_graph_get_property(?G, ?Property:compound) is semidet.
% Extends Semweb's rdf_graph_property/2 by applying RDF prefix expansion
% on the graph.

rdf_graph_get_property(G, Property):-
  var(G), !,
  rdf_graph_id_get_property(Gid, Property),
  assign_graph_id(G, Gid).
rdf_graph_get_property(G, Property):-
  assign_graph_id(G, Gid),
  rdf_graph_id_get_property(Gid, Property).



%! rdf_graph_set_property(+G, +Property:compound) is semidet.
% Extends Semweb's rdf_set_graph/2 by applying RDF prefix expansion
% on the graph.

rdf_graph_set_property(G, Property):-
  assign_graph_id(G, Gid),
  rdf_graph_id_set_property(Gid, Property).



%! rdf_is_graph(@Term) is semidet.
% Fails silently for non-existing graphs, can handle non-atoms
% and always succeeds for the default graph.
%
% The name of this predicate is in line with Semweb's rdf_is_bnode/1,
% rdf_is_literal/1 and rdf_is_resource/1.

rdf_is_graph(G):-
  atom(G),
  (G == default, ! ; rdf_graph(G)).



%! rdf_mv_graph(+FromG, +ToG) is det.

rdf_mv_graph(FromG, ToG):-
  rdf_mv(FromG, _, _, _, ToG),
  rdf_unload_graph(FromG).



%! rdf_new_graph(-G) is det.

rdf_new_graph(G):-
  rdf_new_graph(ex:unnamed, G).


%! rdf_new_graph(+Base:rdf_graph, -G) is det.

rdf_new_graph(G1, G):-
  rdf_global_id(Prefix:Local1, G1), !,
  (   rdf_new_graph_try(G1)
  ->  G = G1
  ;   new_atom(Local1, Local2),
      rdf_global_id(Prefix:Local2, G2),
      rdf_new_graph(G2, G)
  ).
rdf_new_graph(G1, G):-
  uri_components(G1, uri_components(Scheme,Auth,Path1,_,_)),
  % Remove the query and fragment parts, if any.
  uri_components(G2, uri_components(Scheme,Auth,Path1,_,_)),
  (   rdf_new_graph_try(G2)
  ->  G = G2
  ;   new_atom(Path1, Path2),
      uri_components(G2, uri_components(Scheme,Auth,Path2,_,_)),
      rdf_new_graph(G2, G)
  ).
rdf_new_graph(G1, G):-
  rdf_global_id(ex:G1, G2),
  rdf_new_graph(G2, G).

rdf_new_graph_try(G):-
  with_mutex(rdf_graph, (
    \+ rdf_graph(G),
    rdf_create_graph(G)
  )).



%! rdf_stale_graph(+G, +FreshnessLifetime:between(0.0,inf)) is semidet.
%! rdf_stale_graph(-G, +FreshnessLifetime:between(0.0,inf)) is nondet.
% Succeeds for currently loaded graphs whose age is over the given
% FreshnessLifetime.

rdf_stale_graph(G, FLT):-
  rdf_graph_age(G, Age),
  is_stale_age(Age, FLT).



%! rdf_tmp_graph(-G) is det.

rdf_tmp_graph(G):-
  rdf_new_graph(ex:tmp, G).



%! rdf_unload_graph(+G) is semidet.
% Extends Semweb's rdf_unload_graph/1 by applying RDF prefix expansion.
%
% @tbd This does not unload the identity store.

rdf_unload_graph(G):-
  rdf_expect_graph(G),
  assign_graph_id(G, Gid),
  rdf_unload_graph_id(Gid).



%! rdf_unload_graphs is det.

rdf_unload_graphs:-
  rdf_retractall_id(_, _, _, _),
  unload_id_store.
