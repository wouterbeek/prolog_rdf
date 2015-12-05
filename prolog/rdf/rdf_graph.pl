:- module(
  rdf_graph,
  [
    rdf_graph/1, % ?Graph:rdf_graph
    rdf_cp_graph/2, % +From:rdf_graph
                    % +To:rdf_graph
    rdf_fresh_graph/2, % ?Graph:rdf_graph
                       % +FreshnessLifetime:between(0.0,inf)
    rdf_graph_age/2, % ?Graph:rdf_graph
                     % -Age:between(0.0,inf)
    rdf_graph_get_property/2, % ?Graph:rdf_graph
                              % ?Property:compound
    rdf_graph_set_property/2, % +Graph:rdf_graph
                              % +Property:compound
    rdf_is_graph/1, % @Term
    rdf_mv_graph/2, % +From:rdf_graph
                    % +To:rdf_graph
    rdf_new_graph/1, % -Graph:rdf_graph
    rdf_new_graph/2, % +Name:rdf_graph
                     % -Graph:rdf_graph
    rdf_stale_graph/2, % ?Graph:rdf_graph
                       % +FreshnessLifetime:between(0.0,inf)
    rdf_tmp_graph/1, % -Graph:rdf_graph
    rdf_unload_graph/1 % +Graph:rdf_graph
  ]
).

/** <module> RDF graph

@author Wouter Beek
@compat RDF 1.1 Semantics
@license MIT
@see http://www.w3.org/TR/2014/REC-rdf11-mt-20140225/
@version 2015/08, 2015/10, 2015/12
*/

:- use_module(library(atom_ext)).
:- use_module(library(os/file_ext)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_update)).
:- use_module(library(semweb/rdf_db), [
     rdf_create_graph/1 as rdf_create_graph0,
     rdf_graph/1 as rdf_graph0,
     rdf_graph_property/2 as rdf_graph_property0,
     rdf_set_graph/2,
     rdf_unload_graph/1 as rdf_unload_graph0
   ]).
:- use_module(library(uri)).

:- rdf_meta(rdf_create_graph(r)).
:- rdf_meta(rdf_cp_graph(r,r)).
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





%! rdf_create_graph(+Graph:rdf_graph) is semidet.
% Extends Semweb's rdf_create_graph/1 by applying RDF prefix expansion.

rdf_create_graph(G):-
  rdf_create_graph0(G).



%! rdf_cp_graph(+From:rdf_graph, +To:rdf_graph) is det.

rdf_cp_graph(From, From):- !.
rdf_cp_graph(From, To):-
  rdf_cp(From, _, _, _, To).



%! rdf_fresh_graph(
%!   +Graph:rdf_graph,
%!   +FreshnessLifetime:between(0.0,inf)
%! ) is semidet.
%! rdf_fresh_graph(
%!   -Graph:rdf_graph,
%!   +FreshnessLifetime:between(0.0,inf)
%! ) is nondet.
% Succeeds for currently loaded RDF graphs whose age is below
% the given FreshnessLifetime.

rdf_fresh_graph(G, FLT):-
  rdf_graph_age(G, Age),
  is_fresh_age(Age, FLT).



%! rdf_graph(+Graph:rdf_graph) is semidet.
%! rdf_graph(-Graph:rdf_graph) is multi.
% Extends Semweb's rdf_graph/1 by applying RDF prefix expansion.

rdf_graph(G):-
  rdf_graph0(G).



%! rdf_graph_age(+Graph:rdf_graph, -Age:between(0.0,inf)) is det.
%! rdf_graph_age(-Graph:rdf_graph, -Age:between(0.0,inf)) is nondet.
% Succeeds if Age is the age (in seconds) of a currently loaded RDF Graph.

rdf_graph_age(G, Age):-
  rdf_graph_get_property(G, source_last_modified(LastMod)),
  get_time(Now),
  Age is Now - LastMod.



%! rdf_graph_get_property(?Graph:rdf_graph, ?Property:compound) is semidet.
% Extends Semweb's rdf_graph_property/2 by applying RDF prefix expansion
% on the graph.

rdf_graph_get_property(G, Property):-
  rdf_graph_property0(G, Property).



%! rdf_graph_set_property(+Graph:rdf_graph, +Property:compound) is semidet.
% Extends Semweb's rdf_set_graph/2 by applying RDF prefix expansion
% on the graph.

rdf_graph_set_property(G, Property):-
  rdf_set_graph(G, Property).



%! rdf_is_graph(@Term) is semidet.
% rdf_graph/1 throws an exception for any non-atomic nonvar argument,
% whereas this predicate fails silently.
%
% rdf_graph/1 does not succeed for the default graph (called `user`)
% if it is empty whereas this predicate does.
%
% The name of this predicate is in line with Semweb's rdf_is_bnode/1,
% rdf_is_literal/1 and rdf_is_resource/1.

rdf_is_graph(G):-
  atom(G),
  (G == default, ! ; rdf_graph(G)).



%! rdf_mv_graph(+From:rdf_graph, +To:rdf_graph) is det.

rdf_mv_graph(From, To):-
  rdf_mv(From, _, _, _, To),
  rdf_unload_graph(From).



%! rdf_new_graph(-Graph:rdf_graph) is det.

rdf_new_graph(G):-
  rdf_new_graph(ex:unnamed, G).


%! rdf_new_graph(+Base:rdf_graph, -Graph:rdf_graph) is det.

rdf_new_graph(G1, G):-
  rdf_expand_rt(Prefix:Local1, G1), !,
  (   rdf_new_graph_try(G1)
  ->  G = G1
  ;   new_atom(Local1, Local2),
      rdf_expand_rt(Prefix:Local2, G2),
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
  rdf_expand_rt(ex:G1, G2),
  rdf_new_graph(G2, G).

rdf_new_graph_try(G):-
  with_mutex(rdf_graph, (
    \+ rdf_graph(G),
    rdf_create_graph(G)
  )).



%! rdf_stale_graph(
%!   +Graph:rdf_graph,
%!   +FreshnessLifetime:between(0.0,inf)
%! ) is semidet.
%! rdf_stale_graph(
%!   -Graph:rdf_graph,
%!   +FreshnessLifetime:between(0.0,inf)
%! ) is nondet.
% Succeeds for currently loaded graphs whose age is over the given
% FreshnessLifetime.

rdf_stale_graph(G, FLT):-
  rdf_graph_age(G, Age),
  is_stale_age(Age, FLT).



%! rdf_tmp_graph(-Graph:rdf_graph) is det.

rdf_tmp_graph(G):-
  rdf_new_graph(ex:tmp, G).



%! rdf_unload_graph(+Graph:rdf_graph) is semidet.
% Extends Semweb's rdf_unload_graph/1 by applying RDF prefix expansion.

rdf_unload_graph(G):-
  rdf_unload_graph0(G).
