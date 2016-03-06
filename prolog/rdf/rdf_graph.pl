:- module(
  rdf_graph,
  [
    rdf_expect_graph/1, % @Term
    rdf_fresh_graph/2,  % ?G, +FreshnessLifetime:between(0.0,inf)
    rdf_graph_age/2,    % ?G, -Age:between(0.0,inf)
    rdf_new_graph/1,    % -G
    rdf_new_graph/2,    % +Name, -G
    rdf_stale_graph/2,  % ?G, +FreshnessLifetime:between(0.0,inf)
    rdf_tmp_graph/1     % -G
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
:- use_module(library(rdf/rdf_api)).
:- use_module(library(uri)).

:- rdf_meta
   rdf_expect_graph(r),
   rdf_fresh_graph(r, +),
   rdf_graph_age(r, -),
   rdf_new_graph(r, -),
   rdf_stale_graph(r, ?),
   rdf_tmp_graph(r),
   rdf_unload_graph(r).





%! rdf_expect_graph(@Term) is nondet.
% If Term is uninstantiated it is non-deterministically
% instantiated to existing RDF graphs.
% If Term is instantiated and does not denote an existing RDF graph
% this results in an exception.
%
% @throws existence_error

rdf_expect_graph(G) :-
  var(G), !,
  % NONDET.
  rdf_graph(G).
rdf_expect_graph(G) :-
  rdf_graph(G), !.
rdf_expect_graph(G) :-
  existence_error(rdf_graph, G).



%! rdf_fresh_graph(+G, +FreshnessLifetime:between(0.0,inf)) is semidet.
%! rdf_fresh_graph(-G, +FreshnessLifetime:between(0.0,inf)) is nondet.
% Succeeds for currently loaded RDF graphs whose age is below
% the given FreshnessLifetime.

rdf_fresh_graph(G, FLT) :-
  rdf_graph_age(G, Age),
  is_fresh_age(Age, FLT).



%! rdf_graph_age(+G, -Age:between(0.0,inf)) is det.
%! rdf_graph_age(-G, -Age:between(0.0,inf)) is nondet.
% Succeeds if Age is the age (in seconds) of a currently loaded RDF Graph.

rdf_graph_age(G, Age) :-
  rdf_graph_property(G, source_last_modified(LastMod)),
  get_time(Now),
  Age is Now - LastMod.



%! rdf_new_graph(-G) is det.

rdf_new_graph(G) :-
  rdf_new_graph(ex:unnamed, G).


%! rdf_new_graph(+Base:rdf_graph, -G) is det.

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
% Succeeds for currently loaded graphs whose age is over the given
% FreshnessLifetime.

rdf_stale_graph(G, FLT) :-
  rdf_graph_age(G, Age),
  is_stale_age(Age, FLT).



%! rdf_tmp_graph(-G) is det.

rdf_tmp_graph(G) :-
  rdf_new_graph(ex:tmp, G).
