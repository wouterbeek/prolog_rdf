:- module(
  rdf_graph,
  [
    rdf_cp_graph/2, % +From:atom
                    % +To:atom
    rdf_fresh_graph/2, % ?Graph:atom
                          % +FreshnessLifetime:between(0.0,inf)
    rdf_graph_age/2, % ?Graph:atom
                     % -Age:between(0.0,inf)
    rdf_is_graph/1, % @Term
    rdf_mv_graph/2, % +From:atom
                    % +To:atom
    rdf_new_graph/1, % -Graph:atom
    rdf_new_graph/2, % +Name:atom
                     % -Graph:atom
    rdf_stale_graph/2, % ?Graph:atom
                       % +FreshnessLifetime:between(0.0,inf)
    rdf_tmp_graph/1 % -Graph:atom
  ]
).

/** <module> RDF graph

@author Wouter Beek
@compat [RDF 1.1 Semantics](http://www.w3.org/TR/2014/REC-rdf11-mt-20140225/)
@license MIT
@version 2015/08, 2015/10
*/

:- use_module(library(atom_ext)).
:- use_module(library(os/file_ext)).
:- use_module(library(rdf/rdf_update)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(uri)).





%! rdf_cp_graph(+From:atom, +To:atom) is det.

rdf_cp_graph(From, From):- !.
rdf_cp_graph(From, To):-
  rdf_cp(From, _, _, _, To).



%! rdf_fresh_graph(
%!   +Graph:atom,
%!   +FreshnessLifetime:between(0.0,inf)
%! ) is semidet.
%! rdf_fresh_graph(
%!   -Graph:atom,
%!   +FreshnessLifetime:between(0.0,inf)
%! ) is nondet.
% Succeeds for currently loaded RDF graphs whose age is below
% the given FreshnessLifetime.

rdf_fresh_graph(G, FLT):-
  rdf_graph_age(G, Age),
  is_fresh_age(Age, FLT).



%! rdf_graph_age(+Graph:atom, -Age:between(0.0,inf)) is det.
%! rdf_graph_age(-Graph:atom, -Age:between(0.0,inf)) is nondet.
% Succeeds if Age is the age (in seconds) of a currently loaded RDF Graph.

rdf_graph_age(G, Age):-
  rdf_graph_property(G, source_last_modified(LastMod)),
  get_time(Now),
  Age is Now - LastMod.



%! rdf_is_graph(@Term) is semidet.
% rdf_graph/1 throws an exception for any non-atomic nonvar argument,
% whereas this predicate fails silently.
%
% rdf_graph/1 does not succeed for the default graph (called `user`)
% if it is empty whereas this predicate does.
%
% The name of this predicate is in line with rdf_is_bnode/1, rdf_is_literal/1,
% and rdf_is_resource/1 in [library(semweb/rdf_db)].

rdf_is_graph(G):-
  atom(G),
  (G == user ; rdf_graph(G)), !.



%! rdf_mv_graph(+From:atom, +To:atom) is det.

rdf_mv_graph(From, To):-
  rdf_mv(From, _, _, _, To),
  rdf_unload_graph(From).



%! rdf_new_graph(-Graph:atom) is det.

rdf_new_graph(G):-
  rdf_new_graph(noname, G).


%! rdf_new_graph(+Base:atom, -Graph:atom) is det.

rdf_new_graph(Base, G):-
  atomic_concat(/, Base, Path),
  uri_components(GPrefix, uri_components(http,'example.com',Path,_,_)),
  with_mutex(rdf_graph, (
    rdf_new_graph_iri(GPrefix, G),
    rdf_create_graph(G)
  )).


% The graph name is new.
rdf_new_graph_iri(G, G):-
  \+ rdf_graph(G), !.
% An RDF graph with the same name already exists,
% so come up with another name.
rdf_new_graph_iri(GPrefix, G):-
  new_atom(GPrefix, GTmp),
  rdf_new_graph_iri(GTmp, G).



%! rdf_stale_graph(
%!   +Graph:atom,
%!   +FreshnessLifetime:between(0.0,inf)
%! ) is semidet.
%! rdf_stale_graph(
%!   -Graph:atom,
%!   +FreshnessLifetime:between(0.0,inf)
%! ) is nondet.
% Succeeds for currently loaded graphs whose age is over the given
% FreshnessLifetime.

rdf_stale_graph(G, FLT):-
  rdf_graph_age(G, Age),
  is_stale_age(Age, FLT).



%! rdf_tmp_graph(-Graph:atom) is det.

rdf_tmp_graph(G):-
  rdf_new_graph(tmp, G).
