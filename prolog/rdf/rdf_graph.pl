:- module(
  rdf_graph,
  [
    rdf_copy_graph/2, % +From:atom
                      % +To:atom
    rdf_fresh_graph/2, % ?Graph:atom
                          % +FreshnessLifetime:between(0.0,inf)
    rdf_graph_age/2, % ?Graph:atom
                     % -Age:between(0.0,inf)
    rdf_is_graph/1, % @Term
    rdf_stale_graph/2 % ?Graph:atom
                      % +FreshnessLifetime:between(0.0,inf)
  ]
).

/** <module> RDF graph

@author Wouter Beek
@compat [RDF 1.1 Semantics](http://www.w3.org/TR/2014/REC-rdf11-mt-20140225/)
@license MIT
@version 2015/08, 2015/10
*/

:- use_module(library(os/file_ext)).
:- use_module(library(semweb/rdf_db)).





%! rdf_copy_graph(+From:atom, +To:atom) is det.

rdf_copy_graph(From, To):-
  forall(rdf(S, P, O, From), rdf_assert(S, P, O, To)).



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
