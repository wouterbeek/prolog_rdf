:- module(
  rdf_deref,
  [
    rdf_cache/0,
    rdf_cache/1, % +Options:list(compound)
    rdf_deref/1, % +Subject
    rdf_deref/2, % +Subject, -Graph
    rdf_deref/3 % +Subject:iri
                % -Predicate:iri
                % -Object:rdf_term
  ]
).

/** <module> RDF: Synchronization

@author Wouter Beek
@version 2015/12
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(debug_ext)).
:- use_module(library(http/http_info)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(pool)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_graph)).
:- use_module(library(rdf/rdf_literal)).
:- use_module(library(rdf/rdf_load)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(rdf/rdf_read)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(uri)).

:- rdf_meta(rdf_deref(r)).
:- rdf_meta(rdf_deref(r,-)).
:- rdf_meta(rdf_deref(r,-,-)).

:- predicate_options(rdf_cache/1, 1, [
     number_of_workers(+nonneg),
     pass_to(add_worker/3, 3),
     pass_to(rdf_cache_worker/3, 1)
   ]).
:- predicate_options(rdf_cache_worker/3, 1, [
     excluded_authorities(+list(atom))
   ]).


%! rdf_cache:triple_to_iri(+Triple:rdf_triple, -Iri:iri) is nondet.

:- dynamic(rdf_cache:triple_to_iri/2).
:- multifile(rdf_cache:triple_to_iri/2).

rdf_cache:triple_to_iri(rdf(_,P,_), P).
rdf_cache:triple_to_iri(rdf(_,_,O), D):-
  rdf_is_literal(O),
  rdf_literal_data(datatype, O, D).
rdf_cache:triple_to_iri(rdf(_,P,O), O):-
  rdf_memberchk(P, [owl:equivalentClass,owl:sameAs,rdf:type,rdfs:subClassOf,rdfs:subPropertyOf]),
  rdf_is_iri(O).





%! rdf_cache is det.
% Wrapper around rdf_cache/1 with default options.

rdf_cache:-
  rdf_cache([]).


%! rdf_cache(+Options:list(compound)) is det.
% The following options are supported:
%   * excluded_authorities(+list(atom))
%     Default is `[]'.
%   * number_of_workers(+nonneg)
%     Default is `1'.
%   * Other options are passed to add_worker/3.

rdf_cache(Opts1):-
  Pool = rdf_cache,
  forall(distinct(X, rdf_iri(X)), add_resource(Pool, X)),
  print_pool(Pool),
  select_option(number_of_workers(N), Opts1, Opts2, 1),
  forall(between(1, N, _), add_worker(Pool, rdf_cache_worker(Opts2), Opts2)).


rdf_cache_worker(Opts, S, Ys):-
  option(excluded_authorities(ExclAuths), Opts, []),
  (   rdf_is_iri(S)
  ->  (   uri_components(S, uri_components(Scheme,Auth,_,_,_)),
          http_scheme(Scheme)
      ->  (   memberchk(Auth, ExclAuths)
          ->  Ys = [],
              debug(rdf_deref, "Skipping because of excluded authority: ~a", [S])
          ;   rdf_deref(S, G),
              aggregate_all(
                set(Y),
                (rdf(S, P, O, G), rdf_cache:triple_to_iri(rdf(S,P,O), Y)),
                Ys
              )
          )
      ;   Ys = [],
          debug(rdf_deref, "Skipping non-HTTP IRI: ~a", [S])
      )
  ;   Ys = [],
      debug(rdf_deref, "Skipping non-IRI: ~w", [S])
  ).



%! rdf_deref(+Subject:iri) is det.
% Wrapper around rdf_deref/2 that does not return the graph.

rdf_deref(S):-
  rdf_deref(S, _).


%! rdf_deref(+Subject:iri, -Graph:rdf_graph) is det.

rdf_deref(S, G):-
  iri_normalized(S, G),
  (   rdf_is_graph(G)
  ->  true
  ;   rdf_create_graph(G),
      forall(rdf_deref(S, P, O), rdf_assert(S, P, O, G)),
      if_debug(rdf_deref, rdf_print_graph(G, [id_closure(true)]))
  ).


%! rdf_deref(+Subject:iri, -Predicate:iri, -Object:rdf_term) is nondet.

rdf_deref(S, P, O):-
  rdf_call_on_graph(S, rdf_petty_min(S, P, O)).
