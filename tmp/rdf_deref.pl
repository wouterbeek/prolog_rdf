:- module(
  rdf_deref,
  [
    rdf_cache/0,
    rdf_cache/1 % +Opts
  ]
).

/** <module> RDF dereference

@author Wouter Beek
@tbd Fix IRI normalization,
@version 2015/12-2016/02, 2016/04
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(debug_ext)).
:- use_module(library(http/http_ext)).
:- use_module(library(iri/iri_ext)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(pool)).
:- use_module(library(q/q_print)).
:- use_module(library(q/q_term)).
:- use_module(library(rdf/rdfio)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(yall)).

:- dynamic
    rdf_cache:triple_to_iri/2.

:- multifile
    rdf_cache:triple_to_iri/2.

:- debug(rdf(deref)).





%! rdf_cache:triple_to_iri(+Triple, -Iri) is nondet.

rdf_cache:triple_to_iri(rdf(_,P,_), P).
rdf_cache:triple_to_iri(rdf(_,_,O), D) :-
  rdf_is_literal(O),
  q_literal_datatype(O, D).
rdf_cache:triple_to_iri(rdf(_,P,O), O) :-
  (rdf_equal(owl:equivalentClass, P), ! ; rdf_equal(owl:sameAs, P)),
  q_is_iri(O).



%! rdf_cache is det.
%! rdf_cache(+Opts) is det.
%
% The following options are supported:
%
%   * excluded_authorities(+list(atom)) Default is `[]'.
%
%   * number_of_workers(+nonneg) Default is `1'.
%
%   * Other options are passed to add_worker/3.

rdf_cache:-
  rdf_cache([]).


rdf_cache(Opts1) :-
  Pool = rdf_cache,
  forall(distinct(X, rdf_iri(X)), add_resource(Pool, X)),
  select_option(number_of_workers(N), Opts1, Opts2, 1),
  forall(between(1, N, _), add_worker(Pool, rdf_cache_worker(Opts2), Opts2)).


rdf_cache_worker(Opts, S, Ys) :-
  option(excluded_authorities(ExclAuths), Opts, []),
  (   rdf_is_iri(S)
  ->  (   iri_comps(S, uri_components(Scheme,Auth,_,_,_)),
          http_scheme(Scheme)
      ->  (   memberchk(Auth, ExclAuths)
          ->  Ys = [],
              debug(rdf(deref), "Skipping because of excluded authority: ~a", [S])
          ;   rdf_deref(S),
              if_debug(rdf(deref), print_pool(rdf_cache)),
              aggregate_all(
                set(Y),
                (rdf(S, P, O, S), rdf_cache:triple_to_iri(rdf(S,P,O), Y)),
                Ys
              )
          )
      ;   Ys = [],
          debug(rdf(deref), "Skipping non-HTTP IRI: ~a", [S])
      )
  ;   Ys = [],
      debug(rdf(deref), "Skipping non-IRI: ~w", [S])
  ).
