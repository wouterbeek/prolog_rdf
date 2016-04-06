:- module(
  rdf_deref,
  [
    rdf_cache/0,
    rdf_cache/1, % +Opts
    rdf_deref/1  % +S
  ]
).

/** <module> RDF: Synchronization

@author Wouter Beek
@tbd Fix IRI normalization,
@version 2015/12-2016/02
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(debug_ext)).
:- use_module(library(http/http_ext)).
:- use_module(library(iri/iri_ext)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(pool)).
:- use_module(library(rdf/rdf_load)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(yall)).

:- debug(rdf_deref(overview)).
:- debug(rdf_deref(request)).
%:- debug(rdf_deref(result)).

:- rdf_meta(rdf_deref(r)).

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
rdf_cache:triple_to_iri(rdf(_,_,O), D) :-
  rdf_is_literal(O),
  rdf_literal_datatype(O, D).
rdf_cache:triple_to_iri(rdf(_,P,O), O) :-
  (rdf_equal(owl:equivalentClass, P), ! ; rdf_equal(owl:sameAs, P)),
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
              debug(rdf_deref, "Skipping because of excluded authority: ~a", [S])
          ;   rdf_deref(S),
              if_debug(rdf_deref(overview), print_pool(rdf_cache)),
              aggregate_all(
                set(Y),
                (rdf(S, P, O, S), rdf_cache:triple_to_iri(rdf(S,P,O), Y)),
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

rdf_deref(S) :-
  debug(rdf_deref(request), "Dereferencing ~a", [S]),
  call_collect_messages(rdf_call_on_tuples(S, rdf_deref_tuple(S))),
  if_debug(rdf_deref(result), rdf_print_quads(S, _, _, _)).

rdf_deref_tuple(S1, S2, P, O, _) :-
  is_same_iri(S1, S2, S3), !,
  rdf_assert(S3, P, O, S2).
rdf_deref_tuple(_, _, _, _, _).

is_same_iri(X, Y, Z) :-
  iri_normalized(X, Z),
  iri_normalized(Y, Z).
