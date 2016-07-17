:- module(
  ldf,
  [
    ldf/4  % ?S, ?P, ?O, +Endpoint
  ]
).

/** <module> Linked Data Fragments (LDF) client

@author Wouter Beek
@version 2015/08, 2016/07
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(lists)).
:- use_module(library(q/q_stmt)).
:- use_module(library(q/q_term)).
:- use_module(library(q/qb)).
:- use_module(library(rdf/rdfio)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(uri)).

:- qb_alias(hydra, 'http://www.w3.org/ns/hydra/core#').

:- rdf_meta
   ldf(r, r, o, r).





%! ldf(?S, ?P, ?O, +Endpoint) is nondet.

ldf(S, P, O, Endpoint) :-
  uri_components(Endpoint, uri_components(Scheme,Auth,Path,_,_)),
  maplist(ldf_parameter, [subject,predicate,object], [S,P,O], [S0,P0,O0]),
  exclude(var, [S0,P0,O0], Query0),
  uri_query_components(Query, Query0),
  % An IRI that implements a Linked Triple Fragments request.
  uri_components(Iri, uri_components(Scheme,Auth,Path,Query,_)),
  ldf_request(Iri, S, P, O).



%! ldf_parameter(+Key, +Val, -Param) is det.

ldf_parameter(_, Val, _) :-
  var(Val), !.
ldf_parameter(Key, Lit, Key=Val) :-
  rdf_is_literal(Lit), !,
  q_literal_lex(Lit, Lex),
  (   q_is_lts(Lit)
  ->  Lit = _@LTag,
      format(atom(Val), '"~a"@~a', [Lex,LTag])
  ;   q_literal_datatype(Lit, D),
      format(atom(Val), '"~a"^^~a', [Lex,D])
  ).
ldf_parameter(Key, Val, Key=Val).



%! ldf_request(+Iri, ?S, ?P, ?O) is det.

ldf_request(Iri1, S, P, O) :-
  rdf_load_quads(Iri1, Quads, [rdf_format(trig)]),
  partition(q_is_def_quad, Quads, DataQuads, MetaQuads),
  (   member(rdf(S,P,O,_), DataQuads)
  ;   % Check whether there is a next page with more results.
      rdf_equal(hydra:nextPage, Q),
      memberchk(rdf(_,Q,Iri2,_), MetaQuads),
      ldf_request(Iri2, S, P, O)
  ).
