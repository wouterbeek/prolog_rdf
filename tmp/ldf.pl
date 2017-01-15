:- module(
  ldf,
  [
    ldf/4,           % ?S, ?P, ?O, +Endpoint
    ldf_guess_size/5 % ?S, ?P, ?O, +Endpoint, -NumTriples
  ]
).

/** <module> Linked Data Fragments (LDF) client

@author Wouter Beek
@version 2015/08, 2016/07-2016/08
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(rdf/rdf__io)).
:- use_module(library(rdf/rdf_api)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(uri)).

:- rdf_meta
   ldf(r, r, o, r),
   ldf_guess_size(r, r, o, r, -).





%! ldf(?S, ?P, ?O, +Endpoint) is nondet.

ldf(S, P, O, Endpoint) :-
  ldf_request_iri(S, P, O, Endpoint, RequestIri),
  ldf_request(RequestIri, S, P, O).



%! ldf_guess_size(?S, ?P, ?O, +Endpoint, -NumTriples) is det.

ldf_guess_size(S, P, O, Endpoint, NumTriples) :-
  ldf_request_iri(S, P, O, Endpoint, RequestIri),
  rdf_load_quads(RequestIri, Quads, [rdf_media_type(application/trig)]),
  partition(q_is_def_quad, Quads, _, MetaQuads),
  rdf_equal(hydra:totalItems, P),
  memberchk(rdf(_,P,NumTriples^^_,_), MetaQuads).





% HELPERS %

%! ldf_iri_page(+Iri, -Page) is det.
%
% Useful for debugging since this allows the page of a request IRI to
% be extracted.

ldf_iri_page(Iri, Page) :-
  uri_components(Iri, uri_components(_,_,_,Query,_)),
  uri_query_components(Query, QueryComps),
  option(page(Page0), QueryComps), !,
  atom_number(Page0, Page).
ldf_iri_page(_, 1).



%! ldf_parameter(+Key, +Val, -Param) is det.

ldf_parameter(_, Val, _) :-
  var(Val), !.
ldf_parameter(Key, Lit, Key=Val) :-
  q_is_literal(Lit), !,
  rdf_literal_lexical_form(Lit, Lex),
  (   q_is_lts(Lit)
  ->  Lit = _@LTag,
      format(atom(Val), '"~a"@~a', [Lex,LTag])
  ;   q_literal_datatype(Lit, D),
      format(atom(Val), '"~a"^^~a', [Lex,D])
  ).
ldf_parameter(Key, Val, Key=Val).



%! ldf_request(+RequestIri, ?S, ?P, ?O) is det.

ldf_request(RequestIri1, S, P, O) :-
  rdf_load_quads(RequestIri1, Quads, [rdf_media_type(application/trig)]),
  partition(q_is_def_quad, Quads, DataQuads, MetaQuads),
  (   member(rdf(S,P,O,_), DataQuads)
  ;   % Check whether there is a next page with more results.
      rdf_equal(hydra:nextPage, Q),
      memberchk(rdf(_,Q,RequestIri2,_), MetaQuads),
      ldf_request(RequestIri2, S, P, O)
  ).



%! ldf_request_iri(?S, ?P, ?O, +Endpoint, -RequestIri) is det.
%
% An IRI that implements a Linked Triple Fragments request.

ldf_request_iri(S, P, O, Endpoint, RequestIri) :-
  uri_components(Endpoint, uri_components(Scheme,Auth,Path,_,_)),
  maplist(ldf_parameter, [subject,predicate,object], [S,P,O], [S0,P0,O0]),
  exclude(var, [S0,P0,O0], Query0),
  uri_query_components(Query, Query0),
  uri_components(RequestIri, uri_components(Scheme,Auth,Path,Query,_)).
