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
:- use_module(library(debug)).
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
  atom_phrase(uri_optional_query_enc, Query, EncQuery),
  % The name of the metadata graph.
  uri_components(MetaG, uri_components(Scheme,Auth,Path,EncQuery,metadata)),
  ldf_from_iri(Iri, S, P, O, MetaG).



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



%! ldf_from_iri(+Iri, ?S, ?P, ?O, +MetaG) is det.

ldf_from_iri(Iri, S, P, O, MetaG) :-
  rdf_tmp_graph(DataG),
  rdf_load_file(Iri, [force_graph(DataG),rdf_format(nquads)]),
  ldf_from_graph(S, P, O, DataG, MetaG).



%! ldf_from_graph(?S, ?P, ?O, +DataG, +MetaG) is nondet.

ldf_from_graph(S, P, O, DataG, _) :-
  q(rdf, S, P, O, DataG).
% If the metadata graph does not contain any triples then something is
% wrong.
ldf_from_graph(_, _, _, _, MetaG) :-
  \+ q(rdf, _, _, _, MetaG), !,
  existence_error(ldf_metadata_graph, MetaG).
% There is a metadata graph: check whether there is a next page with
% more results.
ldf_from_graph(S, P, O, DataG, MetaG) :-
  q(rdf, _, hydra:nextPage, Iri, MetaG), !,
  ldf_unload(DataG, MetaG),
  ldf_from_iri(Iri, S, P, O, MetaG).
% There is a metadata graph but it contains no more next page links.
ldf_from_graph(_, _, _, DataG, MetaG) :-
  ldf_unload(DataG, MetaG),
  fail.



%! ldf_unload(+DataG, +MetaG) is det.

ldf_unload(DataG, MetaG) :-
  rdf_unload_graph(DataG),
  rdf_unload_graph(MetaG).
