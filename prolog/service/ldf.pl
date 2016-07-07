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
  ldf_iri(S, P, O, Endpoint, Iri, MetaG),
  ldf_triple(Iri, MetaG, S, P, O).



%! ldf_iri(?S, ?P, ?O, +Endpoint, -Iri, -MetaG) is det.
%
% Creates an IRI implementing a Linked Triple Fragments request.

ldf_iri(S, P, O, Endpoint, Iri, MetaG) :-
  uri_components(Endpoint, uri_components(Scheme,Auth,Path,_,_)),
  maplist(ldf_parameter, [subject,predicate,object], [S,P,O], [S0,P0,O0]),
  exclude(var, [S0,P0,O0], Query0),
  uri_query_components(Query, Query0),
  uri_components(Iri, uri_components(Scheme,Auth,Path,Query,_)),
  atom_phrase(uri_optional_query_enc, Query, EncQuery),
  uri_components(MetaG, uri_components(Scheme,Auth,Path,EncQuery,metadata)).



%! ldf_parameter(+Key, +Val, -Param) is det.

ldf_parameter(_, Val, _) :-
  var(Val), !.
ldf_parameter(Key, Lit, Key=Val) :-
  rdf_is_literal(Lit), !,
  q_literal_lex(Lit, Lex),
  (   q_is_lts(Lit)
  ->  Lit = _@LTag,
      format(atom(Val), '"~a"@~a', [Lex,LTag])
  ;   z_literal_datatype(Lit, D),
      format(atom(Val), '"~a"^^~a', [Lex,D])  Scheme = http,
  Auth = 'ldf.lodlaundromat.org',
  doc_name(Doc, Name),
  atomic_list_concat(['',Name], /, Path),

  ).
ldf_parameter(Key, Val, Key=Val).



%! ldf_triple(+Iri, +MetaG, ?S, ?P, ?O) is det.

ldf_triple(Iri, MetaG, S, P, O) :-
  qb_iri(ex, G),
  rdf_create_graph(G),
  debug(ldf(load), "[LDF] Loading ~w", [Iri]),
  catch(
    rdf_load_file(Iri, [graph(G),rdf_format(nquads),silent(true)]),
    E,
    debug(ldf(load), "~w", [E])
  ),
  debug(ldf(load), "[LDF] Loaded ~w", [Iri]),
  ldf_read_triple(MetaG, S, P, O, G).



%! ldf_read_triple(+MetaG, ?S, ?P, ?O, ?G) is nondet.

ldf_read_triple(_, S, P, O, G) :-
  % NONDET
  q(rdf, S, P, O, G).
% If the metadata graph does not contain any triples then something is wrong.
ldf_read_triple(MetaG, _, _, _, _) :-
  \+ q(rdf, _, _, _, MetaG), !,
  fail.
  %%%%existence_error(ldf_metadata_graph, MetaG).
% There is a metadata graph: check whether there is a next page with more results.
ldf_read_triple(MetaG, S, P, O, G) :-
  q(rdf, _, hydra:nextPage, Iri, MetaG), !,
  debug(ldf(nextpage), "[LDF] Next page: ~w", [Iri]),
  rdf_unload_graph(G),
  rdf_unload_graph(MetaG),
  ldf_triple(Iri, MetaG, S, P, O).
% There is a metadata graph but it contains no more next page links.
ldf_read_triple(MetaG, _, _, _, G) :-
  rdf_unload_graph(G),
  rdf_unload_graph(MetaG),
  fail.
