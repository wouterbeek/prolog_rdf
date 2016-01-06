:- module(
  jsonld,
  [
    jsonld_deref/2 % +S, -Dict
  ]
).

/** <module> JSON-LD

@author Wouter Beek
@compat JSON-LD 1.1
@see http://www.w3.org/TR/json-ld/
@version 2016/01
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(rdf/rdf_literal)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_term)).

:- rdf_meta
   jsonld_deref(r, -).





%! jsonld_deref(+S, -Dict) is det.

jsonld_deref(S, D):-
  aggregate_all(set(P-O), rdf(S, P, O), Pairs1),
  maplist(jsonld_pair, Pairs1, Pairs2),
  dict_pairs(D, json_ld, ['@id'-S|Pairs2]).


jsonld_pair(P-Lit, P-rdf_literal{'@type': D, '@value': Lex}):-
  rdf_is_literal(Lit), !,
  rdf_literal_data(datatype, Lit, D),
  rdf_literal_data(lexical_form, Lit, Lex).
jsonld_pair(P-Iri, P-rdf_iri{'@id': Iri}):-
  rdf_is_iri(Iri).
