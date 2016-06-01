:- module(jsonld_array, [array//1]).

/** <module> JSON-LD array

Because JSON-LD cannot deal with GeoJSON coordinate values, we have to
extend it ourselves.  We do this by adding array support.  This way
JSON-LD can be translated into triples while preserving the array
information.  Later RDF transformations can then be used to interpret
the array as e.g. Well-Known Text (WKT).

@author Wouter Beek
@version 2016/05
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(jsonld/jsonld_read), []).
:- use_module(library(semweb/rdf11)).

:- rdf_register_prefix(tcco, 'http://triply.cc/ontology/').

:- dynamic
    jsonld_generics:jsonld_keyword_hook/1,
    jsonld_read:jsonld_tuple_hook/7.

:- multifile
    jsonld_generics:jsonld_keyword_hook/1,
    jsonld_read:jsonld_tuple_hook/7.





% Add array support for JSON-LD.
jsonld_read:jsonld_tuple_hook(Context, S, P, ODef, _, L, Tuple) :-
  ODef == '@array', !,
  string_phrase(array(L), Lex),
  jsonld_read:tuple_term(Context, S, P, Lex^^tcco:array, Tuple).


jsonld_generics:jsonld_keyword_hook('@array').


array(L) --> "[", seplist(array, L), "]", !.
array(N) --> float(N).
