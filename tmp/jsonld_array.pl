:- module(
  jsonld_array,
  [
    array//1,
    geold_array2wkt/0
  ]
).

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

array(L) --> "[", seplist(array, L), "]", !.
array(N) --> float(N).

jsonld_generics:jsonld_keyword_hook('@array').





%! geold_array2wkt is nondet.
%
% Transforms JSON-LD arrays (see module [[jsonld_array]]) into
% Well-Known Text literals (see module [[wkt]]).

geold_array2wkt :-
  geold_array2wkt(_{count:0}).


geold_array2wkt(State) :-
  geold_geo_class(C, Name),
  rdfs_instance(I, C),
  rdf_has(I, geold:coordinates, Lex1^^tcco:array),
  string_phrase(array(Array), Lex1),
  string_phrase(wkt(Name, Array), Lex2),
  rdf_global_id(wkt:Name, D),
  rdf_change(I, geold:coordinates, Lex1^^tcco:array, object(Lex2^^D)),
  dict_inc(count, State),
  fail.
geold_array2wkt(State) :-
  ansi_format(user_output, [fg(yellow)], "~D arrays to WKT.~n", [State.count]).
