:- module(
  rdf_geo,
  [
    rdf_assert_wkt/3, % +Feature, +Shape, +G
    rdf_assert_wkt/4, % +Feature, +Shape, +G, -Geometry
    rdf_wkt/2,        % ?Feature, ?Shape
    rdf_wkt/3         % ?Feature, ?Shape, ?G
  ]
).

/** <module> RDF Geography Plugin

@author Wouter Beek
@version 2017/05-2017/09
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(geo/wkt_generate)).
:- use_module(library(geo/wkt_parse)).
:- use_module(library(semweb/rdf_api)).
:- use_module(library(semweb/rdf_print)).

:- multifile
    rdf_dcg_literal_hook//2,
    in_ground_type_hook/3,
    out_type_hook/3.

rdf_dcg_literal_hook(Shape^^geo:wktLiteral, Opts) -->
  {atom_phrase(wkt_generate(Shape), Lex)},
  rdf_dcg_lexical_form(Lex, Opts).

% (+D,+Shape,-Lex)
in_ground_type_hook(geo:wktLiteral, Shape, Lex) :-
  atom_phrase(wkt_generate(Shape), Lex).

% (+D,-Shape,+Lex)
out_type_hook(geo:wktLiteral, Shape, Lex) :-
  atom_phrase(wkt_parse(Shape), Lex).

:- rdf_meta
   rdf_assert_wkt(r, +, r),
   rdf_assert_wkt(r, +, r, -),
   rdf_wkt(r, ?),
   rdf_wkt(r, ?, r).





%! rdf_assert_wkt(+Feature, +Shape, +G) is det.
%! rdf_assert_wkt(+Feature, +Shape, +G, -Geometry) is det.

rdf_assert_wkt(Feature, Shape, G) :-
  rdf_assert_wkt(Feature, Shape, G, _).


rdf_assert_wkt(Feature, Shape, G, Geometry) :-
  rdf_create_well_known_iri(Geometry),
  rdf_assert(Feature, geo:hasGeometry, Geometry, G),
  rdf_assert(Geometry, geo:asWKT, Shape^^geo:wktLiteral, G),
  rdf_assert(Geometry, rdf:type, geo:'Geometry', G).



%! rdf_wkt(?Feature, ?Shape) is nondet.
%! rdf_wkt(?Feature, ?Shape, ?G) is nondet.

rdf_wkt(Feature, Shape) :-
  rdf_wkt(Feature, Shape, _).


rdf_wkt(Feature, Shape, G) :-
  rdf(Feature, geo:hasGeometry, BNode, G),
  rdf(BNode, geo:asWKT, Shape^^geo:wktLiteral, G).
