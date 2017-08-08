:- module(
  rdf_geo,
  [
    rdf_assert_wkt/3, % +Feature, +Shape, +G
    rdf_assert_wkt/4, % +Feature, +Shape, +G, -Geometry
    rdf_wkt/3,        % +M, ?Feature, ?Shape
    rdf_wkt/4         % +M, ?Feature, ?Shape, ?G
  ]
).
:- reexport(library(semweb/rdf_ext)).

/** <module> RDF Geography Plugin

@author Wouter Beek
@version 2017/05-2017/07
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(geo/wkt_generate)).
:- use_module(library(geo/wkt_parse)).
:- use_module(library(semweb/rdf_print)).

:- multifile
    rdf_print:rdf_dcg_literal_hook//2,
    rdf11:in_ground_type_hook/3,
    rdf11:out_type_hook/3.

rdf_print:rdf_dcg_literal_hook(Shape^^D, Opts) -->
  {
    rdf_equal(geo:wktLiteral, D), !,
    atom_phrase(wkt_generate(Shape), Lex)
  },
  rdf_dcg_lexical_form(Lex, Opts).

% (+D,+Shape,-Lex)
rdf11:in_ground_type_hook(D, Shape, Lex) :-
  % @bug
  rdf_equal(geo:wktLiteral, D),
  atom_phrase(wkt_generate(Shape), Lex).

% (+D,-Shape,+Lex)
rdf11:out_type_hook(D, Shape, Lex) :-
  % @bug
  rdf_equal(geo:wktLiteral, D),
  atom_phrase(wkt_parse(Shape), Lex).

:- rdf_meta
   rdf_assert_wkt(r, +, r),
   rdf_assert_wkt(r, +, r, -),
   rdf_wkt(+, r, ?),
   rdf_wkt(+, r, ?, r).





%! rdf_assert_wkt(+Feature, +Shape, +G) is det.
%! rdf_assert_wkt(+Feature, +Shape, +G, -Geometry) is det.

rdf_assert_wkt(Feature, Shape, G) :-
  rdf_assert_wkt(Feature, Shape, G, _).


rdf_assert_wkt(Feature, Shape, G, Geometry) :-
  rdf_create_bnode_iri(Geometry),
  rdf_assert(Feature, geo:hasGeometry, Geometry, G),
  % @bug
  rdf_equal(geo:wktLiteral, D),
  rdf_assert(Geometry, geo:asWKT, Shape^^D, G),
  % @bug
  rdf_equal(geo:'Geometry', C),
  rdf_assert(Geometry, rdf:type, C, G).



%! rdf_wkt(+M, ?Feature, ?Shape) is nondet.
%! rdf_wkt(+M, ?Feature, ?Shape, ?G) is nondet.

rdf_wkt(M, Feature, Shape) :-
  rdf_wkt(M, Feature, Shape, _).


rdf_wkt(M, Feature, Shape, G) :-
  rdf(M, Feature, geo:hasGeometry, BNode, G),
  rdf(M, BNode, geo:asWKT, Shape^^geo:wktLiteral, G).
