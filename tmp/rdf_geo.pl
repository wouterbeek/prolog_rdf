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

Prefix notation does not work in hooks (workaround: datatype
match in body).

Multifile hooks do not work (workaround: module prefix).

@author Wouter Beek
@version 2017-2018
*/

:- use_module(library(dcg)).
:- use_module(library(sw/rdf_term)).

% (+D, +Shape, -Lex)
rdf11:in_ground_type_hook(D, Shape, Lex) :-
  rdf_equal(D, geo:wktLiteral), !,
  atom_phrase(wkt_generate(Shape), Lex).

% (+D, -Shape, +Lex)
rdf11:out_type_hook(D, Shape, Lex) :-
  rdf_equal(D, geo:wktLiteral), !,
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
  rdf_bnode_iri(Geometry),
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