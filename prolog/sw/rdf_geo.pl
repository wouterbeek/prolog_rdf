:- module(rdf_geo, []).


/** <module> RDF geospatial support

@author Wouter Beek
@version 2018
*/

:- use_module(library(dcg)).
:- use_module(library(gis/wkt)).
:- use_module(library(sw/rdf_prefix)).
:- use_module(library(sw/rdf_term)).

:- dynamic
    rdf_lexical_to_value_hook/3,
    rdf_value_to_lexical_hook/3.

:- multifile
    rdf_lexical_to_value_hook/3,
    rdf_value_to_lexical_hook/3.

:- rdf_assert_prefix(geo).

:- rdf_meta
   rdf_lexical_to_value_hook(r, +, -),
   rdf_value_to_lexical_hook(r, +, -).

rdf_term:rdf_lexical_to_value_hook(D, Lex, Shape) :-
  rdf_equal(D, geo:wktLiteral), !,
  wkt_shape_atom(Shape, Lex).

rdf_term:rdf_value_to_lexical_hook(D, Shape, Lex) :-
  rdf_equal(D, geo:wktLiteral), !,
  wkt_shape_atom(Shape, Lex).
