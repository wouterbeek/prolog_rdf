:- module(rdf_wkt, []).

/** <module> RDF support for Well-Known Text (WKT)

Allows WKT shapes to be read/written from/to the RDF DB.


polygon([[point(52.1335811883338,4.24342337208216),point(52.1240808418951,4.23342263416468),point(52.1827499743908,3.87006309399112),point(52.0599123264119,3.27368644149239),point(52.076579608387,3.2736864626958),point(52.1994172644824,3.87006311490954),point(52.1335811883338,4.24342337208216)]])

array2shape(L1, linestring, linestring(L2)) :- !,
  maplist(point0, L1, L2).
array2shape([L1], multipolygon, polygon([L2])) :- !,
  maplist(point0, L1, L2).
array2shape([X,Y], point, point(X,Y)) :- !.
array2shape([X,Y,Z], point, point(X,Y,Z)) :- !.
array2shape([X,Y,Z,M], point, point(X,Y,Z,M)) :- !.
array2shape([L1], polygon, polygon([L2])) :- !,
  maplist(point0, L1, L2).

point0([X,Y], point(X,Y)).

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(geo/wkt)).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(semweb/rdf11)).

:- rdf_register_prefix(wkt, 'http://geojsonld.com/wkt#').

:- dynamic
   gis:resource_shape_hook/3,
   rdf11:in_ground_type_hook/3,
   rdf11:out_type_hook/3.

:- multifile
   gis:resource_shape_hook/3,
   rdf11:in_ground_type_hook/3,
   rdf11:out_type_hook/3.

gis:resource_shape_hook(Res, Shape, G) :-
  rdf_has(Res, geold:geometry, Shape^^_, _, G).

rdf11:in_ground_type_hook(D, Shape, Lex) :-
  wkt_same_type0(D, Shape),
  atom_phrase(wkt(Shape), Lex).

rdf11:out_type_hook(D, Shape, Lex) :-
  atom_phrase(wkt(Shape), Lex),
  wkt_same_type0(D, Shape).

wkt_same_type0(D, Shape) :-
  rdf_global_id(wkt:Name, D),
  Shape =.. [Name|_].
