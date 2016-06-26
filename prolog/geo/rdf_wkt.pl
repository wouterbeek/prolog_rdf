:- module(rdf_wkt, []).

/** <module> RDF support for Well-Known Text (WKT)

Allows WKT shapes to be read/written from/to the RDF DB.

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(geo/wkt)).
:- use_module(library(html/html_bs)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(z/z_stmt)).
:- use_module(library(z/z_term)).

:- rdf_register_prefix(geold, 'http://geojsonld.com/vocab#').
:- rdf_register_prefix(wkt, 'http://geojsonld.com/wkt#').

:- rdf_meta
   array2shape(+, r, -).

:- multifile
   gis:resource_shape_hook/4,
   rdf11:in_ground_type_hook/3,
   rdf11:out_type_hook/3,
   zh:zh_literal_hook//2.

gis:resource_shape_hook(Res, D, Shape, G) :-
  z(Res, geold:geometry, Array^^D, G),
  array2shape(Array, D, Shape).

array2shape(L1, wkt:lineString, linestring(L2)) :- !,
  maplist(point2shape, L1, L2).
% HACK: We assume that all multi-polygons are plain polygons.
array2shape([Polygon], wkt:multiPolygon, Shape) :- !,
  array2shape(Polygon, wkt:polygon, Shape).
array2shape([X,Y], wkt:point, point(X,Y)) :- !.
array2shape([X,Y,Z], wkt:point, point(X,Y,Z)) :- !.
array2shape([X,Y,Z,M], wkt:point, point(X,Y,Z,M)) :- !.
array2shape([L1], wkt:polygon, polygon([L2])) :- !,
  maplist(point2shape, L1, L2).

point2shape([X,Y], point(X,Y)).

%polygon([[point(52.1335811883338,4.24342337208216),point(52.1240808418951,4.23342263416468),point(52.1827499743908,3.87006309399112),point(52.0599123264119,3.27368644149239),point(52.076579608387,3.2736864626958),point(52.1994172644824,3.87006311490954),point(52.1335811883338,4.24342337208216)]])

rdf11:in_ground_type_hook(D, Array, Lex) :-
  rdf_global_id(wkt:Name, D),
  Shape =.. [Name,Array],
  atom_phrase(wkt(Shape), Lex).

rdf11:out_type_hook(D, Array, Lex) :-
  atom_phrase(wkt(Shape), Lex),
  Shape =.. [Name,Array],
  rdf_global_id(wkt:Name, D).

zh:zh_literal_hook(Array^^D, Opts) -->
  {rdf_global_id(wkt:_, D)}, !,
  {z_literal_lex(Array^^D, Lex)},
  bs_truncated(Lex, Opts.max_length).
