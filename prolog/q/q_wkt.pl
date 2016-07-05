:- module(
  q_wkt,
  [
    qb_wkt_point/4,  % +M, +S, +Point, +G
    qu_flatten_wkt/3 % +M1, +M2, +G
  ]
).

/** <module> Quine Well-Known Text (WKT) plug-in

Allows WKT shapes to be read/written from/to the Quine triple store.

@author Wouter Beek
@version 2016/06-2016/07
*/

:- use_module(library(atom_ext)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(geo/wkt)).
:- use_module(library(html/html_bs)).
:- use_module(library(q/q_stmt)).
:- use_module(library(q/q_term)).
:- use_module(library(q/qb)).
:- use_module(library(semweb/rdf11)).

:- qb_alias(geold, 'http://geojsonld.com/vocab#').
:- qb_alias(wkt, 'http://geojsonld.com/wkt#').

:- multifile
    gis:gis_shape_hook/5,
    gis:subject_to_geometry/5,
    rdf11:in_ground_type_hook/3,
    rdf11:out_type_hook/3,
    qh:qh_literal_hook//2.

:- rdf_meta
   array2shape(+, r, -),
   q_wkt_geometry(+, r, -, -, r),
   qu_wkt_point(+, r, +, r).


gis:gis_shape_hook(M, S, D, G, Shape) :-
  q(M, S, geold:geometry, Array^^D, G),
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


gis:subject_to_geometry(M, S, Array, Name, G) :-
  q(M, S, geold:geometry, Array^^D, G),
  rdf_global_id(wkt:Name0, D),
  capitalize_atom(Name0, Name).


rdf11:in_ground_type_hook(D, Array, Lex) :-
  rdf_global_id(wkt:Name, D),
  Shape =.. [Name,Array],
  atom_phrase(wkt(Shape), Lex).


rdf11:out_type_hook(D, Array, Lex) :-
  atom_phrase(wkt(Shape), Lex),
  Shape =.. [Name,Array],
  rdf_global_id(wkt:Name, D).


qh:qh_literal_hook(Array^^D, Opts) -->
  {
    rdf_global_id(wkt:_, D), !,
    q_literal_lex(Array^^D, Lex)
  },
  bs_truncated(Lex, Opts.max_length).





%! qb_wkt_point(+M, +S, +Point, +G) is det.

qb_wkt_point(M, S, Point, G) :-
  atom_phrase(wkt(point(Point)), Lex),
  qb(M, S, geold:geometry, Lex^^wkt:point, G).



%! qu_flatten_wkt(+M1, +M2, +G) is det.

qu_flatten_wkt(M1, M2, G) :-
  debug(qu(wkt), "Flatten WKT geometries.", []),
  qu_call((
    q(M1, S, geold:geometry, B, G),
    % Without the blank node check an already converted literal may
    % appear in the subject position, resulting in an exception.
    q_is_bnode(B),
    q(M1, B, rdf:type, C, G),
    q(M1, B, geold:coordinates, Array^^tcco:array, G)
  ), (
    q_iri_local(C, Local0),
    lowercase_atom(Local0, Local),
    rdf_global_id(wkt:Local, D),
    qb(M2, S, geold:geometry, Array^^D, G),
    qb_rm(M1, S, geold:geometry, B, G),
    qb_rm(M1, B, rdf:type, C, G),
    qb_rm(M1, B, geold:coordinates, Array^^tcco:array, G)
  )).
