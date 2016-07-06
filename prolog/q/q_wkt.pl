:- module(
  q_wkt,
  [
    qb_wkt_point/4,                   % +M, +S, +Point, +G
    qu_replace_flat_wkt_geometry/3,   % +M1, +M2, +G
    qu_replace_flat_wkt_geometry/5,   % +M1, +M2, +Q, +R, +G
    qu_replace_nested_wkt_geometry/3, % +M1, +M2, +G
    qu_replace_nested_wkt_geometry/6  % +M1, +M2, +P, +Q, +R, +G
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
:- use_module(library(q/qu)).
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
   array_shape(+, r, -),
   qu_replace_flat_wkt_geometry(+, +, r, r),
   qu_replace_flat_wkt_geometry(+, +, r, r, r),
   qu_replace_nested_wkt_geometry(+, +, r, r, r),
   qu_replace_nested_wkt_geometry(+, +, r, r, r, r).


gis:gis_shape_hook(M, S, D, G, Shape) :-
  q(M, S, geold:geometry, Array^^D, G),
  array_shape(Array, D, Shape).


gis:subject_to_geometry(M, S, Array, Name, G) :-
  q(M, S, geold:geometry, Array^^D, G),
  rdf_global_id(wkt:Name0, D),
  capitalize_atom(Name0, Name).


rdf11:in_ground_type_hook(D, Array, Lex) :-
  rdf_global_id(wkt:Name, D),
  Shape =.. [Name,Array],
  atom_phrase(wkt(Shape), Lex).


rdf11:out_type_hook(D, Array, Lex) :-
  rdf_global_id(wkt:Name, D),
  atom_phrase(wkt(Shape), Lex),
  Shape =.. [Name,Array].


qh:qh_literal_hook(Array^^D, Opts) -->
  {
    rdf_global_id(wkt:_, D), !,
    q_literal_lex(Array^^D, Lex)
  },
  bs_truncated(Lex, Opts.max_length).





%! array_shape(+Array, +D, -Shape) is det.
%! array_shape(-Array, +D, +Shape) is det.

array_shape(L1, wkt:lineString, linestring(L2)) :- !,
  maplist(point_shape, L1, L2).
% HACK: We assume that all multi-polygons are plain polygons.
array_shape([Polygon], wkt:multiPolygon, Shape) :- !,
  array_shape(Polygon, wkt:polygon, Shape).
array_shape([X,Y], wkt:point, point(X,Y)) :- !.
array_shape([X,Y,Z], wkt:point, point(X,Y,Z)) :- !.
array_shape([X,Y,Z,M], wkt:point, point(X,Y,Z,M)) :- !.
array_shape([L1], wkt:polygon, polygon([L2])) :- !,
  maplist(point_shape, L1, L2).


point_shape([X,Y], point(X,Y)).



%! qb_wkt_point(+M, +S, +Point, +G) is det.

qb_wkt_point(M, S, Point, G) :-
  point_shape(Array, Point),
  atom_phrase(wkt(point(Array)), Lex),
  qb(M, S, geold:geometry, Lex^^wkt:point, G).



%! qu_replace_flat_wkt_geometry(+M1, +M2, +G) is det.
%! qu_replace_flat_wkt_geometry(+M1, +M2, +Q, +R, +G) is det.

qu_replace_flat_wkt_geometry(M1, M2, G) :-
  qu_replace_flat_wkt_geometry(M1, M2, rdf:type, geold:coordinates, G).


qu_replace_flat_wkt_geometry(M1, M2, Q, R, G) :-
  debug(qu(wkt), "Triplyfy flat WKT geometry notation.", []),
  qu_call((
    q(M1, S, Q, C, G),
    q(M1, S, R, Array^^D0, G)
  ), (
    q_iri_local(C, Local0),
    lowercase_atom(Local0, Local),
    rdf_global_id(wkt:Local, D),
    qb(M2, S, geold:geometry, Array^^D, G),
    qb_rm(M1, S, Q, C, G),
    qb_rm(M1, S, R, Array^^D0, G)
  )).



%! qu_replace_nested_wkt_geometry(+M1, +M2, +G) is det.
%! qu_replace_nested_wkt_geometry(+M1, +M2, +P, +Q, +R, +G) is det.

qu_replace_nested_wkt_geometry(M1, M2, G) :-
  qu_replace_nested_wkt_geometry(M1, M2, geold:geometry, rdf:type, geold:coordinates, G).


qu_replace_nested_wkt_geometry(M1, M2, P, Q, R, G) :-
  qu_call((
    q(M1, S, P, B, G),
    % Without the blank node check an already converted literal may
    % appear in the subject position, resulting in an exception.
    q_is_bnode(B),
    q(M1, B, Q, C, G),
    q(M1, B, R, Array^^D0, G)
  ), (
    q_iri_local(C, Local0),
    lowercase_atom(Local0, Local),
    rdf_global_id(wkt:Local, D),
    qb(M2, S, geold:geometry, Array^^D, G),
    qb_rm(M1, S, P, B, G),
    qb_rm(M1, B, Q, C, G),
    qb_rm(M1, B, R, Array^^D0, G)
  )).
