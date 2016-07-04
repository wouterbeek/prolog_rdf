:- module(
  q_wgs84,
  [
    q_wgs84_point/4,  % ?M, ?S, ?PlPoint, ?G
    qb_wgs84_point/4, % +M, +S, +PlPoint, +G
    qu_wgs84_point/3, % +M1, +M2, +G
    qu_wgs84_wkt/3    % +M1, +M2, +G
  ]
).

/** <module> Quine WGS84 support

rdfs:Resource
    ---[wgs84:location]--> wgs84:SpatialThing
        ---[wgs84:alt]--> xsd:float
        ---[wgs84:lat]--> xsd:float
        ---[wgs84:long]--> xsd:float

wgs84:lat_long (comma-separated)

wgs84:Point IS-A wgs84:SpatialThing

@author Wouter Beek
@see https://www.w3.org/2003/01/geo/wgs84_pos
@version 2016/06-2016/07
*/

:- use_module(library(debug)).
:- use_module(library(q/q_stmt)).
:- use_module(library(q/q_term)).
:- use_module(library(q/qb)).
:- use_module(library(q/qu)).
:- use_module(library(semweb/rdf11)).

:- qb_alias(wkt, 'http://geojsonld.com/wkt#').
:- qb_alias(wgs84, 'http://www.w3.org/2003/01/geo/wgs84_pos#').

:- multifile
    gis:gis_shape_hook/5,
    gis:subject_to_geometry/5,
    rdf11:in_ground_type_hook/3,
    rdf11:out_type_hook/3.

:- rdf_meta
   q_wgs84_point(?, r, ?, r),
   qb_wgs84_point(+, r, +, r),
   qu_wgs84_point(+, +, r),
   qu_wgs84_wkt(+, +, r).


gis:gis_shape_hook(M, S, D, G, Point) :-
  rdf_equal(wkt:point, D),
  q_wgs84_point(M, S, Point, G).


gis:subject_to_geometry(M, S, [Lng,Lat], point, G) :-
  q_wgs84_point(M, S, point(Lng,Lat), G).


rdf11:in_ground_type_hook(D, Lng-Lat, Lex) :-
  rdf_equal(wgs84:pair, D),
  atomic_list_concat([Lng,Lat], ',', Lex).


rdf11:out_type_hook(D, Lng-Lat, Lex) :-
  rdf_equal(wgs84:pair, D),
  atomic_list_concat(Comps, ',', Lex),
  maplist(atom_number, Comps, [Lng,Lat]).





%! q_wgs84_point(?M, ?S, ?PlPoint, ?G) is nondet.
%
% Succeeds if PlPoint denotes a geo-location of resource S.
%
% PlPoint is either of the form `point(?Lng,?Lat)` or
% `point(?Lng,?Lat,?Alt)`.

q_wgs84_point(M, S, PlPoint, G) :-
  q(M, S, wgs84:location, RdfPoint, G),
  q(M, RdfPoint, wgs84:long, Lng^^xsd:float, G),
  q(M, RdfPoint, wgs84:lat, Lat^^xsd:float, G),
  (   q(M, RdfPoint, wgs84:alt, Alt^^xsd:float, G)
  ->  PlPoint = point(Lng,Lat,Alt)
  ;   PlPoint = point(Lng,Lat)
  ).
q_wgs84_point(M, S, point(Lng,Lat), G) :-
  q(M, S, wgs84:location, RdfPoint, G),
  q(M, RdfPoint, wgs84:lat_long, Lng-Lat^^wgs84:pair, G).



%! qb_wgs84_point(+M, +S, +PlPoint, +G) is det.

qb_wgs84_point(M, S, point(Lng,Lat,Alt), G) :- !,
  qb_wgs84_point0(M, S, point(Lng,Lat), G, RdfPoint),
  qb(M, RdfPoint, wgs84:alt, Alt^^xsd:float, G).
qb_wgs84_point(M, S, point(Lng,Lat), G) :- !,
  qb_wgs84_point0(M, S, point(Lng,Lat), G, _).


qb_wgs84_point0(M, S, point(Lng,Lat), G, RdfPoint) :-
  qb_bnode(RdfPoint),
  qb(M, S, wgs84:location, RdfPoint, G),
  qb(M, RdfPoint, wgs84:long, Lng^^xsd:float, G),
  qb(M, RdfPoint, wgs84:lat, Lat^^xsd:float, G).



%! qu_wgs84_point(+M1, +M2, +G) is det.

qu_wgs84_point(M1, M2, G) :-
  qu_call((
    q(M1, S, wgs84:long, Lng^^xsd:float, G),
    q(M1, S, wgs84:lat, Lat^^xsd:float, G)
  ), (
    qb_bnode(RdfPoint),
    qb(M2, S, wgs84:location, RdfPoint, G),
    qu(M1, M2, S, wgs84:long, Lng^^xsd:float, G, subject(RdfPoint)),
    qu(M1, M2, S, wgs84:lat, Lat^^xsd:float, G, subject(RdfPoint))
  )).



%! qu_wgs84_wkt(+M1, +M2, +G) is det.

qu_wgs84_wkt(M1, M2, G) :-
  qu_wgs_84_wkt_deb,
  qu_call((
    q_wgs84_point(M1, S, PlPoint, G)
  ), (
    qb_wkt_point(M2, S, PlPoint, G)
  )).





% DEBUG %

qu_wgs84_wkt_deb :-
  debug(qu(wgs84_wkt), "Create WGS84 points", []).
