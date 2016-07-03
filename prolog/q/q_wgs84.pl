:- module(
  q_wgs84,
  [
    q_wgs84_alt/4,      % ?M, ?S, ?Alt, ?G
    q_wgs84_lat/4,      % ?M, ?S, ?Lat, ?G
    q_wgs84_lat_long/5, % ?M, ?S, ?Lat, ?Lng, ?G
    q_wgs84_long/4,     % ?M, ?S, ?Lng, ?G
    q_wgs84_point/4,    % ?M, ?S, ?PlPoint, ?G
    qb_wgs84_point/4,   % +M, +S, +PlPoint, +G
    qu_wgs84_point/3,   % +M1, +M2, +G
    qu_wgs84_wkt/3      % +M1, +M2, +G
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

@author Willem Robert van Hage
@version 2009-2012

@author Wouter Beek
@see https://www.w3.org/2003/01/geo/wgs84_pos
@version 2016/06
*/

:- use_module(library(q/q_stmt)).
:- use_module(library(q/q_term)).
:- use_module(library(q/qb)).
:- use_module(library(q/qu)).
:- use_module(library(semweb/rdf11)).

:- qb_alias(wkt, 'http://geojsonld.com/wkt#').
:- qb_alias(wgs84, 'http://www.w3.org/2003/01/geo/wgs84_pos#').

:- multifile
   gis:gis_shape_hook/5,
   rdf11:in_ground_type_hook/3,
   rdf11:out_type_hook/3.

:- rdf_meta
   q_wgs84_alt(?, r, ?, r),
   q_wgs84_lat(?, r, ?, r),
   q_wgs84_lat_long(?, r, ?, ?, r),
   q_wgs84_long(?, r, ?, r),
   q_wgs84_point(?, r, ?, r),
   qb_wgs84_point(+, r, +, r),
   qu_wgs84_point(+, +, r),
   qu_wgs84_wkt(+, +, r).

gis:gis_shape_hook(M, S, D, G, Point) :-
  rdf_equal(wkt:point, D),
  q_wgs84_point(M, S, Point, G).

rdf11:in_ground_type_hook(D, Lat-Lng, Lex) :-
  rdf_equal(wgs84:pair, D),
  atomic_list_concat([Lat,Lng], ',', Lex).

rdf11:out_type_hook(D, Lat-Lng, Lex) :-
  rdf_equal(wgs84:pair, D),
  atomic_list_concat(Comps, ',', Lex),
  maplist(atom_number, Comps, [Lat,Lng]).





%! q_wgs84_alt(?M, ?S, ?Alt, ?G) is nondet.
%
% Succeeds if Alt is the WGS84 altitude of resource S.

q_wgs84_alt(M, S, Alt, G) :-
  q(M, S, wgs84:location, RdfPoint, G),
  q(M, RdfPoint, wgs84:alt, Alt^^xsd:float, G).



%! q_wgs84_lat(?M, ?S, ?Lat, ?G) is nondet.
%
% Succeeds if Lat is the WGS84 latitude of resource S.

q_wgs84_lat(M, S, Lat, G) :-
  q(M, S, wgs84:location, RdfPoint, G),
  q(M, RdfPoint, wgs84:lat, Lat^^xsd:float, G).



%! q_wgs84_lat_long(?M, ?S, ?Lat, ?Lng, ?G) is nondet.

q_wgs84_lat_long(M, S, Lat, Lng, G) :-
  q(M, S, wgs84:location, RdfPoint, G),
  q(M, RdfPoint, wgs84:lat_long, Lat-Lng^^wgs84:pair, G).



%! q_wgs84_long(?M, ?S, ?Lng, ?G) is nondet.
%
% Succeeds if Lng is the WGS84 longitude of resource S.

q_wgs84_long(M, S, Lng, G) :-
  q(M, S, wgs84:location, RdfPoint, G),
  q(M, RdfPoint, wgs84:long, Lng^^xsd:float, G).



%! q_wgs84_point(?M, ?S, ?PlPoint, ?G) is nondet.
%
% Succeeds if PlPoint denotes a geo-location of resource S.
%
% PlPoint is either of the form `point(?Lat,?Lng)` or
% `point(?Lat,?Lng,?Alt)`.

q_wgs84_point(M, S, PlPoint, G) :-
  q(M, S, wgs84:location, RdfPoint, G),
  q(M, RdfPoint, wgs84:lat, Lat^^xsd:float, G),
  q(M, RdfPoint, wgs84:long, Lng^^xsd:float, G),
  (   q(M, RdfPoint, wgs84:alt, Alt^^xsd:float, G)
  ->  PlPoint = point(Lat,Lng,Alt)
  ;   PlPoint = point(Lat,Lng)
  ).



%! qb_wgs84_point(+M, +S, +PlPoint, +G) is det.

qb_wgs84_point(M, S, point(Lat,Lng,Alt), G) :- !,
  qb_wgs84_point0(M, S, point(Lat,Lng), G, RdfPoint),
  qb(M, RdfPoint, wgs84:alt, Alt^^xsd:float, G).
qb_wgs84_point(M, S, point(Lat,Lng), G) :- !,
  qb_wgs84_point0(M, S, point(Lat,Lng), G, _).


qb_wgs84_point0(M, S, point(Lat,Lng), G, RdfPoint) :-
  qb_bnode(RdfPoint),
  qb(M, S, wgs84:location, RdfPoint, G),
  qb(M, RdfPoint, wgs84:lat, Lat^^xsd:float, G),
  qb(M, RdfPoint, wgs84:long, Lng^^xsd:float, G).



%! qu_wgs84_point(+M1, +M2, +G) is det.

qu_wgs84_point(M1, M2, G) :-
  qu_call((
    q(M1, S, wgs84:lat, Lat^^xsd:float, G),
    q(M1, S, wgs84:long, Lng^^xsd:float, G)
  ), (
    qb_bnode(RdfPoint),
    qb(M2, S, wgs84:location, RdfPoint, G),
    qu(M1, M2, S, wgs84:lat, Lat^^xsd:float, G, subject(RdfPoint)),
    qu(M1, M2, S, wgs84:long, Lng^^xsd:float, G, subject(RdfPoint))
  )).



%! qu_wgs84_wkt(+M1, +M2, +G) is det.

qu_wgs84_wkt(M1, M2, G) :-
  qu_call((
    q_wgs84_point(M1, S, PlPoint, G)
  ), (
    qb_wkt_point(M2, S, PlPoint, G)
  )).
