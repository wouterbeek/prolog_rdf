:- module(
  q_wgs84,
  [
    wgs84_alt/3,      % ?M, ?S, ?Alt
    wgs84_alt/4,      % ?M, ?S, ?Alt, ?G
    wgs84_lat/3,      % ?M, ?S, ?Lat
    wgs84_lat/4,      % ?M, ?S, ?Lat, ?G
    wgs84_lat_long/4, % ?M, ?S, ?Lat, ?Long
    wgs84_lat_long/5, % ?M, ?S, ?Lat, ?Long, ?G
    wgs84_long/3,     % ?M, ?S, ?Long
    wgs84_long/4,     % ?M, ?S, ?Long, ?G
    wgs84_point/3,    % ?M, ?S, ?Point
    wgs84_point/4     % ?M, ?S, ?Point, ?G
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

:- use_module(library(q/q_term)).
:- use_module(library(q/qb)).
:- use_module(library(semweb/rdf11)).

:- qb_alias(wgs84, 'http://www.w3.org/2003/01/geo/wgs84_pos#').

:- multifile
   gis:resource_shape_hook/5,
   rdf11:in_ground_type_hook/3,
   rdf11:out_type_hook/3.

:- rdf_meta
   wgs84_alt(?, r, ?),
   wgs84_alt(?, r, ?, r),
   wgs84_lat(?, r, ?),
   wgs84_lat(?, r, ?, r),
   wgs84_lat_long(?, r, ?, ?),
   wgs84_lat_long(?, r, ?, ?, r),
   wgs84_long(?, r, ?),
   wgs84_long(?, r, ?, r),
   wgs84_point(?, r, ?),
   wgs84_point(?, r, ?, r).

gis:resource_shape_hook(M, S, D, G, Point) :-
  rdf_equal(wkt:point, D),
  wgs84_point(M, S, Point, G).

rdf11:in_ground_type_hook(D, Lat-Long, Lex) :-
  rdf_equal(wgs84:pair, D),
  atomic_list_concat([Lat,Long], ',', Lex).

rdf11:out_type_hook(D, Lat-Long, Lex) :-
  rdf_equal(wgs84:pair, D),
  atomic_list_concat(Comps, ',', Lex),
  maplist(atom_number, Comps, [Lat,Long]).





%! wgs84_alt(?M, ?S, ?Alt) is nondet.
%! wgs84_alt(?M, ?S, ?Alt, ?G) is nondet.
%
% Succeeds if Alt is the WGS84 altitude of resource S.

wgs84_alt(M, S, Alt) :-
  wgs84_alt(M, S, Alt, _).


wgs84_alt(M, S, Alt, G) :-
  q(M, S, wgs84:location, Point, _, G),
  q(M, Point, wgs84:alt, Alt^^xsd:float, _, G).



%! wgs84_lat(?M, ?S, ?Lat) is nondet.
%! wgs84_lat(?M, ?S, ?Lat, ?G) is nondet.
%
% Succeeds if Lat is the WGS84 latitude of resource S.

wgs84_lat(M, S, Lat) :-
  wgs84_lat(M, S, Lat, _).


wgs84_lat(M, S, Lat, G) :-
  q(M, S, wgs84:location, Point, _, G),
  q(M, Point, wgs84:lat, Lat^^xsd:float, _, G).



%! wgs84_lat_long(?M, ?S, ?Lat, ?Long) is nondet.
%! wgs84_lat_long(?M, ?S, ?Lat, ?Long, ?G) is nondet.

wgs84_lat_long(M, S, Lat, Long) :-
  wgs84_lat_long(M, S, Lat, Long, _).


wgs84_lat_long(M, S, Lat, Long, G) :-
  q(M, S, wgs84:location, Point, _, G),
  q(M, Point, wgs84:lat_long, Lat-Long^^wgs84:pair, _, G).



%! wgs84_long(?M, ?S, ?Long) is nondet.
%! wgs84_long(?M, ?S, ?Long, ?G) is nondet.
%
% Succeeds if Long is the WGS84 longitude of resource S.

wgs84_long(M, S, Long) :-
  wgs84_long(M, S, Long, _).


wgs84_long(M, S, Long, G) :-
  q(M, S, wgs84:location, Point, _, G),
  q(M, Point, wgs84:long, Long^^xsd:float, _, G).



%! wgs84_point(?M, ?S, ?Point) is nondet.
%! wgs84_point(?M, ?S, ?Point, ?G) is nondet.
%
% Succeeds if Point denotes a geo-location of resource S.
%
% Point is either of the form `point(?Lat,?Long)` or
% `point(?Lat,?Long,?Alt)`.

wgs84_point(M, S, Point) :-
  wgs84_point(M, S, Point, _).


wgs84_point(M, S, Point, G) :-
  q(M, S, wgs84:location, Point, _, G),
  q(M, Point, wgs84:lat, Lat^^xsd:float, _, G),
  q(M, Point, wgs84:long, Long^^xsd:float, _, G),
  (   q(M, Point, wgs84:alt, Alt^^xsd:float, G)
  ->  Point = point(Lat,Long,Alt)
  ;   Point = point(Lat,Long)
  ).
