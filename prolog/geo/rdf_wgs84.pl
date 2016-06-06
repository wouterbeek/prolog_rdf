:- module(
  rdf_wgs84,
  [
    wgs84_alt/2,      % ?Res, ?Alt
    wgs84_alt/3,      % ?Res, ?Alt, ?G
    wgs84_lat/2,      % ?Res, ?Lat
    wgs84_lat/3,      % ?Res, ?Lat, ?G
    wgs84_lat_long/3, % ?Res, ?Lat, ?Long
    wgs84_lat_long/4, % ?Res, ?Lat, ?Long, ?G
    wgs84_long/2,     % ?Res, ?Long
    wgs84_long/3,     % ?Res, ?Long, ?G
    wgs84_point/2,    % ?Res, ?Point
    wgs84_point/3     % ?Res, ?Point, ?G
  ]
).

/** <module> WGS84

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

:- use_module(library(rdf/rdf_ext)).
:- use_module(library(semweb/rdf11)).

:- rdf_register_prefix(wgs84, 'http://www.w3.org/2003/01/geo/wgs84_pos#').

:- dynamic
   gis:resource_shape_hook/3,
   rdf11:in_ground_type_hook/3,
   rdf11:out_type_hook/3.

:- multifile
   gis:resource_shape_hook/3,
   rdf11:in_ground_type_hook/3,
   rdf11:out_type_hook/3.

:- rdf_meta
   wgs84_point(r,?),
   wgs84_point(r,?,?).

gis:resource_shape_hook(Res, Point, G) :-
  wgs84_point(Res, Point, G).

rdf11:in_ground_type_hook(D, Lat-Long, Lex) :-
  rdf_equal(wgs84:pair, D),
  atomic_list_concat([Lat,Long], ',', Lex).

rdf11:out_type_hook(D, Lat-Long, Lex) :-
  rdf_equal(wgs84:pair, D),
  atomic_list_concat(Comps, ',', Lex),
  maplist(atom_number, Comps, [Lat,Long]).





%! wgs84_alt(?Res, ?Alt) is nondet.
%! wgs84_alt(?Res, ?Alt, ?G) is nondet.
%
% Succeeds if Alt is the WGS84 altitude of resource Res.

wgs84_alt(Res, Alt) :-
  wgs84_alt(Res, Alt, _).


wgs84_alt(Res, Alt, G) :-
  rdf_has(Res, wgs84:location, Point, _, G),
  rdf_has(Point, wgs84:alt, Alt^^xsd:float, _, G).



%! wgs84_lat(?Res, ?Lat) is nondet.
%! wgs84_lat(?Res, ?Lat, ?G) is nondet.
%
% Succeeds if Lat is the WGS84 latitude of resource Res.

wgs84_lat(Res, Lat) :-
  wgs84_lat(Res, Lat, _).


wgs84_lat(Res, Lat, G) :-
  rdf_has(Res, wgs84:location, Point, _, G),
  rdf_has(Point, wgs84:lat, Lat^^xsd:float, _, G).



%! wgs84_lat_long(?Res, ?Lat, ?Long) is nondet.
%! wgs84_lat_long(?Res, ?Lat, ?Long, ?G) is nondet.

wgs84_lat_long(Res, Lat, Long) :-
  wgs84_lat_long(Res, Lat, Long, _).


wgs84_lat_long(Res, Lat, Long, G) :-
  rdf_has(Res, wgs84:location, Point, _, G),
  rdf_has(Point, wgs84:lat_long, Lat-Long^^wgs84:pair, _, G).



%! wgs84_long(?Res, ?Long) is nondet.
%! wgs84_long(?Res, ?Long, ?G) is nondet.
%
% Succeeds if Long is the WGS84 longitude of resource Res.

wgs84_long(Res, Long) :-
  wgs84_long(Res, Long, _).


wgs84_long(Res, Long, G) :-
  rdf_has(Res, wgs84:location, Point, _, G),
  rdf_has(Point, wgs84:long, Long^^xsd:float, _, G).



%! wgs84_point(?Res, ?Point) is nondet.
%! wgs84_point(?Res, ?Point, ?G) is nondet.
%
% Succeeds if Point denotes a geo-location of resource Res.
%
% Point is either of the form `point(?Lat,?Long)` or
% `point(?Lat,?Long,?Alt)`.

wgs84_point(Res, Point) :-
  wgs84_point(Res, Point, _).


wgs84_point(Res, Point, G) :-
  rdf_has(Res, wgs84:location, Point, _, G),
  rdf_has(Point, wgs84:lat, Lat^^xsd:float, _, G),
  rdf_has(Point, wgs84:long, Long^^xsd:float, _, G),
  (   rdf_has(Point, wgs84:alt, Alt^^xsd:float, G)
  ->  Point = point(Lat,Long,Alt)
  ;   Point = point(Lat,Long)
  ).
