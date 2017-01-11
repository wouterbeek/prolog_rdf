:- module(
  gis,
  [
    gis_bearing/3,          % +Point1, +Point2, -Heading
    gis_contains/2,         % +Shape1, -Shape2
    gis_contains/3,         % +Shape1, -Shape2, +G
    gis_dist/3,             % +Point1, +Point2, -Dist
    gis_dist/4,             % +Point1, +Point2, -Dist, +G
    gis_exists/0,
    gis_graph/1,            % ?G
    gis_intersects/2,       % +Shape1, -Shape2
    gis_intersects/3,       % +Shape1, -Shape2, +G
    gis_nearest/2,          % +Shape1, -Shape2
    gis_nearest/3,          % +Shape1, -Shape2, +G
    gis_nearest_bounded/3,  % +Shape1, +Range, -Shape2
    gis_nearest_bounded/4,  % +Shape1, +Range, -Shape2, +G
    q_view_graph_with_gis/2 % ?M, ?G
  ]
).

/** <module> Geographic Information System (GIS)

@author Willem van Hage
@author Jan Wielemaker
@version 2009-2012

@author Wouter Beek
@version 2016/06-2016/12
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug_ext)).
:- use_module(library(error)).
:- use_module(library(gis/gis)).
:- use_module(library(q/q_io)).
:- use_module(library(q/q_wgs84), []).
:- use_module(library(q/q_wkt), []).
:- use_module(library(semweb/rdf11)).
:- use_module(library(shlib)).

:- use_foreign_library(foreign(space)).


:- dynamic
    gis_current_graph0/1,
    gis_graph0/2. % ?G, ?Dummy


:- multifile
    q_io:q_backend_hook/1,
    q_io:q_store2view_hook/2,
    q_io:q_view_graph_hook/3,
    q_io:q_view_rm_hook/2,
    gis:gis_shape_hook/5.


:- rdf_meta
   gis_contains(+, -, r),
   gis_dist(+, +, -, r),
   gis_intersects(+, -, r),
   gis_nearest(+, -, r),
   gis_nearest_bounded(+, +, -, r).


q_io:q_backend_hook(gis).


% The GIS view uses the RDF cache (i.e., TRP).
q_io:q_store2view_hook(gis, G) :-
  % Also creates the ‘trp’ view.
  q_store2cache(trp, G),
  q_cache2view(trp, G),
  (   q_view_graph_with_gis(trp, G)
  ->  dcg_with_output_to(string(Msg), deb_q_io("TRP", G, "GIS")),
      indent_debug_call(
        q_io(cache2view(gis)),
        Msg,
        with_mutex(gis_db,
          setup_call_cleanup(
            assert(gis_current_graph0(G)),
            (
              rtree_bulkload(G, uri_shape, 2),
              assert(gis_graph0(G,false))
            ),
            retract(gis_current_graph0(G))
          )
        )
      )
  ;   assert(gis_graph0(G,true))
  ).

system:uri_shape(S, Shape) :-
  gis_current_graph0(G),
  gis:gis_shape_hook(trp, S, _, G, Shape),
  debug(gis(db), "[ADDED] ~w ~w", [S,Shape]).


q_io:q_view_graph_hook(gis, G, Dummy) :-
  gis_graph0(G, Dummy).


q_io:q_view_rm_hook(gis, G) :-
  retract(gis_graph0(G,_)),
  rtree_clear(G).





%! gis_bearing(+Point1, +Point2, -Bearing) is det.
%
% Bearing is expressed in degrees.

gis_bearing(point(Lat1deg,Lng1deg), point(Lat2deg,Lng2deg), Bearing) :-
  deg2rad0(Lat1deg, Lat1),
  deg2rad0(Lat2deg, Lat2),
  deg2rad0(Lng1deg, Lng1),
  deg2rad0(Lng2deg, Lng2),
  DistLng is Lng2 - Lng1,
  Y is sin(DistLng) * cos(Lat2),
  X is cos(Lat1) * sin(Lat2) - sin(Lat1) * cos(Lat2) * cos(DistLng),
  Bearing0 is atan(Y, X),
  rad2deg0(Bearing0, Bearing).


deg2rad0(Deg, Rad) :-
  Rad is (Deg * pi) / 180.


rad2deg0(Rad, Deg) :-
  Deg is (Rad * 180) / pi.



%! gis_contains(+Shape1, -Shape2) is nondet.
%! gis_contains(+Shape1, -Shape2, +G) is nondet.
%
% Containment query, non-deterministically unifying Shape to shapes
% contained within the shape or resource Shape1.

gis_contains(Shape1, Shape2) :-
  gis_graph(G),
  gis_contains(Shape1, Shape2, G).


gis_contains(Shape1, Shape2, G) :-
  rtree_incremental_containment_query(Shape1, Shape2, G).



%! gis_dist(+Point1, +Point2, -Dist) is det.
%! gis_dist(+Point1, +Point2, -Dist, +G) is det.
%
% Calculates the Pythagorian distance Dist between Point1 and Point2.

gis_dist(Point1, Point2, Dist) :-
  gis_graph(G),
  gis_dist(Point1, Point2, G, Dist).


gis_dist(Point, Point, 0.0, _) :- !.
gis_dist(point(Lat1,Lng1), point(Lat2,Lng2), Dist2, _) :- !,
  Dist1 is sqrt(((Lat2 - Lat1) ** 2) + ((Lng2 - Lng1) ** 2)),
  lat_lng_to_kms0(Dist1, Dist2).
gis_dist(Point1, Point2, Dist2, G) :-
  rtree_distance(G, Point1, Point2, Dist1),
  lat_lng_to_kms0(Dist1, Dist2).


lat_lng_to_kms0(Dist1, Dist2) :-
  Dist2 is Dist1 * 111.195083724.



%! gis_exists is semidet.
%
% Succeeds if some GIS is currently loaded.

gis_exists :-
  once(gis_graph(_)).



%! gis_graph(?G) is nondet.

gis_graph(G) :-
  % @tbd HACK
  distinct(G, gis_graph0(G, false)).



%! gis_intersects(+Shape1, -Shape2) is nondet.
%! gis_intersects(+Shape1, -Shape2, +G) is nondet.
%
% Intersection query, unifying Inter with shapes that intersect with
% the shape or resource Shape1.
%
% Intersection subsumes containment.

gis_intersects(Shape1, Shape2) :-
  gis_graph(G),
  gis_intersects(Shape1, Shape2, G).


gis_intersects(Shape1, Shape2, G) :-
  rtree_incremental_intersection_query(Shape1, Shape2, G).



%! gis_nearest(+Shape1, -Shape2) is nondet.
%! gis_nearest(+Shape1, -Shape2, +G) is nondet.
%
% Incremental Nearest-Neighbor query, unifying Shape with shapes in
% order of increasing distance to the shape or resource Shape1.

gis_nearest(Shape1, Shape2) :-
  gis_graph(G),
  gis_nearest(Shape1, Shape2, G).


gis_nearest(Shape1, Shape2, G) :-
  rtree_incremental_nearest_neighbor_query(Shape1, Shape2, G).



%! gis_nearest_bounded(+Shape1, +Range, -Shape2) is nondet.
%! gis_nearest_bounded(+Shape1, +Range, -Shape2, +G) is nondet.
%
% Incremental Nearest Neighbor query with a bounded distance scope.
% Unifies Near with shapes in order of increasing distance to shape or
% resource Shape1.
%
% Fails when no more objects are within the given Range.

gis_nearest_bounded(Shape1, Range, Shape2) :-
  gis_graph(G),
  gis_nearest_bounded(Shape1, Range, Shape2, G).


gis_nearest_bounded(Shape1, Range, Shape2, G) :-
  (   ground(Shape2)
  ->  true
  ;   rtree_incremental_nearest_neighbor_query(Shape1, Shape2, G)
  ),
  gis_dist(Shape1, Shape2, Dist, G),
  (ground(Range) -> (Dist > Range -> !, fail ; true) ; Range = Dist).



%! q_view_graph_with_gis(?M, ?G) is nondet.
%
% Graph G as stored in backend M contains some geo data.

q_view_graph_with_gis(M, G) :-
  q_view_graph(M, G),
  once(gis:gis_shape_hook(M, _, _, G, _)).
