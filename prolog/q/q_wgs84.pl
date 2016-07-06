:- module(
  q_wgs84,
  [
    q_wgs84_point/4,                 % ?M, ?S, ?Point, ?G
    qb_wgs84_point/4,                % +M, +S, +Point, +G
    qu_replace_flat_wgs84_point/3,   % +M1, +M2, +G
    qu_replace_flat_wgs84_point/5,   % +M1, +M2, +Q, +R, +G
    qu_replace_nested_wgs84_point/3, % +M1, +M2, +G
    qu_replace_nested_wgs84_point/6  % +M1, +M2, +P, +Q, +R, +G
  ]
).

/** <module> Quine WGS84 support

We write geo-coordinates in the following order: 〈lng,lat〉.

```
rdfs:Resource
    ---[wgs84:location]--> wgs84:SpatialThing
        ---[wgs84:long]--> xsd:float
        ---[wgs84:lat]--> xsd:float

wgs84:lat_long (comma-separated)

wgs84:Point IS-A wgs84:SpatialThing
```

---

@author Wouter Beek
@see https://www.w3.org/2003/01/geo/wgs84_pos
@tbd Add altitude support.
@tbd Add support for lat-lng pair notation.
@version 2016/06-2016/07
*/

:- use_module(library(debug)).
:- use_module(library(q/q_stmt)).
:- use_module(library(q/q_term)).
:- use_module(library(q/q_wkt)).
:- use_module(library(q/qb)).
:- use_module(library(q/qu)).
:- use_module(library(semweb/rdf11)).

:- qb_alias(wkt, 'http://geojsonld.com/wkt#').
:- qb_alias(wgs84, 'http://www.w3.org/2003/01/geo/wgs84_pos#').

:- multifile
    %%%%gis:gis_shape_hook/5,
    %%%%gis:subject_to_geometry/5,
    rdf11:in_ground_type_hook/3,
    rdf11:out_type_hook/3.

:- rdf_meta
   q_wgs84_point(?, r, ?, r),
   qb_wgs84_point(+, r, +, r),
   qu_replace_flat_wgs84_point(+, +, r, r, r),
   qu_replace_nested_wgs84_point(+, +, r, r, r, r).


%%%%gis:gis_shape_hook(M, S, D, G, Point) :-
%%%%  rdf_equal(wkt:point, D),
%%%%  q_wgs84_point(M, S, Point, G).


%%%%gis:subject_to_geometry(M, S, [Lng,Lat], point, G) :-
%%%%  q_wgs84_point(M, S, point(Lng,Lat), G).


rdf11:in_ground_type_hook(D, Lng-Lat, Lex) :-
  rdf_equal(wgs84:pair, D),
  atomic_list_concat([Lng,Lat], ',', Lex).


rdf11:out_type_hook(D, Lng-Lat, Lex) :-
  rdf_equal(wgs84:pair, D),
  atomic_list_concat(Comps, ',', Lex),
  maplist(atom_number, Comps, [Lng,Lat]).





%! q_wgs84_point(?M, ?S, ?Point, ?G) is nondet.
%
% Succeeds if Point denotes a geo-location of resource S.
%
% Point is of the form `point(?Lng,?Lat)`.

q_wgs84_point(M, S, point(Lng,Lat), G) :-
  q(M, S, wgs84:location, B, G),
  q(M, B, wgs84:long, Lng^^xsd:float, G),
  q(M, B, wgs84:lat, Lat^^xsd:float, G).
%q_wgs84_point(M, S, point(Lng,Lat), G) :-
%  q(M, S, wgs84:location, B, G),
%  q(M, B, wgs84:lat_long, Lat-Lng^^wgs84:pair, G).



%! qb_wgs84_point(+M, +S, +Point, +G) is det.

qb_wgs84_point(M, S, point(Lng,Lat), G) :- !,
  qb_bnode(B),
  qb(M, S, wgs84:location, B, G),
  qb(M, B, wgs84:long, Lng^^xsd:float, G),
  qb(M, B, wgs84:lat, Lat^^xsd:float, G).



%! qu_replace_flat_wgs84_point(+M1, +M2, +G) is det.
%! qu_replace_flat_wgs84_point(+M1, +M2, +Q, +R, +G) is det.
%
% From flat WGS84 point notation to Triply point notation.

qu_replace_flat_wgs84_point(M1, M2, G) :-
  qu_replace_flat_wgs84_point(M1, M2, wgs84:long, wgs84:lat, G).


qu_replace_flat_wgs84_point(M1, M2, Q, R, G) :-
  debug(qu(wgs84), "Triplyfy flat WGS84 point notation.", []),
  qu_call((
    q(M1, S, Q, Lng^^xsd:float, G),
    q(M1, S, R, Lat^^xsd:float, G)
  ), (
    qb_wkt_point(M2, S, point(Lng,Lat), G),
    qb_rm(M1, S, Q, Lng^^xsd:float, G),
    qb_rm(M1, S, R, Lat^^xsd:float, G)
  )).



%! qu_replace_nested_wgs84_point(+M1, +M2, +P, +Q, +R, +G) is det.
%
% From proper WGS84 point notation to Triply point notation.

qu_replace_nested_wgs84_point(M1, M2, G) :-
  qu_replace_nested_wgs84_point(M1, M2, wgs84:location, wgs84:long, wgs84:lat, G).


qu_replace_nested_wgs84_point(M1, M2, P, Q, R, G) :-
  debug(qu(wgs84), "Change WGS84 points to WKT points.", []),
  qu_call((
    q(M1, S, P, B, G),
    % Without the blank node check an already converted literal may
    % appear in the subject position, resulting in an exception.
    q_is_bnode(B),
    q(M1, B, Q, Lng^^xsd:float, G),
    q(M1, B, R, Lat^^xsd:float, G)
  ), (
    qb_wkt_point(M2, S, point(Lng,Lat), G),
    qb_rm(M1, S, P, B, G),
    qb_rm(M1, B, R, Lng^^xsd:float, G),
    qb_rm(M1, B, Q, Lat^^xsd:float, G)
  )).
