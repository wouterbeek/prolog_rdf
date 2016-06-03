:- module(
  wkt,
  [
    wkt//2,    % +Name, +Array
    wkt_name/1 % ?Name
  ]
).

/** <module> Well-Known Text

A **linear ring** is a closed line string of at least four positions,
where the first and the last position are the same (both syntactically
and semantically).

A **polygon** consists of exactly one Exterior linear ring and zero or
more interior linear rings (Interiors).

@author Wouter Beek
@version 2016/05-2016/06
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(semweb/rdf11)).

:- rdf_meta
   wkt(+, +, ?, ?).





'circularstring text'(_, L) --> 'empty set'(L), !.
'circularstring text'(ZM, L) --> "( ", seplist(point(ZM), " ", L), " )".

'circularstring text representation'(ZM1, circularstring(L)) --> "CIRCULARSTRING ", 'z m'(ZM1, ZM2), 'circularstring text'(ZM2, L).

'collection text representation'(ZM, L) --> 'multipoint text representation'(ZM, L), !.
'collection text representation'(ZM, L) --> 'multicurve text representation'(ZM, L), !.
'collection text representation'(ZM, L) --> 'multisurface text representation'(ZM, L), !.
'collection text representation'(ZM, L) --> 'geometrycollection text representation'(ZM, L).

'compoundcurve text'(_, L) --> 'empty set'(L), !.
'compoundcurve text'(ZM, L) --> "( ", seplist('single curve text'(ZM), " ", L), " )".

'compoundcurve text representation'(ZM1, compoundcurve(L)) --> "COMPOUNDCURVE ", 'z m'(ZM1, ZM2), 'compoundcurve text'(ZM2, L).

'curve text'(ZM, L) --> 'linestring text body'(ZM, L), !.
'curve text'(ZM, L) --> 'circularstring text representation'(ZM, L), !.
'curve text'(ZM, L) --> 'compoundcurve text representation'(ZM, L).

'curve text representation'(ZM, L) --> 'linestring text representation'(ZM, L), !.
'curve text representation'(ZM, L) --> 'circularstring text representation'(ZM, L), !.
'curve text representation'(ZM, L) --> 'compoundcurve text representation'(ZM, L).

'curvepolygon text'(_, L) --> 'empty set'(L), !.
'curvepolygon text'(ZM, L) --> "( ", seplist('ring text'(ZM), " ", L), " )".

'curvepolygon text body'(ZM, L) --> 'curvepolygon text'(ZM, L).

'curvepolygon text representation'(ZM1, curvepolygon(L)) --> "CURVEPOLYGON ", 'z m'(ZM1, ZM2), 'curvepolygon text body'(ZM2, L), !.
'curvepolygon text representation'(ZM, L) --> 'polygon text representation'(ZM, L), !.
'curvepolygon text representation'(ZM, L) --> 'triangle text representation'(ZM, L).

'empty set'([]) --> "EMPTY".

'geometrycollection text'(_, L) --> 'empty set'(L), !.
'geometrycollection text'(ZM, L) --> seplist('well-known text representation'(ZM), " ", L), " )".

'geometrycollection text representation'(ZM1, geometrycollection(L)) --> "GEOMETRYCOLLECTION ", 'z m'(ZM1, ZM2), 'geometrycollection text'(ZM2, L).

'linestring text'(_, L) --> 'empty set'(L), !.
'linestring text'(ZM, L) --> "( ", seplist(point(ZM), " ", L), " )".

'linestring text body'(ZM, L) --> 'linestring text'(ZM, L).

'linestring text representation'(ZM1, linestring(L)) --> "LINESTRING ", 'z m'(ZM1, ZM2), 'linestring text body'(ZM2, L).

m(N) --> number(N).

'multicurve text'(_, L) --> 'empty set'(L), !.
'multicurve text'(ZM, L) --> "( ", seplist('curve text'(ZM), " ", L), " )".

'multicurve text representation'(ZM1, multicurve(L)) --> "MULTICURVE ", 'z m'(ZM1, ZM2), 'multicurve text'(ZM2, L), !.
'multicurve text representation'(ZM, L) --> 'multilinestring text representation'(ZM, L).

'multilinestring text'(_, L) --> 'empty set'(L), !.
'multilinestring text'(ZM, L) --> "( ", seplist('linestring text body'(ZM), " ", L), " )".

'multilinestring text representation'(ZM1, multilinestring(L)) --> "MULTILINESTRING ", 'z m'(ZM1, ZM2), 'multilinestring text'(ZM2, L).

'multipoint text'(_, L) --> 'empty set'(L), !.
'multipoint text'(ZM, L) --> "( ", seplist('point text'(ZM), " ", L), " )".

'multipoint text representation'(ZM1, multipoint(L)) --> "MULTIPOINT ", 'z m'(ZM1, ZM2), 'multipoint text'(ZM2, L).

'multipolygon text'(_, L) --> 'empty set'(L), !.
'multipolygon text'(ZM, L) --> "( ", seplist('polygon text body'(ZM), " ", L), " )".

'multipolygon text representation'(ZM1, multipolygon(L)) --> "MULTIPOLYGON ", 'z m'(ZM1, ZM2), 'multipolygon text'(ZM2, L).

'multisurface text'(_, L) --> 'empty set'(L), !.
'multisurface text'(ZM, L) --> "( ", seplist('surface text'(ZM), " ", L), " )".

'multisurface text representation'(ZM1, multisurface(L)) --> "MULTISURFACE ", 'z m'(ZM1, ZM2), 'multisurface text'(ZM2, L), !.
'multisurface text representation'(ZM, L) --> 'multipolygon text representation'(ZM, L), !.
'multisurface text representation'(ZM, L) --> 'polyhedralsurface text representation'(ZM, L), !.
'multisurface text representation'(ZM, L) --> 'tin text representation'(ZM, L).

point(zm, [X,Y,Z,M]) --> point(z, [X,Y,Z]), " ", m(M), !.
point(z, [X,Y,Z]) --> point(none, [X,Y]), " ", z(Z), !.
point(m, [X,Y,M]) --> point(none, [X,Y]), " ", m(M), !.
point(_, [X,Y]) --> x(X), " ", y(Y).

'point text'(_, L) --> 'empty set'(L), !.
'point text'(ZM, L) --> "( ", point(ZM, L), " )".

'point text representation'(ZM1, point(L)) --> "POINT ", 'z m'(ZM1, ZM2), 'point text'(ZM2, L).

'polygon text'(_, L) --> 'empty set'(L), !.
'polygon text'(ZM, L) --> "( ", seplist('linestring text'(ZM), " ", L), " )".

'polygon text body'(ZM, L) --> 'polygon text'(ZM, L).

'polygon text representation'(ZM1, polygon(L)) --> "POLYGON ", 'z m'(ZM1, ZM2), 'polygon text body'(ZM2, L).

'polyhedralsurface text'(_, L) --> 'empty set'(L), !.
'polyhedralsurface text'(ZM, L) --> "( ", seplist('polygon text body'(ZM), " ", L), " )".

'polyhedralsurface text representation'(ZM1, polyhedralsurface(L)) --> "POLYHEDRALSURFACE ", 'z m'(ZM1, ZM2), 'polyhedralsurface text'(ZM2, L).

'ring text'(ZM, L) --> 'linestring text body'(ZM, L), !.
'ring text'(ZM, L) --> 'circularstring text representation'(ZM, L), !.
'ring text'(ZM, L) --> 'compoundcurve text representation'(ZM, L).

'single curve text'(ZM, L) --> 'linestring text body'(ZM, L), !.
'single curve text'(ZM, L) --> 'circularstring text representation'(ZM, L).

'surface text'(ZM, curvepolygon(L)) --> "CURVEPOLYGON", 'curvepolygon text body'(ZM, L), !.
'surface text'(ZM, L) --> 'polygon text body'(ZM, L).

'surface text representation'(ZM, L) --> 'curvepolygon text representation'(ZM, L).

'tin text'(_, L) --> 'empty set'(L), !.
'tin text'(ZM, L) --> "( ", seplist('triangle text body'(ZM), " ", L), " )".

'tin text representation'(ZM1, tin(L)) --> "TIN ", 'z m'(ZM1, ZM2), 'tin text'(ZM2, L).

'triangle text'(_, L) --> 'empty set'(L), !.
'triangle text'(ZM, L) --> "( ", 'linestring text'(ZM, L), " )".

'triangle text body'(ZM, L) --> 'triangle text'(ZM, L).

'triangle text representation'(ZM1, triangle(L)) --> "TRIANGLE ", 'z m'(ZM1, ZM2), 'triangle text body'(ZM2, L).

wkt(Name, Array) -->
  {Comp =.. [Name,Array]},
  'well-known text representation'(_, Comp).

wkt_name(circularstring).
wkt_name(compoundcurve).
wkt_name(curvepolygon).
wkt_name(geometrycollection).
wkt_name(linestring).
wkt_name(multicurve).
wkt_name(multilinestring).
wkt_name(multipoint).
wkt_name(multipolygon).
wkt_name(multisurface).
wkt_name(point).
wkt_name(polygon).
wkt_name(polyhedralsurface).
wkt_name(tin).
wkt_name(triangle).

'well-known text representation'(ZM, L) --> 'point text representation'(ZM, L), !.
'well-known text representation'(ZM, L) --> 'curve text representation'(ZM, L), !.
'well-known text representation'(ZM, L) --> 'surface text representation'(ZM, L), !.
'well-known text representation'(ZM, L) --> 'collection text representation'(ZM, L).

x(N) --> number(N).

y(N) --> number(N).

z(N) --> number(N).

'z m'(ZM, ZM) --> "".
'z m'(_, zm) --> "ZM ".
'z m'(_, z) --> "Z ".
'z m'(_, m) --> "M ".
