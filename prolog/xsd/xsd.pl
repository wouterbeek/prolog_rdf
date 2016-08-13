:- module(
  xsd,
  [
    xsd_date_time_datatype/2, % +DT, -D
    xsd_div/3,                % +X, +Y, -Z
    xsd_mod/3,                % +X, +Y, -Z
    xsd_strict_subtype_of/2,  % ?Subtype, ?Supertype
    xsd_subtype_of/2          % ?Subtype, ?Supertype
  ]
).

/** <module> XML Schema Datatypes (XSD)

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(dif)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(xsdp_types)).

:- rdf_meta
   xsd_date_time_datatype(o, r),
   xsd_strict_subtype_of(r, r),
   xsd_subtype_of(r, r).





%! xsd_date_time_datatype(+DT, -D) is det.

xsd_date_time_datatype(date_time(Y,Mo,Da,H,Mi,S,_), D) :-
  (   % xsd:dateTime
      ground(date(Y,Mo,Da,H,Mi,S))
  ->  rdf_equal(xsd:dateTime, D)
  ;   % xsd:date
      ground(date(Y,Mo,Da))
  ->  rdf_equal(xsd:date, D)
  ;   % xsd:time
      ground(date(H,Mi,S))
  ->  rdf_equal(xsd:time, D)
  ;   % xsd:gMonthDay
      ground(date(Mo,Da))
  ->  rdf_equal(xsd:gMonthDay, D)
  ;   % xsd:gYearMonth
      ground(date(Y,Mo))
  ->  rdf_equal(xsd:gYearMonth, D)
  ;   % xsd:gMonth
      ground(date(Mo))
  ->  rdf_equal(xsd:gMonth, D)
  ;   % xsd:gYear
      ground(date(Y))
  ->  rdf_equal(xsd:gYear, D)
  ).



%! xsd_strict_subtype_of(?Subtype, ?Supertype) is nondet.

xsd_strict_subtype_of(X, Y) :-
  dif(X, Y),
  xsd_subtype_of(X, Y).



%! xsd_subtype_of(?Subtype, ?Supertype) is nondet.

xsd_subtype_of(XGlobal, YGlobal) :-
  xsd_global_local0(XGlobal, XLocal),
  xsd_global_local0(YGlobal, YLocal),
  xsdp_subtype_of(XLocal, YLocal),
  xsd_global_local0(XGlobal, XLocal),
  xsd_global_local0(YGlobal, YLocal).


xsd_global_local0(X, Y) :-
  var(X),
  var(Y), !.
xsd_global_local0(X, Y) :-
  rdf_global_id(xsd:Y, X).
