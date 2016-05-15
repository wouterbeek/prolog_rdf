:- module(
  rdf_datatype,
  [
    rdf_datatype_property/2,  % ?D, ?Property
    rdf_datatype_supremum/2,  % +Datatypes, -Supremum
    rdf_subdatatype_of/2,     % ?Subtype, ?Supertype
    xsd_date_time_datatype/2, % +DT, -D
    xsd_subtype_of/2          % ?Subtype, ?Supertype
  ]
).

/** <module> RDF datatype

@author Wouter Beek
@version 2016/01-2016/02, 2016/04-2016/05
*/

:- use_module(library(error)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(xsdp_types)).

:- rdf_meta
   rdf_datatype_property(r, ?),
   rdf_datatype_supremum(t, t),
   rdf_subdatatype_of(r, r),
   xsd_date_time_datatype(o, r),
   xsd_subtype_of(r, r).





%! rdf_datatype_property(?D, ?Property) is nondet.

rdf_datatype_property(C, total) :-
  rdf_datatype_property(C, antisymmetric),
  rdf_datatype_property(C, comparable),
  rdf_datatype_property(C, reflexive),
  rdf_datatype_property(C, transitive).
rdf_datatype_property(xsd:decimal, antisymmetric).
rdf_datatype_property(xsd:decimal, comparable).
rdf_datatype_property(xsd:decimal, reflexive).
rdf_datatype_property(xsd:decimal, transitive).
rdf_datatype_property(xsd:float,   antisymmetric).
rdf_datatype_property(xsd:float,   reflexive).
rdf_datatype_property(xsd:float,   transitive).
rdf_datatype_property(xsd:gYear,   antisymmetric).
rdf_datatype_property(xsd:gYear,   comparable).
rdf_datatype_property(xsd:gYear,   reflexive).
rdf_datatype_property(xsd:gYear,   transitive).
rdf_datatype_property(xsd:integer, antisymmetric).
rdf_datatype_property(xsd:integer, comparable).
rdf_datatype_property(xsd:integer, reflexive).
rdf_datatype_property(xsd:integer, transitive).



%! rdf_datatype_supremum(+Datatypes:list(iri), -Supremum:iri) is semidet.

rdf_datatype_supremum([H], H) :- !.
rdf_datatype_supremum([H1,H2|T], Sup) :-
  rdf_subdatatype_of(H1, H3),
  rdf_subdatatype_of(H2, H3), !,
  rdf_datatype_supremum([H3|T], Sup).



%! rdf_subdatatype_of(?Subtype:iri, ?Supertype:iri) is nondet.

rdf_subdatatype_of(X, Y) :-
  xsd_subtype_of(X, Y).
rdf_subdatatype_of(X, Y) :-
  rdfs_subclass_of(X, Y),
  \+ xsd_subtype_of(X, Y).



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



%! xsd_subtype_of(?Subtype:iri, ?Supertype:iri) is nondet.

xsd_subtype_of(XGlobal, YGlobal) :-
  xsd_global_local(XGlobal, XLocal),
  xsd_global_local(YGlobal, YLocal),
  xsdp_subtype_of(XLocal, YLocal),
  xsd_global_local(XGlobal, XLocal),
  xsd_global_local(YGlobal, YLocal).

xsd_global_local(X, Y) :- var(X), var(Y), !.
xsd_global_local(X, Y) :- rdf_global_id(xsd:Y, X).
