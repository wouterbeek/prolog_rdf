:- module(
  rdf_datatype,
  [
    rdf_datatype_supremum/2, % +Datatypes, -Supremum
    rdf_subdatatype_of/2,    % ?Subtype, ?Supertype
    xsd_subtype_of/2         % ?Subtype, ?Supertype
  ]
).

/** <module> RDF datatype

@author Wouter Beek
@version 2016/01
*/

:- use_module(library(error)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(xsdp_types)).

:- rdf_meta
   rdf_datatype_supremum(t, t),
   rdf_subdatatype_of(r, r),
   xsd_subtype_of(r, r).





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


%! xsd_subtype_of(?Subtype:iri, ?Supertype:iri) is nondet.

xsd_subtype_of(XGlobal, YGlobal) :-
  maplist(xsd_global_local, [XGlobal,YGlobal], [XLocal,YLocal]),
  xsdp_subtype_of(XLocal, YLocal),
  maplist(xsd_global_local, [XGlobal,YGlobal], [XLocal,YLocal]).

xsd_global_local(X, X) :- var(X), !.
xsd_global_local(X, Y) :- rdf_global_id(xsd:Y, X).
