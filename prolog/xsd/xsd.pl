:- module(
  xsd,
  [
    xsd_subtype_of/2 % ?Subtype, ?Supertype
  ]
).

/** <module> XML Schema 1.1 Datatypes

@author Wouter Beek
@version 2017/08
*/

:- use_module(library(semweb/rdf11)).
:- use_module(library(xsd/xsd_number)).
:- use_module(library(xsdp_types)).

:- arithmetic_function(xsd_div/2).

:- op(400, yfx, xsd_div).

% xsd_div(+M, +N, -Z) is det.
%
% # Definition
%
% If `M` and `N` are numbers, then `M div N` is the greatest integer
% less than or equal to `M / N`.
%
% @tbd Import from `library(xsd/xsd_number)'.

xsd_div(X, Y, Z):-
  Z is floor(X rdiv Y).

:- rdf_meta
   xsd_subtype_of(r, r).





%! xsd_subtype_of(?Subtype:atom, ?Supertype:atom) is nondet.

xsd_subtype_of(SubGlobal, SuperGlobal) :-
  xsd_global_local_(SubGlobal, SubLocal),
  xsd_global_local_(SuperGlobal, SuperLocal),
  xsdp_subtype_of(SubLocal, SuperLocal),
  xsd_global_local_(SubGlobal, SubLocal),
  xsd_global_local_(SuperGlobal, SuperLocal).

xsd_global_local_(Global, Local) :-
  var(Global),
  var(Local), !.
xsd_global_local_(Global, Local) :-
  rdf_global_id(xsd:Local, Global).
