:- module(
  rdf_datatype,
  [
    rdf_datatype/2,              % +P, -D
    rdf_datatype/3,              % +P, ?G, -D
    rdf_datatype_compat/2,       % +Lex, -D
    rdf_datatype_compat_min/2,   % +Lex, -D
    rdf_datatypes_compat/2,      % +P, -Ds
    rdf_datatypes_compat/3,      % +P, ?G, -Ds
    rdf_datatype_property/2,     % ?D, ?Property
    rdf_datatype_supremum/2,     % +Datatypes, -Supremum
    rdf_strict_subdatatype_of/2, % ?Subtype, ?Supertype
    rdf_subdatatype_of/2,        % ?Subtype, ?Supertype
    xsd_date_time_datatype/2,    % +DT, -D
    xsd_strict_subtype_of/2,     % ?Subtype, ?Supertype
    xsd_subtype_of/2             % ?Subtype, ?Supertype
  ]
).

/** <module> RDF datatype

@author Wouter Beek
@version 2016/01-2016/02, 2016/04-2016/06
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(debug_ext)).
:- use_module(library(dif)).
:- use_module(library(error)).
:- use_module(library(html/html_dom)).
:- use_module(library(ordsets)).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(semweb/rdf_db), []).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(sgml)).
:- use_module(library(xml/xml_dom)).
:- use_module(library(xsdp_types)).

:- rdf_meta
   rdf_datatype(r, -),
   rdf_datatype(r, r, -),
   rdf_datatype_compat(+, r),
   rdf_datatype_compat_min(+, r),
   rdf_datatype_compats(r, -),
   rdf_datatype_compats(r, r, -),
   rdf_datatype_property(r, ?),
   rdf_datatype_supremum(t, t),
   rdf_strict_subdatatype_of(r, r),
   rdf_subdatatype_of(r, r),
   xsd_date_time_datatype(o, r),
   xsd_strict_subtype_of(r, r),
   xsd_subtype_of(r, r).





%! rdf_datatype(+P, -D) is semidet.
%! rdf_datatype(+P, ?G, -D) is nondet.

rdf_datatype(P, D) :-
  rdf(_, P, O),
  rdf_literal_datatype(O, D), !,
  forall(rdf(_, P, O0), rdf_literal_datatype(O0, D)).


rdf_datatype(P, G, D) :-
  rdf(_, P, O, G),
  rdf_literal_datatype(O, D), !,
  forall(rdf(_, P, O0, G), rdf_literal_datatype(O0, D)).



%! rdf_datatype_compat(+Lex, -D) is det.

rdf_datatype_compat(Lex, D) :-
  catch(xsd_time_string(_, D, Lex), _, false).
rdf_datatype_compat(Lex, D) :-
  catch(xsd_number_string(N, Lex), _, false),
  rdf11:xsd_numerical(D, Dom, _),
  is_of_type(Dom, N).
rdf_datatype_compat(Lex, rdf:'HTML') :-
  string_to_atom(Lex, Lex0),
  call_collect_messages(atom_to_html_dom(Lex0, Dom), _, _),
  memberchk(element(_,_,_), Dom).
rdf_datatype_compat(Lex, rdf:'XMLLiteral') :-
  string_to_atom(Lex, Lex0),
  call_collect_messages(atom_to_xml_dom(Lex0, Dom), _, _),
  memberchk(element(_,_,_), Dom).
rdf_datatype_compat(Lex, xsd:boolean) :-
  rdf11:in_boolean(Lex, _).
rdf_datatype_compat(Lex, xsd:boolean) :-
  string(Lex),
  number_string(N, Lex),
  rdf11:in_boolean(N, _).
rdf_datatype_compat(_, xsd:string).



%! rdf_datatypes_compat(+P, +Ds) is det.
%! rdf_datatypes_compat(+P, ?G, +Ds) is det.

rdf_datatypes_compat(P, Ds) :-
  rdf_datatypes_compat(P, _, Ds).


rdf_datatypes_compat(P, G, Ds) :-
  rdf_aggregate_all(set(Lex), rdf_db:rdf(_, P, literal(type(_,Lex)), G), Lexs),
  maplist(rdf_datatypes_compat0, Lexs, Dss),
  (Dss == [] -> Ds = [] ; ord_intersection(Dss, Ds)).

rdf_datatypes_compat0(Lex, Ds) :-
  aggregate_all(set(D), rdf_datatype_compat(Lex, D), Ds).



%! rdf_datatype_compat_min(+Lex, -D) is nondet.

rdf_datatype_compat_min(Lex, D1) :-
  rdf_datatype_compat(Lex, D1),
  \+ (
    rdf_datatype_compat(Lex, D2),
    rdf_strict_subdatatype_of(D2, D1)
  ).



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



%! rdf_datatype_supremum(+Datatypes, -Supremum) is semidet.
%
% The Supremum is the smallest datatype that covers all given
% Datatypes.

rdf_datatype_supremum([H], H) :- !.
rdf_datatype_supremum([H1,H2|T], Sup) :-
  rdf_subdatatype_of(H1, H3),
  rdf_subdatatype_of(H2, H3), !,
  rdf_datatype_supremum([H3|T], Sup).



%! rdf_strict_subdatatype_of(?Subtype:iri, ?Supertype:iri) is nondet.

rdf_strict_subdatatype_of(X, Y) :-
  dif(X, Y),
  rdf_subdatatype_of(X, Y).



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



%! xsd_strict_subtype_of(?Subtype:iri, ?Supertype:iri) is nondet.

xsd_strict_subtype_of(X, Y) :-
  dif(X, Y),
  xsd_subtype_of(X, Y).



%! xsd_subtype_of(?Subtype:iri, ?Supertype:iri) is nondet.

xsd_subtype_of(XGlobal, YGlobal) :-
  xsd_global_local(XGlobal, XLocal),
  xsd_global_local(YGlobal, YLocal),
  xsdp_subtype_of(XLocal, YLocal),
  xsd_global_local(XGlobal, XLocal),
  xsd_global_local(YGlobal, YLocal).

xsd_global_local(X, Y) :- var(X), var(Y), !.
xsd_global_local(X, Y) :- rdf_global_id(xsd:Y, X).
