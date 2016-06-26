:- module(
  z_datatype,
  [
    z_col_datatype/2,          % +P, -D
    z_col_datatype/3,          % +P, ?G, -D
    z_datatype_compat/2,       % +Lex, -D
    z_datatype_compat_min/2,   % +Lex, -D
    z_datatypes_compat/2,      % +P, -Ds
    z_datatypes_compat/3,      % +P, ?G, -Ds
    z_datatype_property/2,     % ?D, ?Property
    z_datatype_supremum/2,     % +Datatypes, -Supremum
    z_strict_subdatatype_of/2, % ?Subtype, ?Supertype
    z_subdatatype_of/2,        % ?Subtype, ?Supertype
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
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(sgml)).
:- use_module(library(xml/xml_dom)).
:- use_module(library(xsdp_types)).
:- use_module(library(z/z_ext)).
:- use_module(library(z/z_stmt)).
:- use_module(library(z/z_term)).

:- rdf_meta
   z_col_datatype(r, -),
   z_col_datatype(r, r, -),
   z_datatype_compat(+, r),
   z_datatype_compat_min(+, r),
   z_datatype_compats(r, -),
   z_datatype_compats(r, r, -),
   z_datatype_property(r, ?),
   z_datatype_supremum(t, t),
   z_strict_subdatatype_of(r, r),
   z_subdatatype_of(r, r),
   xsd_date_time_datatype(o, r),
   xsd_strict_subtype_of(r, r),
   xsd_subtype_of(r, r).





%! z_col_datatype(+P, -D) is semidet.
%! z_col_datatype(+P, ?G, -D) is nondet.

z_col_datatype(P, D) :-
  z_col_datatype(P, _, D).


z_col_datatype(P, G, D) :-
  z(_, P, O, G),
  z_literal_datatype(O, D), !,
  forall(z(_, P, O0, G), z_literal_datatype(O0, D)).



%! z_datatype_compat(+Lex, -D) is det.

z_datatype_compat(Lex, D) :-
  catch((
    rdf11:xsd_date_time_type(D),
    xsd_time_string(_, D, Lex)
  ), _, false).
z_datatype_compat(Lex, D) :-
  catch(xsd_number_string(N, Lex), _, false),
  rdf11:xsd_numerical(D, Dom, _),
  is_of_type(Dom, N).
z_datatype_compat(Lex, rdf:'HTML') :-
  string_to_atom(Lex, Lex0),
  call_collect_messages(atom_to_html_dom(Lex0, Dom), _, _),
  memberchk(element(_,_,_), Dom).
z_datatype_compat(Lex, rdf:'XMLLiteral') :-
  string_to_atom(Lex, Lex0),
  call_collect_messages(atom_to_xml_dom(Lex0, Dom), _, _),
  memberchk(element(_,_,_), Dom).
z_datatype_compat(Lex, xsd:boolean) :-
  rdf11:in_boolean(Lex, _).
z_datatype_compat(Lex, xsd:boolean) :-
  string(Lex),
  number_string(N, Lex),
  rdf11:in_boolean(N, _).
z_datatype_compat(_, xsd:string).



%! z_datatypes_compat(+P, +Ds) is det.
%! z_datatypes_compat(+P, ?G, +Ds) is det.
%
% @tbd Can be optimized when the compatible datatypes of the previous
% lexical form act as a filter on the datatypes that need to be
% checked for the next lexical form.

z_datatypes_compat(P, Ds) :-
  z_datatypes_compat(P, _, Ds).


z_datatypes_compat(P, G, Ds) :-
  z_aggregate_all(set(Lex), lexical_form0(P, Lex, G), Lexs),
  maplist(z_datatypes_compat0, Lexs, Dss),
  (Dss == [] -> Ds = [] ; ord_intersection(Dss, Ds)).


lexical_form0(P, Lex, G) :-
  z(_, P, Lit, G),
  rdf_is_literal(Lit),
  z_literal_lex(Lit, Lex).


z_datatypes_compat0(Lex, Ds) :-
  aggregate_all(set(D), z_datatype_compat(Lex, D), Ds).



%! z_datatype_compat_min(+Lex, -D) is nondet.

z_datatype_compat_min(Lex, D1) :-
  z_datatype_compat(Lex, D1),
  \+ (
    z_datatype_compat(Lex, D2),
    z_strict_subdatatype_of(D2, D1)
  ).



%! z_datatype_property(?D, ?Property) is nondet.

z_datatype_property(C, total) :-
  z_datatype_property(C, antisymmetric),
  z_datatype_property(C, comparable),
  z_datatype_property(C, reflexive),
  z_datatype_property(C, transitive).
z_datatype_property(xsd:decimal, antisymmetric).
z_datatype_property(xsd:decimal, comparable).
z_datatype_property(xsd:decimal, reflexive).
z_datatype_property(xsd:decimal, transitive).
z_datatype_property(xsd:float,   antisymmetric).
z_datatype_property(xsd:float,   reflexive).
z_datatype_property(xsd:float,   transitive).
z_datatype_property(xsd:gYear,   antisymmetric).
z_datatype_property(xsd:gYear,   comparable).
z_datatype_property(xsd:gYear,   reflexive).
z_datatype_property(xsd:gYear,   transitive).
z_datatype_property(xsd:integer, antisymmetric).
z_datatype_property(xsd:integer, comparable).
z_datatype_property(xsd:integer, reflexive).
z_datatype_property(xsd:integer, transitive).



%! z_datatype_supremum(+Datatypes, -Supremum) is semidet.
%
% The Supremum is the smallest datatype that covers all given
% Datatypes.

z_datatype_supremum([H], H) :- !.
z_datatype_supremum([H1,H2|T], Sup) :-
  z_subdatatype_of(H1, H3),
  z_subdatatype_of(H2, H3), !,
  z_datatype_supremum([H3|T], Sup).



%! z_strict_subdatatype_of(?Subtype:iri, ?Supertype:iri) is nondet.

z_strict_subdatatype_of(X, Y) :-
  dif(X, Y),
  z_subdatatype_of(X, Y).



%! z_subdatatype_of(?Subtype:iri, ?Supertype:iri) is nondet.

z_subdatatype_of(X, Y) :-
  xsd_subtype_of(X, Y).
z_subdatatype_of(X, Y) :-
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
