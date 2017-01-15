:- module(
  rdf_datatype,
  [
    rdf_datatype_check/2,           % +D, +Val
    rdf_datatype_compat/2,          % +Lex, -D
    rdf_datatype_compat/3,          % ?M, +P, -D
    rdf_datatype_compat/4,          % ?M, +P, ?G, -D
    rdf_datatype_compat_supremum/2, % +Lex, -D
    rdf_datatypes_supremum/2,       % +Ds, -D
    rdf_strict_subdatatype_of/2,    % ?SubD, ?SupD
    rdf_subdatatype_of/2,           % ?SubD, ?SupD
    rdf_value_space_property/2      % ?D, ?Prop
  ]
).

/** <module> RDF datatype

@author Wouter Beek
@version 2016/06, 2016/09, 2016/11
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(debug_ext)).
:- use_module(library(error)).
:- use_module(library(html/html_dom)).
:- use_module(library(ordsets)).
:- use_module(library(rdf/rdfs_api)).
:- use_module(library(semweb/rdf11), []).
:- use_module(library(sgml)).
:- use_module(library(xml/xml_dom)).
:- use_module(library(xsd/xsd)).

:- rdf_meta
   rdf_datatype_check(r, +),
   rdf_datatype_compat(+, r),
   rdf_datatype_compat_supremum(t, t),
   rdf_datatype_property(r, ?),
   rdf_datatypes_compat(?, r, -),
   rdf_datatypes_compat(?, r, r, -),
   rdf_strict_subdatatype_of(r, r),
   rdf_subdatatype_of(r, r).





%! rdf_datatype_check(+D, +Val) is semidet.
%
% Throws an exception if value Val does not belong to the value space
% of the datatype denoted by datatype IRI D.

rdf_datatype_check(D, Val) :-
  rdf11:in_ground_type(D, Val, _).



%! rdf_datatype_compat(+Lex, -D) is det.
%
% Enumerates the datatypes that contain Lex in their lexical space.

% xsd:date
% xsd:dateTime
% xsd:gDay
% xsd:gMonth
% xsd:gMonthDay
% xsd:gYear
% xsd:gYearMonth
% xsd:time
rdf_datatype_compat(Lex, D) :-
  catch((
    rdf11:xsd_date_time_type(D),
    xsd_time_string(_, D, Lex)
  ), _, false).

rdf_datatype_compat(Lex, D) :-
  catch(xsd_number_string(N, Lex), _, false),
  rdf11:xsd_numerical(D, Dom, _),
  is_of_type(Dom, N).
% rdf:HTML
rdf_datatype_compat(Lex, rdf:'HTML') :-
  string_to_atom(Lex, Lex0),
  call_collect_messages(atom_to_html_dom(Lex0, Dom), _, _),
  memberchk(element(_,_,_), Dom).
% rdf:XMLLiteral
rdf_datatype_compat(Lex, rdf:'XMLLiteral') :-
  string_to_atom(Lex, Lex0),
  call_collect_messages(atom_to_xml_dom(Lex0, Dom), _, _),
  memberchk(element(_,_,_), Dom).
% rdf:langString
rdf_datatype_compat(_, rdf:langString).
% xsd:boolean
rdf_datatype_compat(Lex, xsd:boolean) :-
  rdf11:in_boolean(Lex, _).
rdf_datatype_compat(Lex, xsd:boolean) :-
  string(Lex),
  number_string(N, Lex),
  rdf11:in_boolean(N, _).
% xsd:string
rdf_datatype_compat(_, xsd:string).


%! rdf_datatype_compat(?M, +P, -D) is nondet.
%! rdf_datatype_compat(?M, +P, ?G, -D) is nondet.
%
% The datatypes that are compatible with the lexical forms for
% predicate P.

rdf_datatype_compat(M, P, D) :-
  rdf_datatype_compat(M, P, _, D).


rdf_datatype_compat(M, P, G, D) :-
  t(M, _, P, O, G),
  rdf_literal_lexical_form(O, Lex),
  rdf_datatype_compat(Lex, D),
  forall(
    t(M, _, P, O0, G),
    (
      rdf_literal_lexical_form(O0, Lex0),
      rdf_datatype_compat(Lex0, D)
    )
  ).



%! rdf_datatype_compat_supremum(+Lex, -D) is nondet.
%
% Like rdf_datatype_compat/2, but only enumerates the supremums.

rdf_datatype_compat_supremum(Lex, D1) :-
  rdf_datatype_compat(Lex, D1),
  \+ (
    rdf_datatype_compat(Lex, D2),
    rdf_strict_subdatatype_of(D2, D1)
  ).



%! rdf_datatypes_supremum(+Ds, -D) is semidet.
%
% Datatype D is the supremum of the given datatypes Ds.

rdf_datatypes_supremum([H], H) :- !.
rdf_datatypes_supremum([H1,H2|T], Sup) :-
  rdf_subdatatype_of(H1, H3),
  rdf_subdatatype_of(H2, H3), !,
  rdf_datatypes_supremum([H3|T], Sup).



%! rdf_strict_subdatatype_of(?Subtype, ?Supertype) is nondet.

rdf_strict_subdatatype_of(X, Y) :-
  dif(X, Y),
  rdf_subdatatype_of(X, Y).



%! rdf_subdatatype_of(?Subtype, ?Supertype) is nondet.

rdf_subdatatype_of(X, Y) :-
  xsd_subtype_of(X, Y).



%! rdf_value_space_property(+D, +Prop) is semidet.
%! rdf_value_space_property(+D, -Prop) is nondet.
%! rdf_value_space_property(-D, +Prop) is nondet.
%! rdf_value_space_property(-D, -Prop) is multi.
%
% Enumerates the properties Prop of the value space of datatype D.
%
% The following properties are supported:
%   * antisymmetric
%   * comparable
%   * reflexive
%   * total
%   * transitive

rdf_value_space_property(C, total) :-
  rdf_value_space_property(C, antisymmetric),
  rdf_value_space_property(C, comparable),
  rdf_value_space_property(C, reflexive),
  rdf_value_space_property(C, transitive).
rdf_value_space_property(xsd:decimal, antisymmetric).
rdf_value_space_property(xsd:decimal, comparable).
rdf_value_space_property(xsd:decimal, reflexive).
rdf_value_space_property(xsd:decimal, transitive).
rdf_value_space_property(xsd:float,   antisymmetric).
rdf_value_space_property(xsd:float,   reflexive).
rdf_value_space_property(xsd:float,   transitive).
rdf_value_space_property(xsd:gYear,   antisymmetric).
rdf_value_space_property(xsd:gYear,   comparable).
rdf_value_space_property(xsd:gYear,   reflexive).
rdf_value_space_property(xsd:gYear,   transitive).
rdf_value_space_property(xsd:integer, antisymmetric).
rdf_value_space_property(xsd:integer, comparable).
rdf_value_space_property(xsd:integer, reflexive).
rdf_value_space_property(xsd:integer, transitive).
