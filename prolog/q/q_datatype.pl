:- module(
  q_datatype,
  [
    q_col_datatype/3,          % ?M, +P, -D
    q_col_datatype/4,          % ?M, +P, ?G, -D
    q_datatype_check/2,        % +D, +Val
    q_datatype_compat/2,       % +Lex, -D
    q_datatype_compat_min/2,   % +Lex, -D
    q_datatypes_compat/3,      % ?M, +P, -Ds
    q_datatypes_compat/4,      % ?M, +P, ?G, -Ds
    q_datatype_property/2,     % ?D, ?Property
    q_datatype_supremum/2,     % +Datatypes, -Supremum
    q_strict_subdatatype_of/2, % ?Subtype, ?Supertype
    q_subdatatype_of/2         % ?Subtype, ?Supertype
  ]
).

/** <module> RDF datatype

@author Wouter Beek
@version 2016/06, 2016/09
*/

:- use_module(library(semweb/rdf11)).
:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(debug_ext)).
:- use_module(library(error)).
:- use_module(library(html/html_dom)).
:- use_module(library(ordsets)).
:- use_module(library(q/q_rdf)).
:- use_module(library(q/q_term)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(sgml)).
:- use_module(library(xml/xml_dom)).
:- use_module(library(xsd/xsd)).

:- rdf_meta
   q_col_datatype(?, r, -),
   q_col_datatype(?, r, r, -),
   q_datatype_compat(+, r),
   q_datatype_compat_min(+, r),
   q_datatype_property(r, ?),
   q_datatype_supremum(t, t),
   q_datatypes_compat(?, r, -),
   q_datatypes_compat(?, r, r, -),
   q_strict_subdatatype_of(r, r),
   q_subdatatype_of(r, r).





%! q_col_datatype(?M, +P, -D) is semidet.
%! q_col_datatype(?M, +P, ?G, -D) is nondet.

q_col_datatype(M, P, D) :-
  q_col_datatype(M, P, _, D).


q_col_datatype(M, P, G, D) :-
  q(M, _, P, O, G),
  q_literal_datatype(O, D), !,
  forall(q(M, _, P, O0, G), q_literal_datatype(O0, D)).



%! q_datatype_check(+D, +Val) is semidet.

q_datatype_check(D, Val) :-
  rdf11:in_ground_type(D, Val, _).



%! q_datatype_compat(+Lex, -D) is det.

q_datatype_compat(Lex, D) :-
  catch((
    rdf11:xsd_date_time_type(D),
    xsd_time_string(_, D, Lex)
  ), _, false).
q_datatype_compat(Lex, D) :-
  catch(xsd_number_string(N, Lex), _, false),
  rdf11:xsd_numerical(D, Dom, _),
  is_of_type(Dom, N).
q_datatype_compat(Lex, rdf:'HTML') :-
  string_to_atom(Lex, Lex0),
  call_collect_messages(atom_to_html_dom(Lex0, Dom), _, _),
  memberchk(element(_,_,_), Dom).
q_datatype_compat(Lex, rdf:'XMLLiteral') :-
  string_to_atom(Lex, Lex0),
  call_collect_messages(atom_to_xml_dom(Lex0, Dom), _, _),
  memberchk(element(_,_,_), Dom).
q_datatype_compat(Lex, xsd:boolean) :-
  rdf11:in_boolean(Lex, _).
q_datatype_compat(Lex, xsd:boolean) :-
  string(Lex),
  number_string(N, Lex),
  rdf11:in_boolean(N, _).
q_datatype_compat(_, xsd:string).



%! q_datatype_compat_min(+Lex, -D) is nondet.

q_datatype_compat_min(Lex, D1) :-
  q_datatype_compat(Lex, D1),
  \+ (
    q_datatype_compat(Lex, D2),
    q_strict_subdatatype_of(D2, D1)
  ).



%! q_datatype_property(?D, ?Property) is nondet.

q_datatype_property(C, total) :-
  q_datatype_property(C, antisymmetric),
  q_datatype_property(C, comparable),
  q_datatype_property(C, reflexive),
  q_datatype_property(C, transitive).
q_datatype_property(xsd:decimal, antisymmetric).
q_datatype_property(xsd:decimal, comparable).
q_datatype_property(xsd:decimal, reflexive).
q_datatype_property(xsd:decimal, transitive).
q_datatype_property(xsd:float,   antisymmetric).
q_datatype_property(xsd:float,   reflexive).
q_datatype_property(xsd:float,   transitive).
q_datatype_property(xsd:gYear,   antisymmetric).
q_datatype_property(xsd:gYear,   comparable).
q_datatype_property(xsd:gYear,   reflexive).
q_datatype_property(xsd:gYear,   transitive).
q_datatype_property(xsd:integer, antisymmetric).
q_datatype_property(xsd:integer, comparable).
q_datatype_property(xsd:integer, reflexive).
q_datatype_property(xsd:integer, transitive).



%! q_datatype_supremum(+Datatypes, -Supremum) is semidet.
%
% The Supremum is the smallest datatype that covers all given
% Datatypes.

q_datatype_supremum([H], H) :- !.
q_datatype_supremum([H1,H2|T], Sup) :-
  q_subdatatype_of(H1, H3),
  q_subdatatype_of(H2, H3), !,
  q_datatype_supremum([H3|T], Sup).



%! q_datatypes_compat(?M, +P, +Ds) is det.
%! q_datatypes_compat(?M, +P, ?G, +Ds) is det.
%
% @tbd Can be optimized when the compatible datatypes of the previous
% lexical form act as a filter on the datatypes that need to be
% checked for the next lexical form.

q_datatypes_compat(M, P, Ds) :-
  q_datatypes_compat(M, P, _, Ds).


q_datatypes_compat(M, P, G, Ds) :-
  q_aggregate_all(set(O), q(M, _, P, O, G), Os),
  maplist(term_compat_datatypes0, Os, Dss),
  (Dss == [] -> Ds = [] ; ord_intersection(Dss, Ds)).


term_compat_datatypes0(O, Ds) :-
  q_is_literal(O), !,
  q_literal_lex(O, Lex),
  aggregate_all(set(D), q_datatype_compat(Lex, D), Ds).
term_compat_datatypes0(O, [iri]) :-
  q_is_iri(O), !.
term_compat_datatypes0(O, [bnode]) :- !,
  q_is_bnode(O).



%! q_strict_subdatatype_of(?Subtype, ?Supertype) is nondet.

q_strict_subdatatype_of(X, Y) :-
  dif(X, Y),
  q_subdatatype_of(X, Y).



%! q_subdatatype_of(?Subtype, ?Supertype) is nondet.

q_subdatatype_of(X, Y) :-
  xsd_subtype_of(X, Y).
q_subdatatype_of(X, Y) :-
  rdfs_subclass_of(X, Y),
  \+ xsd_subtype_of(X, Y).
