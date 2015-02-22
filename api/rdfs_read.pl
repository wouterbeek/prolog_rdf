:- module(
  rdfs_read,
  [
    rdfs_label/4, % ?Term:rdf_term
                  % ?Value
                  % ?LangTagPreference:list(list(atom))
                  % ?Graph:atom
    rdfs_label_value/2, % ?Term:rdf_term
                        % ?Label:atom
    rdfs_label_value/4 % ?Term:rdf_term
                       % ?Label:atom
                       % ?LangTagPreference:list(list(atom))
                       % ?Graph:atom
  ]
).

/** <module> RDFS Read API

@author Wouter Beek
@version 2014/11-2015/02
*/

:- use_module(library(lists), except([delete/3,subset/2])).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plRdf(api/rdf_read)).
:- use_module(plRdf(term/rdf_term)).

:- rdf_meta(rdfs_label(o,?,?,?)).
:- rdf_meta(rdfs_label(o,?,?,?)).
:- rdf_meta(rdfs_label_value(o,?)).
:- rdf_meta(rdfs_label_value(o,?,?,?)).





%! rdfs_label(
%!   ?Term:rdf_term,
%!   ?Value,
%!   ?LangTagPreference:list(list(atom)),
%!   ?Graph:atom
%! ) is nondet.
% Reads RDFS labels attributed to resources.

rdfs_label(Term, Value, LangPrefs, Graph):-
  rdf_plain_literal(Term, rdfs:label, Value, LangPrefs, Graph).



%! rdfs_label_value(?Term:rdf_term, ?Label:atom) is nondet.

rdfs_label_value(Term, Label):-
  rdfs_label_value(Term, Label, _, _).

%! rdfs_label_value(
%!   ?Term:rdf_term,
%!   ?Label:atom,
%!   ?LangTagPreference:list(list(atom)),
%!   ?Graph:atom
%! ) is nondet.
% Since RDFS labels can be of type `rdf:langTag` or `xsd:string`,
% the `Value` returned by rdfs_label/4 can be either an atom or a pair.
%
% This predicate normalizes argument Label.
% It is either the full `Value` (if `xsd:string`)
% or the second argument of the pair (if `rdf:langTag`).
%
% @see rdfs_label/4

rdfs_label_value(Term, Label, LangPrefs, Graph):-
  rdfs_label(Term, Value, LangPrefs, Graph),
  (   Value = Label-_
  ->  true
  ;   Label = Value
  ).
