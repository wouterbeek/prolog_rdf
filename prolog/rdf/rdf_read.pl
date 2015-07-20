:- module(
  rdf_read,
  [
    rdf_date/3, % ?Subject:or([bnode,iri])
                % ?Predicate:iri
                % ?Date:compound
    rdf_date/4, % ?Subject:or([bnode,iri])
                % ?Predicate:iri
                % ?Date:compound
                % ?Graph:atom
    rdf_instance/3, % ?Instance:iri
                    % ?Class:iri
                    % ?Graph:atom
    rdf_load_vocab/1, % +Location:atom
    rdf_literal/4, % ?Subject:iri
                   % ?Predicate:iri
                   % ?Datatype:iri
                   % ?Value
    rdf_literal/4, % ?Subject:or([bnode,iri])
                   % ?Predicate:iri
                   % ?Datatype:iri
                   % ?Value
    rdf_literal/5 % ?Subject:or([bnode,iri])
                  % ?Predicate:iri
                  % ?Datatype:iri
                  % ?Value
                  % ?Graph:atom
  ]
).

/** <module> RDF read

@author Wouter Beek
@compat [RDF 1.1 Concepts and Abstract Syntax](http://www.w3.org/TR/rdf11-concepts/)
@license MIT License
@version 2015/07
*/

:- use_module(library(error)).
:- use_module(library(rdf/rdf_datatype)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_http_plugin)).

:- rdf_meta(rdf_date(r,r,?)).
:- rdf_meta(rdf_date(r,r,?,?)).
:- rdf_meta(rdf_instance(r,r,?)).
:- rdf_meta(rdf_literal(r,r,r,?)).
:- rdf_meta(rdf_literal(r,r,r,?,?)).

:- multifile(error:has_type/2).
error:has_type(rdf_term, Term):-
  (   rdf_is_bnode(Term)
  ;   rdf_is_literal(Term)
  ;   rdf_is_resource(Term)
  ).





%! rdf_date(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Date:compound
%! ) is nondet.
% Wrapper around rdf_date/4.

rdf_date(S, P, V):-
  rdf_date(S, P, V, _).

%! rdf_date(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Date:compound,
%!   ?Graph:atom
%! ) is nondet.
% Read some date-time value.
%
% Supports the following RDF datatypes:
%   * xsd:date
%   * xsd:dateTime
%   * xsd:gDay
%   * xsd:gMonth
%   * xsd:gMonthDay
%   * xsd:gYear
%   * xsd:gYearMonth
%   * xsd:time

rdf_date(S, P, V, G):-
  rdf_typed_literal(S, P, xsd:date, V, G).
rdf_date(S, P, V, G):-
  rdf_typed_literal(S, P, xsd:dateTime, V, G).
rdf_date(S, P, V, G):-
  rdf_typed_literal(S, P, xsd:gDay, V, G).
rdf_date(S, P, V, G):-
  rdf_typed_literal(S, P, xsd:gMonth, V, G).
rdf_date(S, P, V, G):-
  rdf_typed_literal(S, P, xsd:gMonthDay, V, G).
rdf_date(S, P, V, G):-
  rdf_typed_literal(S, P, xsd:gYear, V, G).
rdf_date(S, P, V, G):-
  rdf_typed_literal(S, P, xsd:gYearMonth, V, G).
rdf_date(S, P, V, G):-
  rdf_typed_literal(S, P, xsd:time, V, G).



%! rdf_instance(?Instance:or([bnode,iri]), ?Class:iri, ?Graph:atom) is nondet.

rdf_instance(I, C, G):-
  rdf(I, rdf:type, C, G).



%! rdf_load_vocab(+Location:atom) is det.
% Succees by loading the vocabulary denoted with the given URI
% or RDF namespace-denoting prefix into the RDF DB.

rdf_load_vocab(Prefix):-
  rdf_current_prefix(Prefix, Uri), !,
  rdf_load_vocab(Uri, Prefix).
rdf_load_vocab(Uri):-
  rdf_load_vocab(Uri, Uri).

rdf_load_vocab(Uri, Graph):-
  rdf_load(Uri, [graph(Graph)]).



%! rdf_literal(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Datatype:iri,
%!   ?Value
%! ) is nondet.

rdf_literal(S, P, D, V):-
  rdf_literal(S, P, D, V, _).

%! rdf_literal(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Datatype:iri,
%!   ?Value,
%!   ?Graph:graph
%! ) is nondet.

% Language-tagged strings.
rdf_literal(S, P, rdf:langString, V, G):-
  V = Lex-Lang,
  (   ground(Lang)
  ->  atomic_list_concat(Lang, '-', Lang0), 
      rdf(S, P, literal(lang(Lang0,Lex)), G)
  ;   rdf(S, P, literal(lang(Lang0,Lex)), G),
      atomic_list_concat(Lang, '-', Lang0)
  ).
% Ground datatype and value.
rdf_literal(S, P, D, V, G):-
  ground(D),
  ground(V), !,
  % Map to lexical form.
  rdf_canonical_map(D, V, Lex),
  (   rdf_equal(D, xsd:string),
      O = literal(Lex)
  ;   O = literal(type(D,Lex))
  ),
  rdf(S, P, O, G).
% Typed literal (as per RDF 1.0 specification).
rdf_literal(S, P, D, V, G):-
  rdf(S, P, literal(type(D,Lex)), G),
  rdf_lexical_map(D, Lex, V).
% Simple literal (as per RDF 1.0 specification).
rdf_literal(S, P, xsd:string, V, G):-
  rdf(S, P, literal(Lex), G),
  rdf_lexical_map(xsd:string, Lex, V).
