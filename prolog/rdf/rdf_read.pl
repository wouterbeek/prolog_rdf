:- module(
  rdf_read,
  [
    rdf_date/3, % ?Subject, ?Predicate, ?Date
    rdf_date/4, % ?Subject:rdf_term
                % ?Predicate:iri
                % ?Date:compound
                % ?Graph:atom
    rdf_instance/2, % ?Instance, ?Class
    rdf_instance/3, % ?Instance:iri
                    % ?Class:iri
                    % ?Graph:atom
    rdf_langstring/4, % ?Subject, ?Predicate, +LanguagePreference, ?Value
    rdf_langstring/5, % ?Subject:rdf_term
                      % ?Predicate:iri
                      % +LanguagePreference:atom
                      % ?Value:pair(atom)
                      % ?Graph:atom
    rdf_load_vocab/1, % +Location:atom
    rdf_literal/4, % ?Subject, ?Predicate, ?Datatype, ?Value
    rdf_literal/5, % ?Subject, ?Predicate, ?Datatype, ?Value, ?Graph
    rdf_literal/6, % ?Subject:rdf_term
                   % ?Predicate:iri
                   % ?Datatype:iri
                   % ?Value
                   % ?Graph:atom
                   % -Triple:compound
    rdf2/3, % ?Subject, ?Predicate, ?Object
    rdf2/4 % ?Subject:rdf_term
           % ?Predicate:iri
	   % ?Object:rdf_term
	   % ?Graph:atom
  ]
).

/** <module> RDF read

@author Wouter Beek
@compat [RDF 1.1 Concepts and Abstract Syntax](http://www.w3.org/TR/rdf11-concepts/)
@license MIT License
@version 2015/07-2015/08
*/

:- use_module(library(error)).
:- use_module(library(langtag/langtag_match)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_datatype)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_http_plugin)).

:- rdf_meta(rdf_date(o,r,?)).
:- rdf_meta(rdf_date(o,r,?,?)).
:- rdf_meta(rdf_instance(o,r)).
:- rdf_meta(rdf_instance(o,r,?)).
:- rdf_meta(rdf_langstring(o,r,+,?)).
:- rdf_meta(rdf_langstring(o,r,+,?,?)).
:- rdf_meta(rdf_literal(o,r,r,?)).
:- rdf_meta(rdf_literal(o,r,r,?,?)).
:- rdf_meta(rdf_literal(o,r,r,?,?,-)).
:- rdf_meta(rdf2(o,r,o)).
:- rdf_meta(rdf2(o,r,o,?)).

:- multifile(error:has_type/2).
error:has_type(rdf_term, Term):-
  (   rdf_is_bnode(Term)
  ;   rdf_is_literal(Term)
  ;   rdf_is_resource(Term)
  ).





%! rdf_date(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Date:compound
%! ) is nondet.
% Wrapper around rdf_date/4.

rdf_date(S, P, V):-
  rdf_date(S, P, V, _).

%! rdf_date(
%!   ?Subject:rdf_term,
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
  rdf_literal(S, P, xsd:date, V, G).
rdf_date(S, P, V, G):-
  rdf_literal(S, P, xsd:dateTime, V, G).
rdf_date(S, P, V, G):-
  rdf_literal(S, P, xsd:gDay, V, G).
rdf_date(S, P, V, G):-
  rdf_literal(S, P, xsd:gMonth, V, G).
rdf_date(S, P, V, G):-
  rdf_literal(S, P, xsd:gMonthDay, V, G).
rdf_date(S, P, V, G):-
  rdf_literal(S, P, xsd:gYear, V, G).
rdf_date(S, P, V, G):-
  rdf_literal(S, P, xsd:gYearMonth, V, G).
rdf_date(S, P, V, G):-
  rdf_literal(S, P, xsd:time, V, G).



%! rdf_instance(?Instance:rdf_term, ?Class:iri, ?Graph:atom) is nondet.

rdf_instance(I, C):-
  rdf_instance(I, C, _).

%! rdf_instance(?Instance:rdf_term, ?Class:iri, ?Graph:atom) is nondet.

rdf_instance(I, C, G):-
  rdf2(I, rdf:type, C, G).



%! rdf_langstring(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   +LanguagePreferences:list(list(atom)),
%!   ?Value:pair(atom,list(atom))
%! ) is nondet.

rdf_langstring(S, P, Prefs, V):-
  rdf_langstring(S, P, Prefs, V, _).

%! rdf_langstring(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   +LanguagePreference:atom,
%!   ?Value:pair(atom),
%!   ?Graph:atom
%! ) is nondet.

rdf_langstring(S, P, Pref, V, G):-
  rdf_literal(S, P, rdf:langString, V, G),
  V = Lang-_,
  basic_filtering(Pref, Lang).



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
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Datatype:iri,
%!   ?Value
%! ) is nondet.

rdf_literal(S, P, D, V):-
  rdf_literal(S, P, D, V, _).


%! rdf_literal(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Datatype:iri,
%!   ?Value,
%!   ?Graph:atom
%! ) is nondet.

rdf_literal(S, P, D, V, G):-
  rdf_literal(S, P, D, V, G, _).


%! rdf_literal(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Datatype:iri,
%!   ?Value,
%!   ?Graph:graph,
%!   -Triple:compound
%! ) is nondet.

% Language-tagged strings.
rdf_literal(S, P, rdf:langString, V, G, rdf(S,P,O,G)):-
  V = Lang-Lex,
  O = literal(lang(Lang,Lex)),
  (   ground(Lang)
  ->  rdf2(S, P, O, G)
  ;   rdf2(S, P, O, G)
  ).
% Ground datatype and value.
rdf_literal(S, P, D, V, G, rdf(S,P,O,G)):-
  ground(D),
  ground(V), !,
  % Map to lexical form.
  rdf_canonical_map(D, V, Lex),
  (   rdf_equal(D, xsd:string),
      O = literal(Lex)
  ;   O = literal(type(D,Lex))
  ),
  rdf2(S, P, O, G).
% Typed literal (as per RDF 1.0 specification).
rdf_literal(S, P, D, V, G, rdf(S,P,O,G)):-
  O = literal(type(D,Lex)),
  rdf2(S, P, O, G),
  rdf_lexical_map(D, Lex, V).
% Simple literal (as per RDF 1.0 specification).
rdf_literal(S, P, xsd:string, V, G, rdf(S,P,O,G)):-
  O = literal(Lex),
  rdf2(S, P, O, G),
  atom(Lex),
  rdf_global_id(xsd:string, D),
  rdf_lexical_map(D, Lex, V).



%! rdf2(?Subject:rdf_term, ?Predicate:iri, ?Object:rdf_term) is nondet.
% Variant of rdf/3 that allows literals in the subject position.

% Literals may need to be related to blank nodes.
rdf2(S0, P, O):-
  rdf_is_literal(S0),
  bnode_literal(S, S0), !,
  rdf(S, P, O).
% Variable subject terms may be blank nodes
% that need to be related to literals.
rdf2(S, P, O):-
  var(S), !,
  rdf(S0, P, O),
  (   rdf_is_bnode(S0),
      bnode_literal(S0, S)
  ->  true
  ;   S = S0
  ).
rdf2(S, P, O):-
  rdf(S, P, O).


%! rdf2(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   ?Graph:atom
%! ) is nondet.
% Variant of rdf/4 that allows literals in the subject position.

rdf2(S0, P, O, G):-
  rdf_is_literal(S0),
  bnode_literal(S, S0), !,
  rdf(S, P, O, G).
rdf2(S, P, O, G):-
  var(S), !,
  rdf(S0, P, O, G),
  (   rdf_is_bnode(S0),
      bnode_literal(S0, S)
  ->  true
  ;   S = S0
  ).
rdf2(S, P, O, G):-
  rdf(S, P, O, G).
