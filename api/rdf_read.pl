:- module(
  rdf_read,
  [
    rdf_direction_triples/4, % +Resource:iri,
                             % +Direction:oneof([backward,both,forward])
                             % -Triples:ordset(compound)
                             % ?Graph:atom
    rdf_instance/3, % ?Instance:or([bnode,iri])
                    % ?Class:iri
                    % ?Graph:atom
    rdf_langstring/5, % ?Subject:or([bnode,iri])
                      % ?Predicate:iri
                      % ?Value:pair(atom,list(atom))
                      % ?LangTagPreference:list(list(atom))
                      % ?Graph:atom
    rdf_literal/6, % ?Subject:or([bnode,iri])
                   % ?Predicate:iri
                   % ?Value
                   % ?Datatype:iri
                   % ?LangTag:list(atom)
                   % ?Graph:atom
    rdf_literal/7, % ?Subject:or([bnode,iri])
                   % ?Predicate:iri
                   % ?Value
                   % ?Datatype:iri
                   % ?LangTag:list(atom)
                   % ?Graph:atom
                   % -Triple:compound
    rdf_plain_literal/4, % ?Subject:or([bnode,iri])
                         % ?Predicate:iri
                         % ?Value
                         % ?LangTagPreference:list(list(atom))
    rdf_plain_literal/5, % ?Subject:or([bnode,iri])
                         % ?Predicate:iri
                         % ?Value
                         % ?LangTagPreference:list(list(atom))
                         % ?Graph:atom
    rdf_resource_edge/4, % +Term:rdf_term
                         % -Predicate:iri
                         % -OtherTerm:rdf_term
                         % ?Graph:atom
    rdf_resource_incoming_edge/4, % +Object:rdf_term
                                  % -Predicate:iri
                                  % -Subject:or([bnode,iri])
                                  % ?Graph:atom
    rdf_resource_outgoing_edge/4, % +Subject:or([bnode,iri])
                                  % -Predicate:iri
                                  % -Object:rdf_term
                                  % ?Graph:atom
    rdf_simple_literal/3, % ?Subject:or([bnode,iri])
                          % ?Predicate:iri
                          % ?Value:atom
    rdf_simple_literal/4, % ?Subject:or([bnode,iri])
                          % ?Predicate:iri
                          % ?Value:atom
                          % ?Graph:atom
    rdf_term_edge/4, % +Term:rdf_term
                     % -Predicate:iri
                     % -OtherTerm:rdf_term
                     % ?Graph:atom
    rdf_term_incoming_edge/4, % +Object:rdf_term
                              % -Predicate:iri
                              % -Subject:or([bnode,iri])
                              % ?Graph:atom
    rdf_term_outgoing_edge/4, % +Subject:or([bnode,iri])
                              % -Predicate:iri
                              % -Object:rdf_term
                              % ?Graph:atom
    rdf_typed_literal/4, % ?Subject:or([bnode,iri])
                         % ?Predicate:iri
                         % ?Value
                         % ?Datatype:iri
    rdf_typed_literal/5 % ?Subject:or([bnode,iri])
                        % ?Predicate:iri
                        % ?Value
                        % ?Datatype:iri
                        % ?Graph:atom
  ]
).

/** <module> RDF API: Read

Predicates for reading from RDF, customized for specific datatypes and
 literals.

@author Wouter Beek
@version 2014/11-2014/12, 2015/02
*/

:- use_module(library(lists), except([delete/3])).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(generics(list_ext)).

:- use_module(plRdf(term/rdf_datatype)).
:- use_module(plRdf(term/rdf_term)).

:- rdf_meta(rdf_instance(r,r,?)).
:- rdf_meta(rdf_langstring(r,r,?,?,?)).
:- rdf_meta(rdf_literal(r,r,?,r,?,?)).
:- rdf_meta(rdf_literal(r,r,?,r,?,?,-)).
:- rdf_meta(rdf_plain_literal(r,r,?,?)).
:- rdf_meta(rdf_plain_literal(r,r,?,?,?)).
:- rdf_meta(rdf_resource_edge(t,r,t,?)).
:- rdf_meta(rdf_resource_incoming_edge(t,r,t,?)).
:- rdf_meta(rdf_resource_outgoing_edge(t,r,t,?)).
:- rdf_meta(rdf_simple_literal(r,r,?)).
:- rdf_meta(rdf_simple_literal(r,r,?,?)).
:- rdf_meta(rdf_term_edge(t,r,t,?)).
:- rdf_meta(rdf_term_incoming_edge(t,r,t,?)).
:- rdf_meta(rdf_term_outgoing_edge(t,r,t,?)).
:- rdf_meta(rdf_typed_literal(r,r,?,r)).
:- rdf_meta(rdf_typed_literal(r,r,?,r,?)).

:- multifile(error:has_type/2).
error:has_type(rdf_term, Term):-
  (   rdf_is_bnode(Term)
  ;   rdf_is_literal(Term)
  ;   rdf_is_resource(Term)
  ).





%! rdf_direction_triples(
%!   +Resource:iri,
%!   +Direction:oneof([backward,both,forward]),
%!   -Triples:ordset(compound),
%!   ?Graph:atom
%! ) is det.

rdf_direction_triples(Resource, forward, Triples, Graph):-
  aggregate_all(
    set(rdf(Resource,P,O)),
    rdf_term_outgoing_edge(Resource, P, O, Graph),
    Triples
  ).
rdf_direction_triples(Resource, backward, Triples, Graph):-
  aggregate_all(
    set(rdf(S,P,Resource)),
    rdf_term_incoming_edge(S, P, Resource, Graph),
    Triples
  ).
rdf_direction_triples(Resource, both, Triples, Graph):-
  rdf_direction_triples(Resource, backward, Triples1, Graph),
  rdf_direction_triples(Resource, forward, Triples2, Graph),
  ord_union(Triples1, Triples2, Triples).



%! rdf_instance(?Instance:or([bnode,iri]), ?Class:iri, ?Graph:atom) is nondet.

rdf_instance(Instance, Class, Graph):-
  rdf(Instance, rdf:type, Class, Graph).



%! rdf_langstring(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Value:pair(atom,list(atom)),
%!   ?LangTagPreference:list(list(atom)),
%!   ?Graph:atom
%! ) is nondet.

% Language-tag preference can be met.
rdf_langstring(S, P, Value, LangPrefs, Graph):-
  is_list(LangPrefs), !,
  rdf_literal(S, P, Value, rdf:langString, LangPrefs, Graph).
% Language-tag preference cannot be met.
rdf_langstring(S, P, Value, _, Graph):-
  rdf_literal(S, P, Value, rdf:langString, _, Graph).



%! rdf_literal(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Value,
%!   ?Datatype:iri,
%!   ?LangTagPreferences:list(list(atom)),
%!   ?Graph:graph
%! ) is nondet.

rdf_literal(S, P, Value, Datatype, LangPrefs, Graph):-
  rdf_literal(S, P, Value, Datatype, LangPrefs, Graph, _).

%! rdf_literal(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Value,
%!   ?Datatype:iri,
%!   ?LangTagPreferences:list(list(atom)),
%!   ?Graph:graph
%! ) is nondet.

% Language-tagged strings.
% No datatype is formally defined for `rdf:langString` because
%  the definition of datatypes does not accommodate language tags
%  in the lexical space.
% The value space of `rdf:langString` is the set of all pairs
%  of strings and language tags.
rdf_literal(S, P, Value, rdf:langString, LangPrefs, Graph, rdf(Node,P,O)):-
  O = literal(lang(LangTag0,LexicalValue)),
  % Prioritize language-tagged strings based on the given language tag
  %  preferences, if any.
  % Respect the order on preferences from left to right.
  % Notice that this merely prioritizes: every language-tagged string
  % is returned eventually.
  (   is_list(LangPrefs),
      member(LangTag, LangPrefs),
      sublist(LangTagPrefix, LangTag),
      prefix(LangTagPrefix, LangTag),
      LangTagPrefix \== [],
      atomic_list_concat(LangTagPrefix, '-', LangTag0)
  ;   true
  ),
  rdf(S, P, O, Graph),
  Value = LexicalValue-LangTag.
% Simple literals and (explicitly) typed literals.
rdf_literal(S, P, Value, Datatype, _, Graph, rdf(Node,P,O)):-
  (   O = literal(type(Datatype,LexicalForm)),
      rdf(S, P, O, Graph),
      % Possibly computationally intensive.
      rdf_lexical_map(Datatype, LexicalForm, Value)
  ;   rdf_equal(Datatype, xsd:string),
      O = literal(Value),
      rdf(S, P, O, Graph),
      Value \= type(_,_)
  ).



%! rdf_plain_literal(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Value,
%!   ?LangTagPreference:list(list(atom))
%! ) is nondet.

rdf_plain_literal(S, P, Value, LangTagPreference):-
  rdf_plain_literal(S, P, Value, LangTagPreference, _).


%! rdf_plain_literal(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Value,
%!   ?LangTagPreference:list(list(atom)),
%!   ?Graph:atom
%! ) is nondet.

rdf_plain_literal(S, P, Value, LangTagPreference, Graph):-
  rdf_langstring(S, P, Value, LangTagPreference, Graph).
rdf_plain_literal(S, P, Value, _, Graph):-
  rdf_simple_literal(S, P, Value, Graph).



%! rdf_resource_edge(
%!   +Term:rdf_term,
%!   -Predicate:iri,
%!   -OtherTerm:rdf_term,
%!   ?Graph:atom
%! ) is nondet.
% Returns incoming and outgoing edges for the resource denoted by
%  the given RDF term.

rdf_resource_edge(From, P, To, G):-
  rdf_resource_outgoing_edge(From, P, To, G).
rdf_resource_edge(To, P, From, G):-
  rdf_resource_incoming_edge(To, P, From, G).



%! rdf_resource_incoming_edge(
%!   +To:rdf_term,
%!   -Predicate:iri,
%!   -From:rdf_term,
%!   ?Graph:atom
%! ) is nondet.
% Returns incoming edges for the resource denoted by the given RDF term.

rdf_resource_incoming_edge(To, P, From, G):-
  rdf_id(To, To0),
  rdf(From0, P, To0, G),
  rdf_id(From, From0).



%! rdf_resource_outgoing_edge(
%!   +From:rdf_term,
%!   -Predicate:iri,
%!   -To:rdf_term,
%!   ?Graph:atom
%! ) is nondet.
% Returns outgoing edges for the resource denoted by the given RDF term.

rdf_resource_outgoing_edge(From, P, To, G):-
  rdf_id(From, From0),
  rdf(From0, P, To0, G),
  rdf_id(To, To0).



%! rdf_simple_literal(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Value:atom
%! ) is nondet.

rdf_simple_literal(S, P, Value):-
  rdf_simple_literal(S, P, Value, _).

%! rdf_simple_literal(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Value:atom,
%!   ?Graph:atom
%! ) is nondet.

rdf_simple_literal(S, P, Value, Graph):-
  rdf_literal(S, P, Value, xsd:string, _, Graph).



%! rdf_term_edge(
%!   +Term:rdf_term,
%!   -Predicate:iri,
%!   -OtherTerm:rdf_term,
%!   ?Graph:atom
%! ) is nondet.
% Returns incoming and outgoing edges for the given RDF term.

rdf_term_edge(S, P, O, G):-
  rdf_term_outgoing_edge(S, P, O, G).
rdf_term_edge(O, P, S, G):-
  rdf_term_incoming_edge(O, P, S, G).



%! rdf_term_incoming_edge(
%!   +Object:rdf_term,
%!   -Predicate:iri,
%!   -Subject:or([bnode,iri]),
%!   ?Graph:atom
%! ) is nondet.
% Returns incoming edges for the given RDF term.

rdf_term_incoming_edge(O, P, S, G):-
  rdf(S, P, O, G).



%! rdf_term_outgoing_edge(
%!   +Subject:or([bnode,iri]),
%!   -Predicate:iri,
%!   -Object:rdf_term,
%!   ?Graph:atom
%! ) is nondet.
% Returns outgoing edges for the given RDF term.

rdf_term_outgoing_edge(S, P, O, G):-
  rdf(S, P, O, G).



%! rdf_typed_literal(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Value,
%!   ?Datatype:iri
%! ) is nondet.

rdf_typed_literal(S, P, Value, Datatype):-
  rdf_typed_literal(S, P, Value, Datatype, _).

%! rdf_typed_literal(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Value,
%!   ?Datatype:iri,
%!   ?Graph:atom
%! ) is nondet.

rdf_typed_literal(S, P, Value, Datatype, Graph):-
  rdf_literal(S, P, Value, Datatype, _, Graph).
