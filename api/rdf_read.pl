:- module(
  rdf_read,
  [
    rdf_ground_triple/4, % ?Subject:or([bnode,iri])
                         % ?Predicate:iri
                         % ?Object:rdf_term
                         % ?Graph:atom
    rdf_id/2, % ?Term:rdf_term
              % ?EquivTerm:rdf_term
    rdf_is_ground_triple/1, % +Triple:compound
    rdf_langstring/5, % ?Term:rdf_term
                      % ?Predicate:iri
                      % ?LexicalForm:atom
                      % ?LangTag:atom
                      % ?Graph:atom
    rdf_langstring_data/1, % ?Field:oneof([datatype,langtag,lexical_form])
                           % +Literal:compound
                           % -Data
    rdf_langstring_term/1, % ?Literal:compound
    rdf_literal/6, % ?Term:rdf_term
                   % ?Predicate:iri
                   % ?LexicalForm:atom
                   % ?Datatype:iri
                   % ?LangTag:list(atom)
                   % ?Graph:atom
    rdf_literal_data/3, % ?Field:oneof([datatype,langtag,lexical_form])
                        % +Literal:compound
                        % -Data
    rdf_literal_term/1, % ?Literal:compound
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
    rdf_term_edge/4, % +Term:rdf_term
                     % -Predicate:iri
                     % -OtherTerm:rdf_term
                     % ?Graph:atom
    rdf_term_incoming_edge/4, % +Object:rdf_term
                              % -Predicate:iri
                              % -Subject:or([bnode,iri])
                              % ?Graph:atom
    rdf_term_outgoing_edge/4 % +Subject:or([bnode,iri])
                             % -Predicate:iri
                             % -Object:rdf_term
                             % ?Graph:atom
  ]
).

/** <module> RDF API: Read

Predicates for reading from RDF, customized for specific datatypes and
 literals.

@author Wouter Beek
@version 2014/11
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(semweb/rdf_db)).

:- use_module(plRdf(term/rdf_term)).

:- rdf_meta(rdf_ground_triple(r,r,o,?)).
:- rdf_meta(rdf_id(o,o)).
:- rdf_meta(rdf_langstring(o,r,?,?,?)).
:- rdf_meta(rdf_langstring_data(?,o,-)).
:- rdf_meta(rdf_langstring_term(o)).
:- rdf_meta(rdf_literal(o,r,?,r,?,?)).
:- rdf_meta(rdf_literal_data(+,o,-)).
:- rdf_meta(rdf_literal_term(o)).
:- rdf_meta(rdf_resource_edge(o,r,o,?)).
:- rdf_meta(rdf_resource_incoming_edge(o,r,o,?)).
:- rdf_meta(rdf_resource_outgoing_edge(o,r,o,?)).
:- rdf_meta(rdf_term_edge(o,r,o,?)).
:- rdf_meta(rdf_term_incoming_edge(o,r,o,?)).
:- rdf_meta(rdf_term_outgoing_edge(o,r,o,?)).

:- multifile(error:has_type/2).
error:has_type(rdf_term, Term):-
  (   rdf_is_bnode(Term)
  ;   rdf_is_literal(Term)
  ;   rdf_is_resource(Term)
  ).



%! rdf_ground_triple(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   ?Graph:atom
%! ) is nondet.

rdf_ground_triple(S, P, O, G):-
  rdf(S, P, O, G),
  rdf_is_ground_triple(rdf(S,P,O)).



%! rdf_id(+Term:rdf_term, +EquivTerm:rdf_term) is semidet.
%! rdf_id(+Term:rdf_term, -EquivTerm:rdf_term) is multi.
%! rdf_id(-Term:rdf_term, +EquivTerm:rdf_term) is multi.

rdf_id(T1, T2):-
  rdf_reachable(T1, owl:sameAs, T2).



%! rdf_is_ground_triple(+Triple:compound) is semidet.
% Succeeds if the given triple is ground, i.e., contains no blank node.

rdf_is_ground_triple(rdf(S,_,O)):-
  maplist(rdf_is_name, [S,O]).



%! rdf_langstring(
%!   ?Term:rdf_term,
%!   ?Predicate:iri,
%!   ?LexicalForm:atom,
%!   ?LangTag:list(atom),
%!   ?Graph:atom
%! ) is nondet.

rdf_langstring(Term, Predicate, LexicalForm, LangTag, Graph):-
  rdf_literal(Term, Predicate, LexicalForm, rdf:langString, LangTag, Graph).



%! rdf_langstring_data(
%!   +Field:oneof([datatype,langtag,lexical_form])
%!   +Literal:compound
%!   -Data
%! ) is det.
%! rdf_langstring_data(
%!   -Field:oneof([datatype,langtag,lexical_form])
%!   +Literal:compound
%!   -Data
%! ) is multi.

rdf_langstring_data(datatype, literal(lang(_,_)), rdf:langString).
rdf_langstring_data(langtag, literal(lang(Langtag,_)), Langtag).
rdf_langstring_data(lexical_form, literal(lang(_,LexicalForm)), LexicalForm).



%! rdf_langstring_term(+LangTaggedString:compound) is semidet.
%! rdf_langstring_term(-LangTaggedString:compound) is nondet.
% Language tagged strings, according to the Semweb format.
% Enumeration is assured to not deliver any duplicates.

rdf_langstring_term(literal(lang(LangTag,LexicalForm))):-
  rdf_current_literal(literal(lang(LangTag,LexicalForm))).



%! rdf_literal(
%!   ?Term:rdf_term,
%!   ?Predicate:iri,
%!   ?LexicalForm:atom,
%!   ?Datatype:iri,
%!   ?LangTag:list(atom),
%!   ?Graph:graph
%! ) is nondet.

% Literals that are mapped onto a blank node.
rdf_literal(Literal, P, LexicalForm, rdf:langString, LangTag, Graph):-
  rdf_is_literal(Literal),
  term_get_bnode(Literal, BNode), !,
  rdf_literal(BNode, P, LexicalForm, rdf:langString, LangTag, Graph).
% Language-tagged strings.
rdf_literal(Node, P, LexicalForm, rdf:langString, LangTag, Graph):-
  rdf(Node, P, literal(lang(LangTag,LexicalForm)), Graph).
% Simple literals and (explicitly) typed literals.
rdf_literal(Node, P, LexicalForm, Datatype, LangTag, Graph):-
  var(LangTag),
  (   rdf_equal(Datatype, xsd:string),
      rdf(Node, P, literal(LexicalForm), Graph)
  ;   rdf(Node, P, literal(type(Datatype,LexicalForm)), Graph)
  ).



%! rdf_literal_data(
%!   +Field:oneof([datatype,langtag,lexical_form])
%!   +Literal:compound
%!   -Data
%! ) is det.
%! rdf_literal_data(
%!   -Field:oneof([datatype,langtag,lexical_form])
%!   +Literal:compound
%!   -Data
%! ) is multi.

rdf_literal_data(datatype, literal(type(Datatype,_)), Datatype).
rdf_literal_data(langtag, literal(lang(LangTag,_)), LangTag).
rdf_literal_data(lexical_form, Literal, LexicalForm):-
  (   Literal = literal(lang(_,LexicalForm))
  ->  true
  ;   Literal = literal(type(_,LexicalForm))
  ->  true
  ;   Literal = literal(LexicalForm)
  ).



%! rdf_literal_term(+Literal:compound) is semidet.
%! rdf_literal_term(-Literal:compound) is nondet.

rdf_literal_term(Literal):-
  % Enumerates all literals.
  rdf_current_literal(Literal).



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
