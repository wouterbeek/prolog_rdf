:- module(
  rdf_reification_read,
  [
    rdf_datatype_statement/6, % ?Subject:or([bnode,iri])
                              % ?Predicate:iri
                              % ?Value
                              % ?Datatype:iri
                              % ?Graph:graph
                              % ?Statement:or([bnode,iri])
    rdf_object/3, % ?Statement:or([bnode,iri])
                  % ?Object:onef([literal,resource])
                  % ?Graph:graph
    rdf_predicate/3, % ?Statement:or([bnode,iri])
                     % ?Predicate:iri
                     % +Graph:atom
    rdf_statement/5, % ?Subject:onef([bnode,iri])
                     % ?Predicate:iri
                     % ?Object:onef([bnode,iri,literal])
                     % ?Graph:atom
                     % ?Statement:or([bnode,iri])
    rdf_subject/3 % ?Statement:or([bnode,iri])
                  % ?Subject:or([bnode,iri])
                  % ?Graph:atom
  ]
).

/** <module> RDF reification: read

Read support for reified triples.

@author Wouter Beek
@version 2014/09
*/

:- use_module(plXsd(xsd)).

:- use_module(plRdf_term(rdf_literal)).

:- rdf_meta(rdf_datatype_statement(r,r,?,r,?,r)).
:- rdf_meta(rdf_object(r,o,?)).
:- rdf_meta(rdf_predicate(r,r,?)).
:- rdf_meta(rdf_statement(r,r,o,?,r)).
:- rdf_meta(rdf_subject(r,r,?)).



%! rdf_datatype_statement(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Value,
%!   ?Datatype:iri,
%!   ?Graph:atom,
%!   ?Statement:or([bnode,iri])
%! ) is nondet.
% Reads statements with datatyped literals converted to proper values.

rdf_datatype_statement(
  Subject,
  Predicate,
  Value,
  Datatype,
  Graph,
  Statement
):-
  rdf_subject(Statement, Subject, Graph),
  rdf_predicate(Statement, Predicate, Graph),
  (   nonvar(Datatype),
      ground(Value)
  ->  xsd_canonical_map(Datatype, Value, LexicalForm),
      rdf_object_literal(Statement, LexicalForm, Datatype, _, Graph)
  ;   rdf_object_literal(Statement, LexicalForm, Datatype, _, Graph),
      rdf_literal_map(LexicalForm, Datatype, _, Value)
  ).


%! rdf_object(
%!   ?Statement:or([bnode,iri]),
%!   ?Object:or([bnode,iri,literal]),
%!   ?Graph:atom
%! ) is nondet.

% Literals are treated specially.
rdf_object(Statement, Object, Graph):-
  rdf_is_literal(Object), !,
  rdf_literal(Object, LexicalForm, Datatype, LanguageTag),
  rdf_object_literal(Statement, LexicalForm, Datatype, LanguageTag, Graph).
rdf_object(Statement, Object, Graph):-
  rdf(Statement, rdf:object, Object, Graph).


%! rdf_object_literal(
%!    ?Statement:or([bnode,iri]),
%!    ?LexicalForm:atom,
%!    ?Datatype:iri,
%!    ?LanguageTag:atom,
%!    ?Graph:atom
%! ) is nondet.
% Relations between statements and decomposed literals.
% The literals are notinterpreted, i.e. they are plain lexical forms.

rdf_object_literal(Statement, LexicalForm, Datatype, LanguageTag, Graph):-
  rdf_literal(
    Statement,
    rdf:object,
    LexicalForm,
    Datatype,
    LanguageTag,
    Graph
  ).


%! rdf_predicate(
%!   ?Statement:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Graph:atom
%! ) is nondet.

rdf_predicate(Statement, Predicate, Graph):-
  rdf(Statement, rdf:predicate, Predicate, Graph).


%! rdf_statement(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate,
%!   ?Object:or([bnode,iri,literal]),
%!   ?Graph:atom,
%!   ?Statement:or([bnode,iri])
%! ) is nondet.
% Relations between statements and their components,
% where the object term is treated as a plain literal,
% i.e. not interpreted.

rdf_statement(Subject, Predicate, Object, Graph, Statement):-
  rdf_subject(Statement, Subject, Graph),
  rdf_predicate(Statement, Predicate, Graph),
  rdf_object(Statement, Object, Graph).


%! rdf_subject(
%!   ?Statement:or([bnode,iri]),
%!   ?Subject:or([bnode,iri]),
%!   ?Graph:atom
%! ) is nondet.

rdf_subject(Statement, Subject, Graph):-
  rdf(Statement, rdf:subject, Subject, Graph).

