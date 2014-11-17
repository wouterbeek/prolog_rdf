:- module(
  rdf_build,
  [
    rdf_assert_instance/3, % +Instance:or([bnode,iri])
                           % ?Class:iri
                           % ?Graph:atom
    rdf_assert_language_tagged_string/6, % +Term:rdf_term
                                         % +Predicate:iri
                                         % +LexicalForm:atom
                                         % +LangTag:list(atom)
                                         % ?Graph:atom
                                         % -Triple:compound
    rdf_assert_literal/7, % +Term:rdf_term
                          % +Predicate:iri
                          % +LexicalForm:atom
                          % ?DatatypeIri:iri
                          % ?LangTag:list(atom)
                          % ?Graph:atom
                          % -Triple:compound
    rdf_assert_property/2, % +Property:iri
                           % ?Graph:atom
    rdf_assert2/4, % +Subject:or([bnode,iri])
                   % +Predicate:iri
                   % +Object:rdf_term
                   % ?Graph:atom
    rdf_copy/5, % +FromGraph:atom
                % ?Subject:or([bnode,iri])
                % ?Predicate:iri
                % ?Object:rdf_term
                % +ToGraph:atom
    rdf_create_next_resource/5, % +Prefix:atom
                                % +SubPaths:list(atom)
                                % ?Class:iri
                                % ?Graph:atom
                                % -Resource:iri
    rdf_retractall2/4, % ?Term:rdf_term
                       % ?Predicate:iri
                       % ?Object:rdf_term
                       % ?Graph:atom
    rdf_retractall_literal/5, % ?Subject:oneof([bnode,iri])
                              % ?Predicate:iri
                              % ?Value:atom
                              % ?DatatypeIri:iri
                              % ?Graph:atom
    rdf_retractall_resource/2, % +Resource:rdf_term
                               % ?Graph:atom
    rdf_retractall_term/2 % +Term:rdf_term
                          % ?Graph:atom
  ]
).

/** <module> RDF API: Build RDF

Simple asserion and retraction predicates for RDF.
Triples with literals are treated in dedicated modules.

@author Wouter Beek
@version 2013/10, 2013/12-2014/01, 2014/06, 2014/08-2014/11
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(library(uri)).

:- use_module(generics(meta_ext)).

:- use_module(plRdf(api/rdf_read)).
:- use_module(plRdf(entailment/rdf_bnode_map)).
:- use_module(plRdf(term/rdf_datatype)).

:- rdf_meta(rdf_assert_instance(o,r,?)).
:- rdf_meta(rdf_assert_language_tagged_string(o,r,+,+,?,-)).
:- rdf_meta(rdf_assert_literal(o,r,+,?,?,?,-)).
:- rdf_meta(rdf_assert_property(o,?)).
:- rdf_meta(rdf_assert2(o,r,o,?)).
:- rdf_meta(rdf_copy(+,r,r,o,+)).
:- rdf_meta(rdf_create_next_resource(+,+,r,?,-)).
:- rdf_meta(rdf_retractall2(o,r,o,?)).
:- rdf_meta(rdf_retractall_literal(o,r,?,r,?)).
:- rdf_meta(rdf_retractall_resource(o,?)).
:- rdf_meta(rdf_retractall_term(o,?)).



%! rdf_assert_instance(+Term:rdf_term, ?Class:iri, ?Graph:graph) is det.
% Asserts an instance/class relationship.
%
% The following triples are added to the database:
%
% ```nquads
% <TERM,rdf:type,CLASS,GRAPH>
% ```
%
% @arg Instance Required IRI or blank node.
% @arg Class    Using `rdfs:Resource` when uninstantiated.
% @arg Grapg    Using `user` when uninstantiated.

rdf_assert_instance(Term, Class, Graph):-
  default(rdfs:'Resource', Class),
  rdf_assert2(Term, rdf:type, Class, Graph).



%! rdf_assert_language_tagged_string(
%!   +Term:rdf_term,
%!   +Predicate:iri,
%!   +LexicalForm:atom,
%!   +LangTag:list(atom),
%!   ?Graph:atom,
%!   -Triple:compound
%! ) is det.

rdf_assert_language_tagged_string(Term, P, LexicalForm, LangTag, G, T):-
  rdf_assert_literal(Term, P, LexicalForm, rdf:langString, LangTag, G, T).



%! rdf_assert_literal(
%!   +Term:rdf_term,
%!   +Predicate:iri,
%!   +Value:atom,
%!   ?Datatype:iri,
%!   ?LangTag:list(atom),
%!   ?Graph:atom,
%!   -Triple:compound
%! ) is det.
% Asserts a triple with a literal object term.
%
% Only emits canonical representations for XSD values.
%
% @compat RDF 1.1 Concepts and Abstract Syntax
% @compat XSD 1.1 Schema 2: Datatypes

% Language-tagged strings.
rdf_assert_literal(Node, P, Val, rdf:langString, LangTag, G, rdf(Node,P,O)):-
  nonvar(LangTag), !,
  O = literal(lang(LangTag,Val)),
  rdf_assert2(Node, P, O, Graph).
% Simple literals.
rdf_assert_literal(Node, P, Value, Datatype, _, Graph, Triple):-
  var(Datatype), !,
  rdf_assert_literal(Node, P, Value, xsd:string, _, Graph, Triple).
% (Explicitly) typed literals.
rdf_assert_literal(Node, P, Value, Datatype, _, Graph, rdf(Node,P,O)):-
  rdf_canonical_map(Datatype, Value, LexicalForm),
  O = literal(type(Datatype,LexicalForm)),
  rdf_assert2(Node, P, O, Graph).



%! rdf_assert_property(+Term:rdf_term, ?Graph:atom) is det.
% Asserts an RDF property.
%
% The following triples are added to the database:
%
% ```nquads
% <TERM,rdf:type,rdf:Property,GRAPH>
% ```

rdf_assert_property(Term, Graph):-
  rdf_assert_instance(Term, rdf:'Property', Graph).



%! rdf_assert2(
%!   +Term:rdf_term,
%!   +Predicate:iri,
%!   +Object:rdf_term,
%!   ?Graph:atom
%! ) is det.
% Like rdf/4 in [rdf_db], but allows Graph to be uninstantiated.
%
% @see rdf_db:rdf/4

rdf_assert2(Literal, P, O, G):-
  rdf_is_literal(Literal), !,
  term_get_bnode(Literal, BNode),
  rdf_assert2(BNode, P, O, G).
rdf_assert2(Node, P, O, G):-
  var(G), !,
  rdf_assert(Node, P, O).
rdf_assert2(Node, P, O, G):-
  rdf_assert(Node, P, O, G).



%! rdf_copy(
%!   +FromGraph:atom,
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:or([bnode,iri,literal]),
%!   +ToGraph:atom
%! ) is det.
% Copies triples between graphs.
%
% @tbd Perform blank node renaming.

rdf_copy(FromGraph, S, P, O, ToGraph):-
  forall(
    rdf(S, P, O, FromGraph),
    rdf_assert(S, P, O, ToGraph)
  ).



%! rdf_create_next_resource(
%!   +Prefix:atom,
%!   +SubPaths:list(atom),
%!   ?Class:iri,
%!   ?Graph:atom,
%!   -Resource:iri
%! ) is det.
% Creates new resource-denoting IRIs in a uniform way.
%
% @arg Prefix is a registered RDF prefix name.
%      The replacing IRI is used as the base IRI for the resource.
%      See rdf_register_prefix/2.
% @arg SubPaths is a list of path names that are suffixed to the base IRI.
% @arg Class An optional IRI denoting an RDFS class.
%      See rdf_assert_instance/3.
% @arg Graph An optional RDF graph name.
% @arg Resource The newly created IRI.
%
% The Prefix + Subpaths combination is used as the unique flag name
% for counting the created IRIs.

rdf_create_next_resource(Prefix, SubPaths1, Class, Graph, Resource):-
  % A counter keeps track of the integer identifier of the IRI.
  with_output_to(atom(FlagTerm), write_term([Prefix|SubPaths1], [])),
  rdf_atom_md5(FlagTerm, 1, Flag),
  flag(Flag, Id, Id + 1),
  
  % The identifier is appended to the IRI path.
  append(SubPaths1, [Id], SubPaths2),
  atomic_list_concat(SubPaths2, '/', Path),
  
  % Resolve the absolute IRI against the base IRI denoted by the RDF prefix.
  rdf_global_id(Prefix:'', Base),
  uri_normalized(Path, Base, Resource),
  
  (   nonvar(Class)
  ->  rdf_assert_instance(Resource, Class, Graph)
  ;   true
  ).



%! rdf_retractall2(
%!   ?Term:rdf_term,
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   ?Graph:atom
%! ) is det.

rdf_retractall2(Term, P, O, Graph):-
  rdf_is_literal(Term), !,
  term_get_bnode(Term, BNode),
  rdf_retractall(BNode, P, O, Graph).
rdf_retractall2(Node, P, O, Graph):-
  rdf_retractall(Node, P, O, Graph).



%! rdf_retractall_literal(
%!   ?Term:rdf_term,
%!   ?Predicate:iri,
%!   ?Value,
%!   ?DatatypeIri:iri,
%!   ?Graph:atom
%! ) is det.
% Retracts all matching RDF triples that have literal object terms.
%
% Implementation note: this assumes that simple literals are always
%  asserted with datatype IRI `xsd:string`.
% We do not retract literal compound terms of the form
%  `literal(LexicalForm:atom)`.

rdf_retractall_literal(Node, P, Value, Datatype, Graph):-
  % Retract language-tagged strings only if:
  %   1. Datatype is unifiable with `rdf:langString`, and
  %   2. Value us unifiable with a value from the value space of
  %       language-tagged strings.
  (   rdf_equal(Datatype, rdf:langString),
      Value = LexicalForm-LangTag
  ->  rdf_retractall2(Node, P, literal(lang(LangTag,LexicalForm)), Graph)
  ;   true
  ),
  % Retract all matching typed literals.
  forall(
    (
      rdf(Node, P, literal(type(Datatype,LexicalForm)), Graph),
      % Possibly computationally intensive!
      rdf_lexical_map(Datatype, LexicalForm, Value)
    ),
    rdf_retractall2(Node, P, literal(type(Datatype,LexicalForm)), Graph)
  ).



%! rdf_retractall_resource(+Resource:rdf_term, ?Graph:atom) is det.
% Removes all triples in which the resource denoted by the given RDF term
%  occurs.

rdf_retractall_resource(Term, Graph):-
  forall(
    rdf_id(Term, Term0),
    rdf_retractall_term(Term0, Graph)
  ).



%! rdf_retractall_term(+Term:rdf_term, ?Graph:atom) is det.
% Removes all triples in which the given RDF term occurs.

rdf_retractall_term(Term, Graph):-
  rdf_retractall2(Term, _, _, Graph),
  rdf_retractall2(_, Term, _, Graph),
  rdf_retractall2(_, _, Term, Graph).
