:- module(
  rdf_build,
  [
    rdf_assert_instance/2, % +Instance:or([bnode,iri])
                           % ?Class:iri
    rdf_assert_instance/3, % +Instance:or([bnode,iri])
                           % ?Class:iri
                           % ?Graph:atom
    rdf_assert_langstring/4, % +Subject:or([bnode,iri])
                             % +Predicate:iri
                             % +Value:pair(list(atom),atom)
                             % ?Graph:atom
    rdf_assert_langstring/5, % +Subject:or([bnode,iri])
                             % +Predicate:iri
                             % +Value:pair(list(atom),atom)
                             % ?Graph:atom
                             % -Triple:compound
    rdf_assert_literal/5, % +Subject:or([bnode,iri])
                          % +Predicate:iri
                          % +LexicalForm:atom
                          % ?Datatype:iri
                          % ?Graph:atom
    rdf_assert_literal/6, % +Subject:or([bnode,iri])
                          % +Predicate:iri
                          % +LexicalForm:atom
                          % ?Datatype:iri
                          % ?Graph:atom
                          % -Triple:compound
    rdf_assert_plain_literal/4, % +Subject:or([bnode,iri])
                                % +Predicate:iri
                                % +Value
                                % ?Graph:atom
    rdf_assert_plain_literal/5, % +Subject:or([bnode,iri])
                                % +Predicate:iri
                                % +Value
                                % ?Graph:atom
                                % -Triple:compound
    rdf_assert_property/2, % +Property:iri
                           % ?Graph:atom
    rdf_assert_simple_literal/4, % +Subject:or([bnode,iri])
                                 % +Predicate:iri
                                 % +Value
                                 % ?Graph:atom
    rdf_assert_simple_literal/5, % +Subject:or([bnode,iri])
                                 % +Predicate:iri
                                 % +Value
                                 % ?Graph:atom
                                 % -Triple:compound
    rdf_assert_typed_literal/4, % +Subject:or([bnode,iri])
                                % +Predicate:iri
                                % +Value
                                % +Datatype:iri
    rdf_assert_typed_literal/5, % +Subject:or([bnode,iri])
                                % +Predicate:iri
                                % +Value
                                % +Datatype:iri
                                % ?Graph:atom
    rdf_assert_typed_literal/6, % +Subject:or([bnode,iri])
                                % +Predicate:iri
                                % +Value
                                % +Datatype:iri
                                % ?Graph:atom
                                % -Triple:compound
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
    rdf_retractall2/4, % ?Subject:or([bnode,iri])
                       % ?Predicate:iri
                       % ?Object:rdf_term
                       % ?Graph:atom
    rdf_retractall_literal/5, % ?Subject:or([bnode,iri])
                              % ?Predicate:iri
                              % ?Value
                              % ?Datatype:iri
                              % ?Graph:atom
    rdf_retractall_resource/2, % +Resource:rdf_term
                               % ?Graph:atom
    rdf_retractall_simple_literal/4, % ?Subject:or([bnode,iri])
                                     % ?Predicate:iri
                                     % ?Value
                                     % ?Graph:atom
    rdf_retractall_term/2 % +Term:rdf_term
                          % ?Graph:atom
  ]
).

/** <module> RDF API: Build RDF

Simple asserion and retraction predicates for RDF.
Triples with literals are treated in dedicated modules.

@author Wouter Beek
@version 2014/11, 2015/02
*/

:- use_module(library(semweb/rdf_db), except([rdf_node/1])).
:- use_module(library(uri)).

:- use_module(generics(meta_ext)).

:- use_module(plDcg(dcg_generics)).

:- use_module(plLangTag(language_tag)).

:- use_module(plRdf(term/rdf_datatype)).
:- use_module(plRdf(term/rdf_term)).

:- rdf_meta(rdf_assert_instance(o,r)).
:- rdf_meta(rdf_assert_instance(o,r,?)).
:- rdf_meta(rdf_assert_langstring(o,r,+,+)).
:- rdf_meta(rdf_assert_langstring(o,r,+,+,-)).
:- rdf_meta(rdf_assert_literal(o,r,+,r,?)).
:- rdf_meta(rdf_assert_literal(o,r,+,r,?,-)).
:- rdf_meta(rdf_assert_plain_literal(o,r,+,?)).
:- rdf_meta(rdf_assert_plain_literal(o,r,+,?,-)).
:- rdf_meta(rdf_assert_property(o,?)).
:- rdf_meta(rdf_assert_simple_literal(o,r,+,?)).
:- rdf_meta(rdf_assert_simple_literal(o,r,+,?,-)).
:- rdf_meta(rdf_assert_typed_literal(o,r,+,r)).
:- rdf_meta(rdf_assert_typed_literal(o,r,+,r,?)).
:- rdf_meta(rdf_assert_typed_literal(o,r,+,r,?,-)).
:- rdf_meta(rdf_assert2(t,r,o,?)).
:- rdf_meta(rdf_copy(+,r,r,o,+)).
:- rdf_meta(rdf_create_next_resource(+,+,r,?,-)).
:- rdf_meta(rdf_retractall_literal(o,r,?,r,?)).
:- rdf_meta(rdf_retractall_resource(o,?)).
:- rdf_meta(rdf_retractall_simple_literal(o,r,?,?)).
:- rdf_meta(rdf_retractall_term(o,?)).





%! rdf_assert_instance(+Instance:or([bnode,iri]), ?Class:iri) is det.
% @see rdf_assert_instance/3

rdf_assert_instance(Instance, Class):-
  rdf_assert_instance(Instance, Class, user).

%! rdf_assert_instance(
%!   +Instance:or([bnode,iri]),
%!   ?Class:iri,
%!   ?Graph:atom
%! ) is det.
% Asserts an instance/class relationship.
%
% The following triples are added to the database:
%
% ```nquads
% <TERM,rdf:type,CLASS,GRAPH>
% ```
%
% @arg Instance Required: IRI or blank node.
% @arg Class    Using `rdfs:Resource` when uninstantiated.
% @arg Graph    Using `user` when uninstantiated.

rdf_assert_instance(Instance, Class, Graph):-
  default(rdfs:'Resource', Class),
  rdf_assert2(Instance, rdf:type, Class, Graph).



%! rdf_assert_langstring(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Value:pair(list(atom),atom),
%!   ?Graph:atom
%! ) is det.

rdf_assert_langstring(S, P, Value, Graph):-
  rdf_assert_langstring(S, P, Value, Graph, _).

%! rdf_assert_langstring(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Value:pair(list(atom),atom),
%!   ?Graph:atom,
%!   -Triple:compound
%! ) is det.

rdf_assert_langstring(S, P, Value, Graph, Triple):-
  rdf_assert_literal(S, P, Value, rdf:langString, Graph, Triple).



%! rdf_assert_literal(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Value,
%!   ?Datatype:iri,
%!   ?Graph:atom
%! ) is det.

rdf_assert_literal(S, P, Value, Datatype, Graph):-
  rdf_assert_literal(S, P, Value, Datatype, Graph, _).

%! rdf_assert_literal(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Value,
%!   ?Datatype:iri,
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
rdf_assert_literal(S, P, LangTag-LexicalForm, rdf:langString, G, Triple):-
  nonvar(LangTag), !,
  % @ tbd Use 'Language-Tag'//1.
  atomic_list_concat(LangTag, '-', LangTagString),
  O = literal(lang(LangTagString,LexicalForm)),
  rdf_assert2(S, P, O, G),
  Triple = rdf(S,P,O).
% Simple literals.
rdf_assert_literal(S, P, Value, Datatype, Graph, Triple):-
  var(Datatype), !,
  rdf_assert_literal(S, P, Value, xsd:string, Graph, Triple).
% (Explicitly) typed literals.
rdf_assert_literal(S, P, Value, Datatype, Graph, rdf(Node,P,O)):-
  rdf_canonical_map(Datatype, Value, LexicalForm),
  O = literal(type(Datatype,LexicalForm)),
  rdf_assert2(S, P, O, Graph).



%! rdf_assert_plain_literal(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Value,
%!   ?Graph:atom
%! ) is det.
% @see rdf_assert_plain_literal/5

rdf_assert_plain_literal(S, P, Value, Graph):-
  rdf_assert_plain_literal(S, P, Value, Graph, _).

%! rdf_assert_plain_literal(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Value,
%!   ?Graph:atom,
%!   -Triple:compound
%! ) is det.
% Asserts a plain literal.

rdf_assert_plain_literal(S, P, Value, Graph, Triple):-
  (   Value = _-_
  ->  rdf_equal(Datatype, rdf:langTag)
  ;   rdf_equal(Datatype, xsd:string)
  ),
  rdf_assert_literal(S, P, Value, Datatype, Graph, Triple).



%! rdf_assert_property(+Property:iri, ?Graph:atom) is det.
% Asserts an RDF property.
%
% The following triples are added to the database:
%
% ```nquads
% <TERM,rdf:type,rdf:Property,GRAPH>
% ```

rdf_assert_property(Property, Graph):-
  rdf_assert_instance(Property, rdf:'Property', Graph).



%! rdf_assert_simple_literal(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Value,
%!   ?Graph:atom
%! ) is det.
% @see rdf_assert_simple_literal/5

rdf_assert_simple_literal(S, P, Value, Graph):-
  rdf_assert_simple_literal(S, P, Value, Graph, _).

%! rdf_assert_simple_literal(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Value,
%!   ?Graph:atom,
%!   -Triple:compound
%! ) is det.
% Asserts a simple literal.

rdf_assert_simple_literal(S, P, Value, Graph, Triple):-
  rdf_assert_literal(S, P, Value, xsd:string, Graph, Triple).



%! rdf_assert_typed_literal(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Value,
%!   +Datatype:iri
%! ) is det.
% @see rdf_assert_typed_literal/6

rdf_assert_typed_literal(S, P, Value, Datatype):-
  rdf_assert_typed_literal(S, P, Value, Datatype, user).

%! rdf_assert_typed_literal(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Value,
%!   +Datatype:iri,
%!   ?Graph:atom
%! ) is det.
% @see rdf_assert_typed_literal/6

rdf_assert_typed_literal(S, P, Value, Datatype, Graph):-
  rdf_assert_typed_literal(S, P, Value, Datatype, Graph, _).

%! rdf_assert_typed_literal(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Value,
%!   +Datatype:iri,
%!   ?Graph:atom,
%!   -Triple:compound
%! ) is det.
% Asserts a typed literal.

rdf_assert_typed_literal(S, P, Value, Datatype, Graph, Triple):-
  rdf_assert_literal(S, P, Value, Datatype, Graph, Triple).



%! rdf_assert2(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Object:rdf_term,
%!   ?Graph:atom
%! ) is det.
% Like rdf/4 in [rdf_db], but allows Graph to be uninstantiated.
%
% @see rdf_db:rdf/4

rdf_assert2(S, P, O, G):-
  (   var(G)
  ->  rdf_assert(S, P, O)
  ;   rdf_assert(S, P, O, G)
  ).



%! rdf_copy(
%!   +FromGraph:atom,
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
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



%! rdf_retractall_literal(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Value,
%!   ?Datatype:iri,
%!   ?Graph:atom
%! ) is det.
% Retracts all matching RDF triples that have literal object terms.
%
% Implementation note: this assumes that simple literals are always
%  asserted with datatype IRI `xsd:string`.
% We do not retract literal compound terms of the form
%  `literal(LexicalForm:atom)`.

rdf_retractall_literal(S, P, Value, Datatype, Graph):-
  % Retract language-tagged strings only if:
  %   1. Datatype is unifiable with `rdf:langString`, and
  %   2. Value us unifiable with a value from the value space of
  %       language-tagged strings.
  (   rdf_equal(Datatype, rdf:langString),
      Value = LexicalForm-LangTag
  ->  rdf_retractall(S, P, literal(lang(LangTag,LexicalForm)), Graph)
  ;   true
  ),

  % Retract all matching typed literals.
  forall(
    (
      rdf(S, P, literal(type(Datatype,LexicalForm)), Graph),
      % Possibly computationally intensive!
      rdf_lexical_map(Datatype, LexicalForm, Value)
    ),
    rdf_retractall(S, P, literal(type(Datatype,LexicalForm)), Graph)
  ).



%! rdf_retractall_resource(+Resource:rdf_term, ?Graph:atom) is det.
% Removes all triples in which the resource denoted by the given RDF term
%  occurs.

rdf_retractall_resource(Term, Graph):-
  forall(
    rdf_id(Term, Term0),
    rdf_retractall_term(Term0, Graph)
  ).



%! rdf_retractall_simple_literal(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Value,
%!   ?Graph:atom
%! ) is det.

rdf_retractall_simple_literal(S, Predicate, Value, Graph):-
  rdf_retractall_literal(S, Predicate, Value, xsd:string, Graph).



%! rdf_retractall_term(+Term:rdf_term, ?Graph:atom) is det.
% Removes all triples in which the given RDF term occurs.

rdf_retractall_term(Term, Graph):-
  rdf_retractall(Term, _, _, Graph),
  rdf_retractall(_, Term, _, Graph),
  rdf_retractall(_, _, Term, Graph).
