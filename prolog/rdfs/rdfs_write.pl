:- module(
  rdfs_write,
  [
    rdfs_assert_comment/3, % +Subject:or([bnode,iri])
                           % +Comment:atom
                           % ?Graph:atom
    rdfs_assert_domain/3, % +Property:iri
                          % +Domain:iri
                          % ?Graph:atom
    rdfs_assert_isDefinedBy/2, % +Subject:or([bnode,iri])
                               % ?Graph:atom
    rdfs_assert_isDefinedBy/3, % +Subject:or([bnode,iri])
                               % ?Uri:atom
                               % ?Graph:atom
    rdfs_assert_label/2, % +Subject:or([bnode,iri])
                         % +Label
    rdfs_assert_label/3, % +Subject:or([bnode,iri])
                         % +Label
                         % ?Graph:atom
    rdfs_assert_property/4, % +Domain:iri
                            % +Property:iri
                            % +Range:iri
                            % ?Graph:atom
    rdfs_assert_range/3, % +Property:iri
                         % +Range:iri
                         % ?Graph:atom
    rdfs_assert_seeAlso/3, % +Subject:or([bnode,iri])
                           % +Uri:atom
                           % +Graph:atom
    rdfs_assert_subclass/3, % +SubClass:iri
                            % ?SuperClass:iri
                            % ?Graph:atom
    rdfs_assert_subproperty/3, % +Subproperty:or([bnode,iri])
                               % +SuperProperty:iri
                               % ?Graph:atom
    rdfs_retractall_class_resource/1, % +Class:iri
    rdfs_retractall_class_term/1, % +Class:iri
    rdfs_retractall_label/3 % +Subject:or([bnode,iri])
                            % ?Value
                            % ?Graph:atom
  ]
).

/** <module> RDFS write

Predicates for asseritng RDFS statements in an easy way.

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(rdf/rdf_default)).
:- use_module(library(rdf/rdf_write)).
:- use_module(library(semweb/rdf_db)).

:- rdf_meta(rdfs_assert_comment(r,+,?)).
:- rdf_meta(rdfs_assert_domain(r,r,+)).
:- rdf_meta(rdfs_assert_isDefinedBy(r,?)).
:- rdf_meta(rdfs_assert_isDefinedBy(r,?,?)).
:- rdf_meta(rdfs_assert_label(r,+)).
:- rdf_meta(rdfs_assert_label(r,+,?)).
:- rdf_meta(rdfs_assert_property(r,r,r,+)).
:- rdf_meta(rdfs_assert_range(r,r,+)).
:- rdf_meta(rdfs_assert_seeAlso(r,+,?)).
:- rdf_meta(rdfs_assert_subclass(r,r,+)).
:- rdf_meta(rdfs_assert_subproperty(r,r,?)).
:- rdf_meta(rdfs_retractall_class_resource(r)).
:- rdf_meta(rdfs_retractall_class_term(r)).
:- rdf_meta(rdfs_retractall_label(r,?,?)).





%! rdfs_assert_comment(
%!   +Subject:or([bnode,iri]),
%!   +Comment:or([atom,pair(atom,list(atom))]),
%!   ?Graph:atom
%! ) is det.

% Without a language tag the comment is asserted as XSD string.
rdfs_assert_comment(S, V, G):-
  V = _-_, !,
  rdf_assert_literal(S, rdfs:comment, rdf:langString, V, G).
% With a language tag the comment is asserted as RDF langString.
rdfs_assert_comment(S, V, G):-
  rdf_assert_literal(S, rdfs:comment, xsd:string, V, G).



%! rdfs_assert_domain(+Property:iri, +Domain:iri, ?Graph:atom) is det.
% Asserts the following propositions:
%
% ```nquads
% NODE  rdfs:domain CLASS GRAPH .
% ```

rdfs_assert_domain(P, D, G):-
  rdf_assert2(P, rdfs:domain, D, G).



%! rdfs_assert_isDefinedBy(+Subject:or([bnode,iri]), ?Graph:atom) is det.

rdfs_assert_isDefinedBy(S, G):-
  rdfs_assert_isDefinedBy(S, _, G).


%! rdfs_assert_isDefinedBy(
%!   +Subject:or([bnode,iri]),
%!   ?Uri:atom,
%!   ?Graph:atom
%! ) is det.
% Asserts the following propositions:
%
% ```nquads
% NODE  rdfs:isDefinedBy  NAMESPACE GRAPH .
% ```
%
% If the given RDF term is a literal,
% then rdfs:isDefinedBy is asserted of its assigned blank node.
%
% If Uri is uninstantiated, the IRI denoted by the registered RDF prefix
% of Term is used, if any.

rdfs_assert_isDefinedBy(S, O, G):-
  var(O), !,
  rdf_prefix_iri(S, O),
  rdf_assert2(S, rdfs:isDefinedBy, O, G).
rdfs_assert_isDefinedBy(S, O, G):-
  rdf_assert2(S, rdfs:isDefinedBy, O, G).



%! rdfs_assert_label(+Subject:or([bnode,iri]), +Label, ?Graph:atom) is det.

rdfs_assert_label(S, V):-
  rdfs_assert_label(S, V, _).

%! rdfs_assert_label(+Subject:or([bnode,iri]), +Label, ?Graph:atom) is det.
% Assigns an RDFS label to the resource denoted by the given RDF term.
%
% This predicate stores the label as an RDF language-tagged string.
% The default language is `en-US`.

% Labels without language tag are asserted as `xsd:string`.
rdfs_assert_label(S, V, G):-
  V = _-_, !,
  rdf_assert_literal(S, rdfs:label, rdf:langString, V, G).
% Labels with language tag are asserted as `rdf:langString`.
rdfs_assert_label(S, V, G):-
  rdfs_assert_literal(S, rdfs:label, xsd:string, V, G).



%! rdfs_assert_property(
%!   +Domain:iri,
%!   +Property:iri,
%!   +Range:iri,
%!   ?Graph:atom
%! ) is det.
% Asserts the following propositions:
%
% ```nquads
% NODE  rdfs:range  CLASS GRAPH .
% ```

rdfs_assert_property(D, P, R, G):-
  rdfs_assert_domain(P, D, G),
  rdfs_assert_range(P, R, G).



%! rdfs_assert_range(+Property:iri, +Range:iri, ?Graph:atom) is det.
% Asserts the following propositions:
%
% ```nquads
% NODE  rdfs:range  CLASS GRAPH .
% ```

rdfs_assert_range(P, R, G):-
  rdf_assert2(P, rdfs:range, R, G).



%! rdfs_assert_seeAlso(
%!   +Subject:or([bnode,iri]),
%!   +Uri:atom,
%!   ?Graph:atom
%! ) is det.
% The following propositions are asserted:
%
% ```nquads
% NODE  rdfs:seeAlso  URI GRAPH .
% ```

rdfs_assert_seeAlso(S, Uri, G):-
  rdf_assert2(S, rdfs:seeAlso, Uri, G).



%! rdfs_assert_seeAlso(
%!   +Subject:or([bnode,iri]),
%!   +Uri:atom,
%!   ?Graph:atom
%! ) is det.
% The following propositions are asserted:
%
% ```nquads
% NODE  rdfs:seeAlso  URI GRAPH .
% ```

rdfs_assert_seeAlso(S, O, G):-
  rdf_assert2(S, rdfs:seeAlso, O, G).



%! rdfs_assert_subclass(+SubClass:iri, ?SuperClass:iri, ?Graph:atom) is det.
% Asserts the following propositions:
%
% ```nquads
% NODE  rdfs:subClassOf SUPERCLASS  GRAPH .
% ```
%
% If SuperClass is uninstantiated it defaults to `rdfs:Resource`.

rdfs_assert_subclass(C, D, G):-
  % Allow the superclass to be uninstantiated.
  rdf_defval(rdfs:'Resource', D),
  rdf_assert2(C, rdfs:subClassOf, D, G).



%! rdfs_assert_subproperty(
%!   +Subproperty:iri,
%!   ?SuperProperty:iri,
%!   ?Graph:atom
%! ) is det.
% Creates a new property that is a subproperty of the given parent property.
%
% The following propositions are asserted:
%
% ```nquads
% NODE  rdfs:subPropertyOf  SUPER-PROPERTY  GRAPH .
% ```
%
% If SuperProperty is uninstantiated it defaults to `rdf:Property`.

rdfs_assert_subproperty(P, Q, G):-
  rdf_defval(rdf:'Property', Q)
  rdf_assert2(P, rdfs:subPropertyOf, Q, G).



%! rdfs_retractall_class_resource(+Class:iri) is det.
% Removes the given class from the triple store.
%
% This is the same as removing class terms that are closed under identity.
%
% @see rdfs_retractall_class_term/1 removes class terms.

rdfs_retractall_class_resource(Class):-
  rdf_id(Class, Class0),
  rdfs_retractall_class_term(Class0).



%! rdfs_retractall_class_term(+Class:iri) is det.
% Removes the given class term from the triple store.
%
% This connects all subclasses of Class to all superclasses of Class.

rdfs_retractall_class_term(Class):-
  % [1] Remove the links to subclasses.
  %     Connect all subclasses of Class to all superclasses of Class.
  forall(
    (
      rdf(Subclass, rdfs:subClassOf, Class),
      rdf(Class, rdfs:subClassOf, Superclass)
    ),
    (
      % The transitive link is now a direct one.
      rdfs_assert_subclass(Subclass, Superclass, _),
      % Remove the link to a subclass.
      rdf_retractall(Subclass, rdfs:subClassOf, Class)
    )
  ),

  % [2] Remove the links to superclasses.
  rdf_retractall(Class, rdfs:subClassOf, _),

  % [3] Remove other triples in which the class occurs.
  rdf_retractall_term(Class, _).



%! rdfs_retractall_label_term(
%!   +Subject:or([bnode,iri]),
%!   ?Value,
%!   ?Graph:atom
%! ) is det.

rdfs_retractall_label(S, V, G):-
  (   ground(V)
  ->  (   V = _-_
      ->  rdf_equal(D, rdf:langString)
      ;   rdf_equal(D, xsd:string)
      )
  ;   true
  ),
  rdf_retractall_literal(S, rdfs:label, D, V, G).
