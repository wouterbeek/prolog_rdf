:- module(
  rdfs_build,
  [
    rdfs_assert_class/2, % +Term:rdf_term
                         % ?Graph:atom
    rdfs_assert_comment/4, % +Resource:rdf_term
                           % +Comment:atom
                           % ?LanguageTag:atom
                           % ?Graph:atom
    rdfs_assert_domain/3, % +Property:iri
                          % +Class:iri
                          % ?Graph:atom
    rdfs_assert_domain_range/3, % +Property:iri
                                % +Class:iri
                                % ?Graph:atom
    rdfs_assert_instance/2, % +Instance:iri
                            % ?Graph:atom
    rdfs_assert_isDefinedBy/3, % +Term:rdf_term
                               % ?Uri:atom
                               % ?Graph:atom
    rdfs_assert_label/4, % +Term:rdf_term
                         % +Label:atom
                         % ?LangTag:atom
                         % ?Graph:graph
    rdfs_assert_property_class/2, % +PropertyClass:iri
                                  % ?Graph:atom
    rdfs_assert_range/3, % +Property:iri
                         % +Class:iri
                         % ?Graph:atom
    rdfs_assert_seeAlso/3, % +Term:rdf_term
                           % +Uri:atom
                           % +Graph:atom
    rdfs_assert_subclass/3, % +Class:iri
                            % ?Superclass:iri
                            % ?Graph:atom
    rdfs_assert_subproperty/3, % +Property:iri
                               % +SuperProperty:iri
                               % ?Graph:atom
    rdfs_update_label/5, % +Term:rdf_term
                         % +Label:atom
                         % ?LangTag:list(atom)
                         % ?Graph:atom
                         % +Action:compound
    rdfs_retractall_class_resource/1, % +Class:iri
    rdfs_retractall_class_term/1 % +Class:iri
    rdfs_retractall_label/4 % +Term:rdf_term
                            % ?Label:atom
                            % ?LangTag:list(atom)
                            % ?Graph:atom
  ]
).

/** <module> RDF API: Build lower-level descriptions in RDFS

Predicates for asseritng RDFS statements in an easy way.

@author Wouter Beek
@version 2011/08, 2012/01, 2012/03, 2012/09, 2012/11-2013/02, 2013/05-2013/06,
         2014/03, 2014/08, 2014/11
*/

:- use_module(library(semweb/rdf_db)).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdfs_read)).
:- use_module(plRdf(entailment/rdf_bnode_map)).
:- use_module(plRdf(term/rdf_language_tagged_string)).
:- use_module(plRdf(term/rdf_string)).

:- rdf_meta(rdfs_assert_class(o,?)).
:- rdf_meta(rdfs_assert_comment(o,+,?,?)).
:- rdf_meta(rdfs_assert_domain(o,r,?)).
:- rdf_meta(rdfs_assert_domain_range(o,r,?)).
:- rdf_meta(rdfs_assert_instance(o,?)).
:- rdf_meta(rdfs_assert_isDefinedBy(o,?,?)).
:- rdf_meta(rdfs_assert_label(o,+,?,?)).
:- rdf_meta(rdfs_assert_property_class(o,?)).
:- rdf_meta(rdfs_assert_range(o,r,?)).
:- rdf_meta(rdfs_assert_seeAlso(o,+,?)).
:- rdf_meta(rdfs_assert_subclass(o,r,?)).
:- rdf_meta(rdfs_assert_subproperty(o,r,?)).
:- rdf_meta(rdfs_update_label(o,+,?,?,+)).
:- rdf_meta(rdfs_retractall_class_resource(o)).
:- rdf_meta(rdfs_retractall_class_term(o)).
:- rdf_meta(rdfs_retractall_label(o,?,?,?)).



%! rdfs_assert_class(+Term:rdf_term, ?Graph:atom) is det.
% Asserts the following propositions:
%
% ```nquads
% NODE  rdf:type        rdfs:Class    GRAPH .
% NODE  rdfs:subClassOf rdfs:Resource GRAPH .
% ```

rdfs_assert_class(Term, Graph):-
  rdf_assert_instance(Term, rdfs:'Class', Graph),
  rdfs_assert_subclass(Term, rdfs:'Resource', Graph).



%! rdfs_assert_comment(
%!   +Term:rdf_term,
%!   +Comment:atom,
%!   ?LangTag:atom,
%!   ?Graph:atom
%! ) is det.

% Without a language tag the comment is asserted as XSD string.
rdfs_assert_comment(Term, Comment, LangTag, Graph):-
  var(LangTag), !,
  rdf_assert_string(Term, rdfs:comment, Comment, Graph).
% With a language tag the comment is asserted as RDF langString.
rdfs_assert_comment(Term, Comment, LangTag, Graph):-
  rdf_assert_language_tagged_string(
    Term,
    rdfs:comment,
    Comment,
    LangTag,
    Graph
  ).



%! rdfs_assert_domain(+Term:rdf_term, +Class:iri, ?Graph:atom) is det.
% Asserts the following propositions:
%
% ```nquads
% NODE  rdfs:domain CLASS GRAPH .
% ```

rdfs_assert_domain(Term, Class, Graph):-
  rdf_assert2(Term, rdfs:domain, Class, Graph).



%! rdfs_assert_domain_range(+Term:rdf_term, +Class:iri, ?Graph:atom) is det.
% RDFS properties whose domain and range are the same RDFS class.
%
% Asserts the following propositions:
%
% ```nquads
% NODE  rdfs:domain CLASS GRAPH .
% NODE  rdfs:range  CLASS GRAPH .
% ```

rdfs_assert_domain_range(Term, Class, Graph):-
  rdfs_assert_domain(Term, Class, Graph),
  rdfs_assert_range(Term, Class, Graph).



%! rdfs_assert_instance(+Term:rdf_term, ?Graph:atom) is det.
% Asserts the following propositions:
%
% ```nquads
% NODE  rdf:type  rdfs:Resource GRAPH .
% ```

rdfs_assert_instance(Term, Graph):-
  rdf_assert_instance(Term, rdfs:'Resource', Graph).



%! rdfs_assert_isDefinedBy(+Term:rdf_term, ?Uri:atom, ?Graph:atom) is det.
% Asserts the following propositions:
%
% ```nquads
% NODE  rdfs:isDefinedBy  NAMESPACE GRAPH .
% ```
%
% If the given RDF term is a literal, then rdfs:isDefinedBy is asserted of its
%  assigned blank node.
%
% If Uri is uninstantiated, the IRI denoted by the registered RDF prefix
%  of Term is used, if any.

rdfs_assert_isDefinedBy(Term, Uri, Graph):-
  var(Uri), !,
  rdf_prefix_iri(Term, Uri),
  rdfs_assert_isDefinedBy(Term, Uri, Graph).
rdfs_assert_isDefinedBy(Term, Uri, Graph):-
  rdf_assert2(Term, rdfs:isDefinedBy, Uri, Graph).



%! rdfs_assert_label(
%!   +Term:rdf_term,
%!   +Label:atom,
%!   ?LangTag:list(atom),
%!   ?Graph:atom
%! ) is det.
% Assigns an RDFS label to the resource denoted by the given RDF term.

% Labels without language tag are asserted as `xsd:string`.
rdfs_assert_label(Term, Label, LangTag, Graph):-
  var(LangTag), !,
  rdf_assert_string(Term, rdfs:label, Label, Graph).
% Labels with language tag are asserted as `rdf:langString`.
rdfs_assert_label(Term, Label, LangTag, Graph):-
  rdf_assert_language_tagged_string(Term, rdfs:label, Label, LangTag, Graph).



%! rdfs_assert_property_class(+Term:rdf_term, ?Graph:atom) is det.
% Asserts the following propositions:
%
% ```nquads
% NODE  rdf:type        rdfs:Class    GRAPH .
% NODE  rdfs:subClassOf rdf:Property  GRAPH. 
% ```

rdfs_assert_property_class(Term, Graph):-
  % Materialization would figure this one out as well.
  rdf_assert_instance(Term, rdfs:'Class', Graph),
  rdfs_assert_subclass(Term, rdf:'Property', Graph).



%! rdfs_assert_range(+Term:rdf_term, +Class:iri, ?Graph:atom) is det.
% Asserts the following propositions:
%
% ```nquads
% NODE  rdfs:range  CLASS GRAPH .
% ```

rdfs_assert_range(Term, Class, Graph):-
  rdf_assert2(Term, rdfs:range, Class, Graph).



%! rdfs_assert_seeAlso(+Term:rdf_term, +Uri:atom, ?Graph:atom) is det.
% The following propositions are asserted:
%
% ```nquads
% NODE  rdfs:seeAlso  URI GRAPH .
% ```

rdfs_assert_seeAlso(Term, Uri, Graph):-
  rdf_assert2(Term, rdfs:seeAlso, Uri, Graph).



%! rdfs_assert_subclass(+Term:rdf_term, ?Superclass:iri, ?Graph:atom) is det.
% Asserts the following propositions:
%
% ```nquads
% NODE  rdfs:subClassOf SUPERCLASS  GRAPH .
% ```

rdfs_assert_subclass(Term, Superclass, Graph):-
  % Allow the superclass to be uninstantiated.
  (   var(Superclass)
  ->  rdf_equal(Superclass, rdfs:'Class')
  ;   true
  ),
  rdf_assert2(Term, rdfs:subClassOf, Superclass, Graph).



%! rdfs_assert_subproperty(
%!   +Term:rdf_term,
%!   +SuperProperty:iri,
%!   ?Graph:atom
%! ) is det.
% Creates a new property that is a subproperty of the given parent property.
%
% The following propositions are asserted:
%
% ```nquads
% NODE  rdf:type            rdf:Property    GRAPH .
% NODE  rdfs:subPropertyOf  SUPER-PROPERTY  GRAPH .
% ```

rdfs_assert_subproperty(Term, SuperProperty, Graph):-
  rdf_assert2(Term, rdfs:subPropertyOf, SuperProperty, Graph).



%! rdfs_update_label(
%!   +Term:rdf_term,
%!   +Label:atom,
%!   ?LangTag:list(atom),
%!   ?Graph:atom,
%!   +Action:compound
%! ) is det.

rdfs_update_label(Term, Label, LangTag, Graph, Action):-
  rdfs_update_literal(Term, Label, _, LangTag, Graph, Action).



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
      rdfs_assert_subclass(Subclass, Superclass, _)
      % Remove the link to a subclass.
      rdf_retractall2(Subclass, rdfs:subClassOf, Class, _)
    )
  ),
  
  % [2] Remove the links to superclasses.
  rdf_retractall2(Class, rdfs:subClassOf, _, _),
  
  % [3] Remove other triples in which the class occurs.
  rdf_retractall_term(Class, _).



%! rdfs_retractall_label_term(
%!   +Term:rdf_term,
%!   ?Label:atom,
%!   ?LangTag:list(atom),
%!   ?Graph:atom
%! ) is det.

rdfs_retractall_label(Term, Label, LangTag, Graph):-
  rdf_retractall_literal(Term, rdfs:label, Label, _, LangTag, Graph).

