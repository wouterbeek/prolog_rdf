:- module(
  rdfs_build,
  [
    rdfs_assert_class/2, % +Class:iri
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
    rdfs_remove_class_resource/1, % +Class:iri
    rdfs_remove_class_term/1 % +Class:iri
  ]
).

/** <module> RDF API: Build RDFS

Predicates for asseritng RDFS statements in an easy way.

@author Wouter Beek
@version 2011/08, 2012/01, 2012/03, 2012/09, 2012/11-2013/02, 2013/05-2013/06,
         2014/03, 2014/08, 2014/11
*/

:- use_module(library(semweb/rdf_db)).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdfs_read)).
:- use_module(plRdf(term/rdf_bnode_map)).
:- use_module(plRdf(term/rdf_language_tagged_string)).
:- use_module(plRdf(term/rdf_string)).

:- rdf_meta(rdfs_assert_class(r,?)).
:- rdf_meta(rdfs_assert_comment(r,+,?,?)).
:- rdf_meta(rdfs_assert_domain(r,r,?)).
:- rdf_meta(rdfs_assert_domain_range(r,r,?)).
:- rdf_meta(rdfs_assert_instance(r,?)).
:- rdf_meta(rdfs_assert_isDefinedBy(r,?,?)).
:- rdf_meta(rdfs_assert_property_class(r,?)).
:- rdf_meta(rdfs_assert_range(r,r,?)).
:- rdf_meta(rdfs_assert_seeAlso(r,+,?)).
:- rdf_meta(rdfs_assert_subclass(r,r,?)).
:- rdf_meta(rdfs_assert_subproperty(r,r,?)).
:- rdf_meta(rdfs_remove_class(r,?)).



%! rdfs_assert_class(+Class:iri, ?Graph:atom) is det.
% Asserts the following propositions:
%
% ```nquads
% CLASS rdf:type        rdfs:Class    GRAPH .
% CLASS rdfs:subClassOf rdfs:Resource GRAPH .
% ```

rdfs_assert_class(Class, Graph):-
  rdf_assert_instance(Class, rdfs:'Class', Graph),
  rdfs_assert_subclass(Class, rdfs:'Resource', Graph).



%! rdfs_assert_comment(
%!   +Term:rdf_term,
%!   +Comment:atom,
%!   ?LangTag:atom,
%!   ?Graph:atom
%! ) is det.

% Comments can be asserted of literals by mapping them onto blank nodes.
rdfs_assert_comment(Term, Comment, LangTag, Graph):-
  rdf_is_literal(Term), !,
  term_to_bnode(Graph, Term, BNode),
  rdfs_assert_comment(BNode, Comment, LangTag, Graph).
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



%! rdfs_assert_domain(+Property:iri, +Class:iri, ?Graph:atom) is det.
% Asserts the following propositions:
%
% ```nquads
% PROPERTY  rdfs:domain CLASS GRAPH .
% ```

rdfs_assert_domain(Property, Class, Graph):-
  rdf_assert(Property, rdfs:domain, Class, Graph).



%! rdfs_assert_domain_range(+Property:iri, +Class:iri, ?Graph:atom) is det.
% RDFS properties whose domain and range are the same RDFS class.
%
% Asserts the following propositions:
%
% ```nquads
% PROPERTY  rdfs:domain CLASS GRAPH .
% PROPERTY  rdfs:range  CLASS GRAPH .
% ```

rdfs_assert_domain_range(Property, Class, Graph):-
  rdfs_assert_domain(Property, Class, Graph),
  rdfs_assert_range(Property, Class, Graph).



%! rdfs_assert_instance(+Instance:iri, ?Graph:atom) is det.
% Asserts the following propositions:
%
% ```nquads
% INSTANCE  rdf:type  rdfs:Resource GRAPH .
% ```

rdfs_assert_instance(Instance, Graph):-
  rdf_assert_instance(Instance, rdfs:'Resource', Graph).



%! rdfs_assert_isDefinedBy(+Term:rdf_term, ?Uri:atom, ?Graph:atom) is det.
% Asserts the following propositions:
%
% ```nquads
% TERM  rdfs:isDefinedBy  NAMESPACE GRAPH .
% ```
%
% If the given RDF term is a literal, then rdfs:isDefinedBy is asserted of its
%  assigned blank node.
%
% If Uri is uninstantiated, the IRI denoted by the registered RDF prefix
%  of Term is used, if any.

rdfs_assert_isDefinedBy(Term, Uri, Graph):-
  rdfs_is_literal(Term), !,
  term_to_bnode(Term, BNode),
  rdfs_assert_isDefinedBy(BNode, Uri, Graph).
rdfs_assert_isDefinedBy(Term, Uri, Graph):-
  var(Uri), !,
  rdf_prefix_iri(Term, Uri),
  rdfs_assert_isDefinedBy(Term, Uri, Graph).
rdfs_assert_isDefinedBy(Term, Uri, Graph):-
  rdf_assert(Term, rdfs:isDefinedBy, Uri, Graph).



%! rdfs_assert_property_class(+PropertyClass:iri, ?Graph:atom) is det.
% Asserts the following propositions:
%
% ```nquads
% PROPERTY-CLASS  rdf:type        rdfs:Class    GRAPH .
% PROPERTY-CLASS  rdfs:subClassOf rdf:Property  GRAPH. 
% ```

rdfs_assert_property_class(PropertyClass, Graph):-
  % Materialization would figure this one out as well.
  rdf_assert_instance(PropertyClass, rdfs:'Class', Graph),
  rdfs_assert_subclass(PropertyClass, rdf:'Property', Graph).



%! rdfs_assert_range(+Property:iri, +Class:iri, ?Graph:atom) is det.
% Asserts the following propositions:
%
% ```nquads
% PROPERTY  rdfs:range  CLASS GRAPH .
% ```

rdfs_assert_range(Property, Class, Graph):-
  rdf_assert(Property, rdfs:range, Class, Graph).



%! rdfs_assert_seeAlso(+Term:rdf_term, +Uri:atom, ?Graph:atom) is det.
% The following propositions are asserted:
%
% ```nquads
% TERM  rdfs:seeAlso  URI GRAPH .
% ```

rdfs_assert_seeAlso(Term, Uri, Graph):-
  rdfs_is_literal(Term), !,
  term_to_bnode(Graph, Term, BNode),
  rdfs_assert_seeAlso(BNode, Uri, Graph).
rdfs_assert_seeAlso(Term, Uri, Graph):-
  rdf_assert(Term, rdfs:seeAlso, Uri, Graph).



%! rdfs_assert_subclass(+Subclass:iri, ?Superclass:iri, ?Graph:atom) is det.
% Asserts the following propositions:
%
% ```nquads
% SUB-CLASS rdfs:subClassOf SUPER-CLASS GRAPH .
% ```
%
% @arg Subclass   The subclass is the only required argument.
% @arg SuperClass If uninstantiated `rdfs:Class` is used.
% @arg Graph      If uninstantiated the proposition is asserted to no graph.

rdfs_assert_subclass(Subclass, Superclass, Graph):-
  % Allow the superclass to be uninstantiated.
  (   var(Superclass)
  ->  rdf_equal(Superclass, rdfs:'Class')
  ;   true
  ),
  rdf_assert(Class, rdfs:subClassOf, Superclass, Graph).



%! rdfs_assert_subproperty(
%!   +Subproperty:iri,
%!   +SuperProperty:iri,
%!   ?Graph:atom
%! ) is det.
% Creates a new property that is a subproperty of the given parent property.
%
% The following propositions are asserted:
%
% ```nquads
% SUB-PROPERTY  rdf:type            rdf:Property    GRAPH .
% SUB-PROPERTY  rdfs:subPropertyOf  SUPER-PROPERTY  GRAPH .
% ```

rdfs_assert_subproperty(Subproperty, SuperProperty, Graph):-
  rdf_assert(Subproperty, rdf:type, rdf:'Property', Graph),
  rdf_assert(Subproperty, rdfs:subPropertyOf, SuperProperty, Graph).



%! rdfs_remove_class_resource(+Class:iri) is det.
% Removes the given class from the triple store.
%
% This is the same as removing class terms that are closed under identity.
%
% @see rdfs_remove_class_term/1 removes class terms.

rdfs_remove_class_resource(Class):-
  rdf_id(Class, Class0),
  rdfs_remove_class_term(Class0).



%! rdfs_remove_class_term(+Class:iri) is det.
% Removes the given class term from the triple store.
%
% This connects all subclasses of Class to all superclasses of Class.

rdfs_remove_class_term(Class):-
  % [1] Remove the links to subclasses.
  %     Connect all subclasses of Class to all superclasses of Class.
  forall(
    (
      rdf(Subclass, rdfs:subClassOf, Class),
      rdf(Class, rdfs:subClassOf, Superclass)
    ),
    (
      % The transitive link is now a direct one.
      rdf_assert(Subclass, rdfs:subClassOf, Superclass)
      % Remove the link to a subclass.
      rdf_retractall(Subclass, rdfs:subClassOf, Class)
    )
  ),
  
  % [2] Remove the links to superclasses.
  rdf_retractall(Class, rdfs:subClassOf, _),
  
  % [3] Remove other triples in which the class occurs.
  rdf_remove_term(Class, _).
