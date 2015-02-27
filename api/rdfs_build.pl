:- module(
  rdfs_build,
  [
    rdfs_assert_class/2, % +Subject:or([bnode,iri])
                         % ?Graph:atom
    rdfs_assert_comment/3, % +Resource:rdf_term
                           % +Comment:atom
                           % ?Graph:atom
    rdfs_assert_domain/3, % +Property:or([bnode,iri])
                          % ?Class:iri
                          % ?Graph:atom
    rdfs_assert_domain_range/3, % +Property:or([bnode,iri])
                                % ?Class:iri
                                % ?Graph:atom
    rdfs_assert_instance/2, % +Instance:or([bnode,iri])
                            % ?Graph:atom
    rdfs_assert_isDefinedBy/2, % +Subject:or([bnode,iri])
                               % +Graph:atom
    rdfs_assert_isDefinedBy/3, % +Subject:or([bnode,iri])
                               % ?Uri:atom
                               % ?Graph:atom
    rdfs_assert_label/3, % +Subject:or([bnode,iri])
                         % +Label
                         % ?Graph:graph
    rdfs_assert_label/4, % +Subject:or([bnode,iri])
                         % +Label
                         % ?Graph:graph
                         % -Triple:compound
    rdfs_assert_property_class/2, % +PropertyClass:iri
                                  % ?Graph:atom
    rdfs_assert_range/3, % +Property:or([bnode,iri])
                         % ?Class:iri
                         % ?Graph:atom
    rdfs_assert_seeAlso/3, % +Subject:or([bnode,iri])
                           % +Uri:atom
                           % +Graph:atom
    rdfs_assert_subclass/3, % +Class:or([bnode,iri])
                            % ?Superclass:iri
                            % ?Graph:atom
    rdfs_assert_subproperty/3, % +Subproperty:or([bnode,iri])
                               % +SuperProperty:iri
                               % ?Graph:atom
    %rdfs_update_label/5, % +Subject:or([bnode,iri])
    %                     % +Label:atom
    %                     % ?LangTag:list(atom)
    %                     % ?Graph:atom
    %                     % +Action:compound
    rdfs_retractall_class_resource/1, % +Class:iri
    rdfs_retractall_class_term/1, % +Class:iri
    rdfs_retractall_label/3 % +Subject:or([bnode,iri])
                            % ?Value
                            % ?Graph:atom
  ]
).

/** <module> RDF API: Build lower-level descriptions in RDFS

Predicates for asseritng RDFS statements in an easy way.

@author Wouter Beek
@version 2011/08, 2012/01, 2012/03, 2012/09, 2012/11-2013/02, 2013/05-2013/06,
         2014/03, 2014/08, 2014/11-2014/12, 2015/02
*/

:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdf_read)).
:- use_module(plRdf(api/rdfs_build)).
:- use_module(plRdf(management/rdf_prefix)).
:- use_module(plRdf(term/rdf_term)).

:- rdf_meta(rdfs_assert_class(r,?)).
:- rdf_meta(rdfs_assert_comment(r,+,?)).
:- rdf_meta(rdfs_assert_domain(r,r,?)).
:- rdf_meta(rdfs_assert_domain_range(r,r,?)).
:- rdf_meta(rdfs_assert_instance(r,?)).
:- rdf_meta(rdfs_assert_isDefinedBy(r,+)).
:- rdf_meta(rdfs_assert_isDefinedBy(r,?,?)).
:- rdf_meta(rdfs_assert_label(r,+,?)).
:- rdf_meta(rdfs_assert_label(r,+,?,-)).
:- rdf_meta(rdfs_assert_property_class(r,?)).
:- rdf_meta(rdfs_assert_range(r,r,?)).
:- rdf_meta(rdfs_assert_seeAlso(r,+,?)).
:- rdf_meta(rdfs_assert_string(r,r,+,?)).
:- rdf_meta(rdfs_assert_subclass(r,r,?)).
:- rdf_meta(rdfs_assert_subproperty(r,r,?)).
%:- rdf_meta(rdfs_update_label(r,+,?,?,+)).
:- rdf_meta(rdfs_retractall_class_resource(r)).
:- rdf_meta(rdfs_retractall_class_term(r)).
:- rdf_meta(rdfs_retractall_label(r,?,?)).





%! rdfs_assert_class(+Subject:or([bnode,iri]), ?Graph:atom) is det.
% Asserts the following propositions:
%
% ```nquads
% NODE  rdf:type        rdfs:Class    GRAPH .
% NODE  rdfs:subClassOf rdfs:Resource GRAPH .
% ```

rdfs_assert_class(Class, G):-
  rdf_assert_instance(Class, rdfs:'Class', G),
  rdfs_assert_subclass(Class, rdfs:'Resource', G).



%! rdfs_assert_comment(
%!   +Subject:or([bnode,iri]),
%!   +Comment,
%!   ?Graph:atom
%! ) is det.

% Without a language tag the comment is asserted as XSD string.
rdfs_assert_comment(S, Value, G):-
  Value = _-_, !,
  rdf_assert_langstring(S, rdfs:comment, Value, G).
% With a language tag the comment is asserted as RDF langString.
rdfs_assert_comment(S, Value, G):-
  rdfs_assert_comment(S, [en,'US']-Value, G).



%! rdfs_assert_domain(
%!   +Subject:or([bnode,iri]),
%!   ?Class:iri,
%!   ?Graph:atom
%! ) is det.
% Asserts the following propositions:
%
% ```nquads
% NODE  rdfs:domain CLASS GRAPH .
% ```
%
% Default class: `rdfsResource`.

rdfs_assert_domain(Property, Class, G):-
  (   var(Class)
  ->  rdf_equal(Class, rdfs:'Resource')
  ;   true
  ),
  rdf_assert2(Property, rdfs:domain, Class, G).



%! rdfs_assert_domain_range(
%!   +Subject:or([bnode,iri]),
%!   ?Class:iri,
%!   ?Graph:atom
%! ) is det.

rdfs_assert_domain_range(S, Class, G):-
  rdfs_assert_domain(S, Class, G),
  rdfs_assert_range(S, Class, G).



%! rdfs_assert_instance(+Subject:or([bnode,iri]), ?Graph:atom) is det.
% Asserts the following propositions:
%
% ```nquads
% NODE  rdf:type  rdfs:Resource GRAPH .
% ```

rdfs_assert_instance(S, G):-
  rdf_assert_instance(S, rdfs:'Resource', G).



%! rdfs_assert_isDefinedBy(+Subject:or([bnode,iri]), +Graph:atom) is det.

rdfs_assert_isDefinedBy(S, G):-
  rdf_global_id(Prefix:_, S),
  rdfs_assert_isDefinedBy(S, Prefix, G).



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
% If the given RDF term is a literal, then rdfs:isDefinedBy is asserted of its
%  assigned blank node.
%
% If Uri is uninstantiated, the IRI denoted by the registered RDF prefix
%  of Term is used, if any.

rdfs_assert_isDefinedBy(S, Uri, G):-
  var(Uri), !,
  rdf_prefix_iri(S, Uri),
  rdfs_assert_isDefinedBy(S, Uri, G).
rdfs_assert_isDefinedBy(S, Uri, G):-
  rdf_assert2(S, rdfs:isDefinedBy, Uri, G).



%! rdfs_assert_label(+Subject:or([bnode,iri]), +Label, ?Graph:atom) is det.

rdfs_assert_label(S, Label, G):-
  rdfs_assert_label(S, Label, G, _).

%! rdfs_assert_label(
%!   +Subject:or([bnode,iri]),
%!   +Label,
%!   ?Graph:atom,
%!   -Triple:compound
%! ) is det.
% Assigns an RDFS label to the resource denoted by the given RDF term.
%
% This predicate stores the label as an RDF language-tagged string.
% The default language is `en-US`.

% Labels without language tag are asserted as `xsd:string`.
rdfs_assert_label(S, Label, G, Triple):-
  Label = _-_, !,
  rdf_assert_langstring(S, rdfs:label, Label, G, Triple).
% Labels with language tag are asserted as `rdf:langString`.
rdfs_assert_label(S, Label, G, Triple):-
  rdfs_assert_label(S, [en,'US']-Label, G, Triple).



%! rdfs_assert_property_class(+Subject:or([bnode,iri]), ?Graph:atom) is det.
% Asserts the following propositions:
%
% ```nquads
% NODE  rdf:type        rdfs:Class    GRAPH .
% NODE  rdfs:subClassOf rdf:Property  GRAPH.
% ```

rdfs_assert_property_class(S, G):-
  % Materialization would figure this one out as well.
  rdf_assert_instance(S, rdfs:'Class', G),
  rdfs_assert_subclass(S, rdf:'Property', G).



%! rdfs_assert_range(
%!   +Subject:or([bnode,iri]),
%!   ?Class:iri,
%!   ?Graph:atom
%! ) is det.
% Asserts the following propositions:
%
% ```nquads
% NODE  rdfs:range  CLASS GRAPH .
% ```
%
% Default class: `rdfsResource`.

rdfs_assert_range(Instance, Class, G):-
  (   var(Class)
  ->  rdf_equal(Class, rdfs:'Resource')
  ;   true
  ),
  rdf_assert2(Instance, rdfs:range, Class, G).



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



%! rdfs_assert_subclass(
%!   +Subject:or([bnode,iri]),
%!   ?Superclass:iri,
%!   ?Graph:atom
%! ) is det.
% Asserts the following propositions:
%
% ```nquads
% NODE  rdfs:subClassOf SUPERCLASS  GRAPH .
% ```
%
% If SuperClass is uninstantiated it defaults to `rdfs:Resource`.

rdfs_assert_subclass(S, Superclass, G):-
  % Allow the superclass to be uninstantiated.
  (   var(Superclass)
  ->  rdf_equal(Superclass, rdfs:'Resource')
  ;   true
  ),
  rdf_assert2(S, rdfs:subClassOf, Superclass, G).



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
% NODE  rdf:type            rdf:Property    GRAPH .
% NODE  rdfs:subPropertyOf  SUPER-PROPERTY  GRAPH .
% ```

rdfs_assert_subproperty(Subproperty, SuperProperty, G):-
  rdf_assert2(Subproperty, rdfs:subPropertyOf, SuperProperty, G).



/*
%! rdfs_update_label(
%!   +Subject:or([bnode,iri]),
%!   +Label:atom,
%!   ?LangTag:list(atom),
%!   ?Graph:atom,
%!   +Action:compound
%! ) is det.
% @tbd

rdfs_update_label(S, Label, LangTag, G, Action):-
  rdfs_update_literal(S, Label, _, LangTag, G, Action).
*/



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

rdfs_retractall_label(S, Value, G):-
  rdf_retractall_literal(S, rdfs:label, Value, _, G).

