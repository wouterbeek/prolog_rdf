:- module(
  rdfs_build,
  [
    rdfs_assert_class/5, % +Class:iri
                         % ?Parent:or([iri,list(iri)])
                         % ?Label:or([atom,pair(atom)])
                         % ?Comment:or([atom,pair(atom)])
                         % ?Graph:rdf_graph
    rdfs_assert_comment/3, % +Subject:rdf_term
                           % +Comment:or([atom,pair(atom)])
                           % ?Graph:rdf_graph
    rdfs_assert_domain/3, % +Property:iri
                          % +Domain:rdf_term
                          % ?Graph:rdf_graph
    rdfs_assert_isDefinedBy/2, % +Subject, ?Graph
    rdfs_assert_isDefinedBy/3, % +Subject:rdf_term
                               % ?Uri:atom
                               % ?Graph:rdf_graph
    rdfs_assert_label/2, % +Subject, +Label
    rdfs_assert_label/3, % +Subject:rdf_term
                         % +Label:or([atom,pair(atom)])
                         % ?Graph:rdf_graph
    rdfs_assert_property/4, % +Domain:rdf_term
                            % +Property:iri
                            % +Range:rdf_term
                            % ?Graph:rdf_graph
    rdfs_assert_range/3, % +Property:iri
                         % +Range:rdf_term
                         % ?Graph:rdf_graph
    rdfs_assert_seeAlso/3, % +Subject:rdf_term
                           % +Uri:atom
                           % +Graph:rdf_graph
    rdfs_assert_subclass/2, % +Class, ?Graph
    rdfs_assert_subclass/3, % +Class:iri
                            % ?ParentClass:or([iri,list(iri)])
                            % ?Graph:rdf_graph
    rdfs_assert_subproperty/3, % +Subproperty:or([bnode,iri])
                               % +ParentProperty:or([iri,list(iri)])
                               % ?Graph:rdf_graph
    rdfs_retractall_class/1, % +Class:iri
    rdfs_retractall_label/3 % +Subject:rdf_term
                            % ?Label:or([atom,pair(atom)])
                            % ?Graph:rdf_graph
  ]
).

/** <module> RDFS build

Predicates for asseritng RDFS statements in an easy way.

@author Wouter Beek
@version 2015/07-2015/09, 2015/12
*/

:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_default)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_read)).

:- rdf_meta(rdfs_assert_class(r,t,?,?,r)).
:- rdf_meta(rdfs_assert_comment(o,+,r)).
:- rdf_meta(rdfs_assert_domain(r,r,r)).
:- rdf_meta(rdfs_assert_isDefinedBy(o,r)).
:- rdf_meta(rdfs_assert_isDefinedBy(o,?,r)).
:- rdf_meta(rdfs_assert_label(o,+)).
:- rdf_meta(rdfs_assert_label(o,+,r)).
:- rdf_meta(rdfs_assert_property(r,r,r,r)).
:- rdf_meta(rdfs_assert_range(r,r,r)).
:- rdf_meta(rdfs_assert_seeAlso(o,+,r)).
:- rdf_meta(rdfs_assert_subclass(r,r)).
:- rdf_meta(rdfs_assert_subclass(r,t,r)).
:- rdf_meta(rdfs_assert_subproperty(r,t,r)).
:- rdf_meta(rdfs_retractall_class(o)).
:- rdf_meta(rdfs_retractall_label(o,?,r)).





%! rdfs_assert_class(
%!   +Class:iri,
%!   ?Parent:or([iri,list(iri)]),
%!   ?Label:or([atom,pair(atom)]),
%!   ?Comment:or([atom,pair(atom)]),
%!   ?Graph:rdf_graph
%! ) is det.

rdfs_assert_class(C, Parent, Lbl, Comm, G):-
  rdf_assert_instance(C, rdfs:'Class', G),
  rdfs_assert_class0(C, Parent, Lbl, Comm, G).



%! rdfs_assert_comment(
%!   +Subject:rdf_term,
%!   +Comment:or([atom,pair(atom)]),
%!   ?Graph:rdf_graph
%! ) is det.

% Without a language tag the comment is asserted as XSD string.
rdfs_assert_comment(S, V, G):-
  V = _-_, !,
  rdf_assert_literal(S, rdfs:comment, rdf:langString, V, G).
% With a language tag the comment is asserted as RDF langString.
rdfs_assert_comment(S, V, G):-
  rdf_assert_literal(S, rdfs:comment, xsd:string, V, G).



%! rdfs_assert_domain(
%!   +Property:iri,
%!   +Domain:rdf_term,
%!   ?Graph:rdf_graph
%! ) is det.
% Asserts the following propositions:
%
% ```nquads
% 〈NODE, rdfs:domain, CLASS, GRAPH〉
% ```

rdfs_assert_domain(P, D, G):-
  rdf_assert(P, rdfs:domain, D, G).



%! rdfs_assert_isDefinedBy(+Subject:rdf_term, ?Graph:rdf_graph) is det.

rdfs_assert_isDefinedBy(S, G):-
  rdfs_assert_isDefinedBy(S, _, G).


%! rdfs_assert_isDefinedBy(
%!   +Subject:rdf_term,
%!   ?Iri:iri,
%!   ?Graph:rdf_graph
%! ) is det.
% Asserts the following propositions:
%
% ```nquads
% 〈NODE, rdfs:isDefinedBy, NAMESPACE, GRAPH〉
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
  rdf_assert(S, rdfs:isDefinedBy, O, G).
rdfs_assert_isDefinedBy(S, O, G):-
  rdf_assert(S, rdfs:isDefinedBy, O, G).



%! rdfs_assert_label(+Subject:rdf_term, +Label:or([atom,pair(atom)])) is det.
% Wrapper around rdfs_assert_label/3 with uninstantiated graph.

rdfs_assert_label(S, V):-
  rdfs_assert_label(S, V, _).


%! rdfs_assert_label(
%!   +Subject:rdf_term,
%!   +Label:or([atom,pair(atom)]),
%!   ?Graph:rdf_graph
%! ) is det.
% Assigns an RDFS label to the resource denoted by the given RDF term.
%
% This predicate stores the label as an RDF language-tagged string.
% The default language is `en-US`.

% Labels with language tag are asserted as `rdf:langString`.
rdfs_assert_label(S, V, G):-
  V = _-_, !,
  rdf_assert_literal(S, rdfs:label, rdf:langString, V, G).
% Labels without language tag are asserted as `xsd:string`.
rdfs_assert_label(S, V, G):-
  rdf_assert_literal(S, rdfs:label, xsd:string, V, G).



%! rdfs_assert_property(
%!   +Domain:rdf_term,
%!   +Property:iri,
%!   +Range:rdf_term,
%!   ?Graph:rdf_graph
%! ) is det.
% Asserts the following propositions:
%
% ```nquads
% 〈NODE, rdfs:range, CLASS, GRAPH〉
% ```

rdfs_assert_property(D, P, R, G):-
  rdfs_assert_domain(P, D, G),
  rdfs_assert_range(P, R, G).



%! rdfs_assert_range(+Property:iri, +Range:rdf_term, ?Graph:rdf_graph) is det.
% Asserts the following propositions:
%
% ```nquads
% 〈NODE, rdfs:range, CLASS, GRAPH〉
% ```

rdfs_assert_range(P, R, G):-
  rdf_assert(P, rdfs:range, R, G).



%! rdfs_assert_seeAlso(+Subject:rdf_term, +Iri:atom, ?Graph:rdf_graph) is det.
% The following propositions are asserted:
%
% ```nquads
% 〈NODE, rdfs:seeAlso, IRI, GRAPH〉
% ```

rdfs_assert_seeAlso(S, O, G):-
  rdf_assert(S, rdfs:seeAlso, O, G).



%! rdfs_assert_subclass(+Class:iri, ?Graph:rdf_graph) is det.

rdfs_assert_subclass(C, G):-
  rdfs_assert_subclass(C, _, G).

%! rdfs_assert_subclass(
%!   +Class:iri,
%!   ?ParentClass:or([iri,list(iri)]),
%!   ?Graph:rdf_graph
%! ) is det.
% Asserts the following propositions:
%
% ```nquads
% 〈NODE, rdfs:subClassOf, PARENT, GRAPH〉
% ```
%
% If ParentClass is uninstantiated it defaults to `rdfs:Resource`.

rdfs_assert_subclass(C, D, G):-
  % Allow the parent class to be uninstantiated.
  (   var(D)
  ->  rdf_assert(C, rdfs:subClassOf, rdfs:'Resource', G)
  ;   is_list(D)
  ->  forall(member(D0, D), rdf_assert(C, rdfs:subClassOf, D0, G))
  ;   rdf_assert(C, rdfs:subClassOf, D, G)
  ).



%! rdfs_assert_subproperty(
%!   +Subproperty:iri,
%!   ?ParentProperties:or([iri,list(iri)]),
%!   ?Graph:rdf_graph
%! ) is det.
% Creates a new property that is a subproperty of the given parent property.
%
% The following propositions are asserted:
%
% ```nquads
% 〈NODE, rdfs:subPropertyOf, PARENT, GRAPH〉
% ```
%
% If ParentProperty is uninstantiated it defaults to `rdf:Property`.

rdfs_assert_subproperty(P, Qs, G):-
  is_list(Qs), !,
  forall(member(Q, Qs), rdfs_assert_subproperty(P, Q, G)).
rdfs_assert_subproperty(P, Q, G):-
  rdf_defval(rdf:'Property', Q),
  rdf_assert(P, rdfs:subPropertyOf, Q, G).



%! rdfs_retractall_class(+Class:iri) is det.
% Removes the given class from the triple store.
%
% This is the same as removing class terms that are closed under identity.
%
% This connects all subclasses of Class to all superclasses of Class.

rdfs_retractall_class(C):-
  % [1] Remove the links to subclasses.
  %     Connect all subclasses of Class to all superclasses of Class.
  forall(
    (
      rdf(SubC, rdfs:subClassOf, C),
      rdf(C, rdfs:subClassOf, SuperC)
    ),
    (
      % The transitive link is now a direct one.
      rdfs_assert_subclass(SubC, SuperC, _),
      % Remove the link to a subclass.
      rdf_retractall(SubC, rdfs:subClassOf, C)
    )
  ),

  % [2] Remove the links to superclasses.
  rdf_retractall(C, rdfs:subClassOf, _),

  % [3] Remove other triples in which the class occurs.
  rdf_retractall_term(C, _).



%! rdfs_retractall_label_term(
%!   +Subject:rdf_term,
%!   ?Label:or([atom,pair(atom)]),
%!   ?Graph:rdf_graph
%! ) is det.

rdfs_retractall_label(S, V, G):-
  (   ground(V)
  ->  (   V = _-_
      ->  rdf_expand_ct(rdf:langString, D)
      ;   rdf_expand_ct(xsd:string, D)
      )
  ;   true
  ),
  rdf_retractall_literal(S, rdfs:label, D, V, G).





% HELPERS %

rdfs_assert_class0(C, Parent, Lbl, Comm, G):-
  rdfs_assert_subclass(C, Parent, G),
  (var(Lbl) -> true ; rdfs_assert_label(C, Lbl, G)),
  (var(Comm) -> true ; rdfs_assert_comment(C, Comm, G)),
  rdfs_assert_isDefinedBy(C, G).
