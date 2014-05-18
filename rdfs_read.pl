:- module(
  rdfs_read,
  [
    rdfs/4, % ?Subject:or([bnode,iri])
            % ?Predicate:iri
            % ?Object:or([bnode,iri,literal])
            % ?Graph:atom
    rdfs_class/3, % +Mode:compound
                  % ?Class:iri
                  % ?Graph:atom
    rdfs_domain/4, % +Mode:compound
                   % ?Property:iri
                   % ?Domain:iri
                   % ?Graph:atom
    rdfs_individual/4, % +Mode:compound
                       % ?Individual:iri
                       % ?Class:iri
                       % ?Graph:atom
    rdfs_property/3, % +Mode:compound
                     % ?RDFS_Property:iri
                     % ?Graph:atom
    rdfs_range/4, % +Mode:compound
                  % ?Property:iri
                  % ?Range:iri
                  % ?Graph:atom
    rdfs_subclass/4, % +Mode
                     % ?Subclass:iri
                     % ?Superclass:iri
                     % ?Graph:atom
    rdfs_subproperty/4 % +Mode:compound
                       % ?RDFS_Subproperty:iri
                       % ?RDFS_Superproperty:iri
                       % ?Graph:atom
  ]
).

/** <module> RDFS read

Examples of deductions that are incorrect in SWI-Prolog Semweb v3.

~~~
rdfs_individual_of(rdfs:Datatype,X).
+rdfs:Resource
-rdfs:Class

rdfs_individual_of(X, rdfs:'Class')
-rdf:Alt
-rdf:Bag
-rdf:List
-rdf:Property
-rdf:Seq
-rdf:Statement
-rdf:XMLLiteral
-rdfs:Class
-rdfs:Container
-rdfs:ContainerMembershipProperty
-rdfs:Datatype
-rdfs:Literal
-rdfs:Property
-rdfs:Resource

rdfs_individual_of(rdfs:'Class', X)
-rdfs:Class
+rdfs:Resource

rdfs_individual_of(rdfs:Class, rdfs:Class)
~false
~~~

## Loops

If the RDF(S) specifications are straightforwardly implemented,
a few loops occur in deduction. We enumerate these loops.

### RDFS10-RDFS9 loop

Example:
~~~
[RDFS 10] <rdfs:Dataype, rdf:type, rdfs:Datatype> if
          <rdfs:Dataype, rdf:type, rdfs:Class>
[RDFS 9]  <rdfs:Dataype, rdf:type, rdfs:Class> if
          <X, rdfs:subClassOf, rdfs:Class>
          <rdfs:Datatype, rdf:type, X>
[AXIOM]   [X/rdfs:Datatype]
~~~

This is solved by constraining RDFS 9 to non-reflexive
applications of the subclass relation.

## RDFS9-RDFS11-RDFS10 loop

Example:
~~~
[RDFS 9]  <rdfs:Datatype, rdf:type, rdfs:Class> if
          <X, rdfs:subClassOf, rdfs:Class>
          <rdfs:Datatype, rdf:type, X>
[RDFS 11] <X, rdfs:subClassOf, rdfs:Class> if
          <Y, rdfs:subClassOf, rdfs:Class>
          <X, rdfs:subClassOf, Y>
[AXIOM]   [Y/rdfs:Datatype]
[RDFS 10] [X/rdfs:Datatype] if
          <rdfs:Datatype, rdf:type, rdfs:Class>
~~~

This is solved by constraining RDFS 11 to non-reflexive
application of the subclass relation.

@author Wouter Beek
@tbd How to materialize the membership properties (an infinite lot of them)?
@version 2011/08-2012/03, 2012/09, 2012/11-2013/03, 2013/07-2013/09, 2014/02-2014/03
*/

:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_container)).
:- use_module(rdf(rdf_read)).
:- use_module(rdf_reasoning(rdf_bnode_map)).
:- use_module(rdf_term(rdf_typed_literal)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
:- xml_register_namespace(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').

:- rdf_meta(rdf_db_or_axiom(+,r,r,r,?)).
:- rdf_meta(rdfs(r,r,r,?)).
:- rdf_meta(rdfs_class(+,r,?)).
:- rdf_meta(rdfs_domain(+,r,r,?)).
:- rdf_meta(rdfs_domain_axiom(+,r,r)).
:- rdf_meta(rdfs_individual(+,r,r,?)).
:- rdf_meta(rdfs_individual_axiom(+,r,r)).
:- rdf_meta(rdfs_property(+,r,?)).
:- rdf_meta(rdfs_property_axiom(+,r,r)).
:- rdf_meta(rdfs_range(+,r,r,?)).
:- rdf_meta(rdfs_range_axiom(+,r,r)).
:- rdf_meta(rdfs_subclass(+,r,r,?)).
:- rdf_meta(rdfs_subclass_axiom(+,r,r)).
:- rdf_meta(rdfs_subproperty(+,r,r,?)).



rdf_db_or_axiom(M, P, rdfs:domain, C, _):-
  rdfs_domain_axiom(M, P, C),
  debug(rdfs_read, '[AX] ~w DOMAIN ~w', [P,C]).
rdf_db_or_axiom(M, I, rdf:type, C, _):-
  rdfs_individual_axiom(M, I, C),
  debug(rdfs_read, '[AX] ~w IN ~w', [I,C]).
rdf_db_or_axiom(M, P, rdfs:range, C, _):-
  rdfs_range_axiom(M, P, C),
  debug(rdfs_read, '[AX] ~w RANGE ~w', [P,C]).
rdf_db_or_axiom(M, P1, rdfs:subPropertyOf, P2, _):-
  rdfs_subproperty_axiom(M, P1, P2),
  debug(rdfs_read, '[AX] ~w SUBPROP ~w', [P1,P2]).
rdf_db_or_axiom(M, C1, rdfs:subClassOf, C2, _):-
  rdfs_subclass_axiom(M, C1, C2),
  debug(rdfs_read, '[AX] ~w SUBCLASS ~w', [C1,C2]).
rdf_db_or_axiom(_, S, P, O, G):-
  rdf([graph_mode(no_index)], S, P, O, G),
  debug(rdfs_read, '[DB] ~w ~w ~w', [S,P,O]).

rdfs(S, P, O, G):-
  rdfs_subproperty(m(t,f,f), SubP, P, G),
  rdf(S, SubP, O, G).

% This circumvents RDFS 9&10 loops.
rdfs_class(m(t,_,_), C, _):-
  nonvar(C),
  rdf_equal(rdfs:'Class', C), !.
rdfs_class(M, C, G):-
  nonvar(C), !,
  rdfs_individual(M, C, rdfs:'Class', G), !.
rdfs_class(M, C, G):-
  rdfs_individual(M, C, rdfs:'Class', G).

rdfs_domain(M, P, C, G):-
  rdf_db_or_axiom(M, P, rdfs:domain, C, G).
% EXT 1
rdfs_domain(M, P, C, G):- M=m(t,t,_),
  rdf_db_or_axiom(M, P, rdfs:domain, C0, G),
  rdfs_subclass(M, C0, C, G),
  debug(rdfs_read, '[EXT 1] ~w DOMAIN ~w', [P,C]).
% EXT 3
rdfs_domain(M, P, C, G):- M=m(t,t,_),
  (
    nonvar(P)
  ->
    rdfs_subproperty(M, P, P0, G),
    rdfs_domain(M, P0, C, G)
  ;
    rdfs_domain(M, P0, C, G),
    rdfs_subproperty(M, P, P0, G)
  ),
  debug(rdfs_read, '[EXT 3] ~w DOMAIN ~w', [P,C]).

% RDFS axioms: class
rdfs_domain_axiom(m(t,_,_), P, C):-
  rdf_equal(rdfs:subClassOf, P),
  rdf_equal(rdfs:'Class', C).
% RDFS axioms: list
rdfs_domain_axiom(m(t,_,_), P, C):-
  ( rdf_equal(rdf:first, P)
  ; rdf_equal(rdf:rest,  P)
  ),
  rdf_equal(rdf:'List', C).
% RDFS axioms: property
rdfs_domain_axiom(m(t,_,_), P, C):-
  ( rdf_equal(rdfs:domain,        P)
  ; rdf_equal(rdfs:range,         P)
  ; rdf_equal(rdfs:subPropertyOf, P)
  ),
  rdf_equal(rdf:'Property', C).
% RDFS axioms: resource
rdfs_domain_axiom(m(t,_,_), P, C):-
  ( rdf_equal(rdfs:comment,     P)
  ; rdf_equal(rdfs:isDefinedBy, P)
  ; rdf_equal(rdfs:label,       P)
  ; rdf_equal(rdfs:member,      P)
  ; rdf_equal(rdfs:seeAlso,     P)
  ; rdf_equal( rdf:type,        P)
  ; rdf_equal(rdfs:value,       P)
  ),
  rdf_equal(rdfs:'Resource', C).
% RDFS axioms: statement
rdfs_domain_axiom(m(t,_,_), P, C):-
  ( rdf_equal(rdf:subject,   P)
  ; rdf_equal(rdf:object,    P)
  ; rdf_equal(rdf:predicate, P)
  ),
  rdf_equal(rdf:'Statement', C).

rdfs_individual(M, I, C, G):-
  rdf_db_or_axiom(M, I, rdf:type, C, G).
% RDF 1
rdfs_individual(M, I, C, G):-
  rdf_equal(rdf:'Property', C),
  rdf_db_or_axiom(M, _, I, _, G),
  debug(rdfs_read, '[RDF 1] ~w IN ~w', [I,C]).
% RDF 2
% @tbd Blank nodes are needed here.
%rdfs_individual(M, I, C, G):-
%  rdf_db_or_axiom(M, _, _, I, G),
%  rdf_equal(rdf:'XMLLiteral', C),
%  debug(rdfs_read, '[RDF 2] ~w IN ~w', [I,C]).
% RDFS 1
% @tbd Blank nodes are needed here.
%rdfs_individual(M, I, C, G):- M=m(t,_,_),
%  rdf_db_or_axiom(M, _, _, I, G),
%  rdf_is_literal(I),
%  rdf_equal(rdfs:'Literal', C),
%  debug(rdfs_read, '[RDFS 1] ~w IN ~w', [I,C]).
% RDFS 2
rdfs_individual(M, I, C, G):- M=m(t,_,_),
  rdfs_domain(M, P, C, G),
  rdf_db_or_axiom(M, I, P, _, G),
  debug(rdfs_read, '[RDFS 2] ~w IN ~w', [I,C]).
% RDFS 3
rdfs_individual(M, I, C, G):- M=m(t,_,_),
  rdfs_range(M, P, C, G),
  rdf_db_or_axiom(M, _, P, I, G),
  % The object term should not be a literal.
  \+ rdf_is_literal(I),
  debug(rdfs_read, '[RDFS 3] ~w IN ~w', [I,C]).
% RDFS 4a
rdfs_individual(M, I, C, G):- M=m(t,_,_),
  rdf_equal(rdfs:'Resource', C),
  rdf_db_or_axiom(M, I, _, _, G),
  debug(rdfs_read, '[RDFS 4a] ~w IN ~w', [I,C]).
% RDFS 4b
rdfs_individual(M, I, C, G):- M=m(t,_,_),
  rdf_equal(rdfs:'Resource', C),
  rdf_db_or_axiom(M, _, _, I, G),
  % Literals are resources, but this is not deduced by this rule (see RDFS 1).
  \+ rdf_is_literal(I),
  debug(rdfs_read, '[RDFS 4b] ~w IN ~w', [I,C]).
% RDFS 9
rdfs_individual(M, I, C, G):- M=m(t,_,_),
  rdfs_subclass(r(f,_), M, C0, C, G),
  \+ rdf_same(C0, C),
  rdfs_individual(M, I, C0, G),
  debug(rdfs_read, '[RDFS 9] ~w IN ~w', [I,C]).
% RDFD 1
rdfs_individual(M, LexicalForm, Datatype, G):- M=m(t,_,t),
  rdf_db_or_axiom(M, _, _, TypedLiteral, G),
  rdf_typed_literal(G, TypedLiteral),
  rdf_typed_literal(TypedLiteral, LexicalForm, Datatype),
  rdfs_individual(M, Datatype, rdfs:'Datatype', G),
  debug(rdfs_read, '[RDFD 1] ~w IN ~w', [LexicalForm,Datatype]).

% RDF axioms: list.
rdfs_individual_axiom(_, I, C):-
  rdf_equal(rdf:nil, I),
  rdf_equal(rdf:'List', C).
% RDF axioms: property.
rdfs_individual_axiom(_, I, C):-
  ( rdf_equal(rdf:first,     I)
  ; rdf_equal(rdf:object,    I)
  ; rdf_equal(rdf:predicate, I)
  ; rdf_equal(rdf:rest,      I)
  ; rdf_equal(rdf:subject,   I)
  ; rdf_equal(rdf:type,      I)
  ; rdf_equal(rdf:value,     I)
  ; nonvar(I),
    rdf_container_membership_property(I, _)
  ;
    var(I),
    % The `var` case would introduce an infinite number of axioms,
    % so we restrict it.
    between(1, 3, N),
    format(atom(Name), '_~w', [N]),
    rdf_global_id(rdf:Name, I)
  ),
  rdf_equal(rdf:'Property', C).

rdfs_property(M, P, G):-
  rdfs_individual(M, P, rdf:'Property', G).

rdfs_range(M, P, C, G):-
  rdf_db_or_axiom(M, P, rdfs:range, C, G).
% EXT 2
rdfs_range(M, P, C, G):- M=m(t,t,_),
  rdfs_range(M, P, C0, G),
  rdfs_subclass(M, C0, C, G),
  debug(rdfs_read, '[EXT 2] ~w RANGE ~w', [P,C]).
% EXT 4
rdfs_range(M, P, C, G):- M=m(t,t,_),
  rdfs_range(M, P0, C, G),
  rdfs_subproperty(M, P, P0, G),
  debug(rdfs_read, '[EXT 4] ~w RANGE ~w', [P,C]).

% RDFS axioms: class
rdfs_range_axiom(m(t,_,_), P, C):-
  ( rdf_equal(rdfs:domain,     P)
  ; rdf_equal(rdfs:subClassOf, P)
  ; rdf_equal( rdf:type,       P)
  ; rdf_equal(rdfs:range,      P)
  ),
  rdf_equal(rdfs:'Class', C).
% RDFS axioms: list.
rdfs_range_axiom(m(t,_,_), P, C):-
  rdf_equal(rdf:rest, P),
  rdf_equal(rdf:'List', C).
% RDFS axioms: literal.
rdfs_range_axiom(m(t,_,_), P, C):-
  ( rdf_equal(rdfs:comment, P)
  ; rdf_equal(rdfs:label,   P)
  ),
  rdf_equal(rdfs:'Literal', C).
% RDFS axioms: resource.
rdfs_range_axiom(m(t,_,_), P, C):-
  ( rdf_equal( rdf:first,       P)
  ; rdf_equal(rdfs:isDefinedBy, P)
  ; rdf_equal(rdfs:member,      P)
  ; rdf_equal( rdf:object,      P)
  ; rdf_equal( rdf:predicate,   P)
  ; rdf_equal(rdfs:seeAlso,     P)
  ; rdf_equal( rdf:subject,     P)
  ; rdf_equal( rdf:value,       P)
  ; nonvar(P),
    rdf_container_membership_property(P, _)
  ; var(P),
    % The `var` case would introduce an infinite number of axioms,
    % so we restrict it.
    between(1, 3, N),
    format(atom(Name), '_~w', [N]),
    rdf_global_id(rdf:Name, P)
  ),
  rdf_equal(rdfs:'Resource', C).

%! rdf_same(
%!   +Resource1:or([bnode,iri,literal]),
%!   +Resource1:or([bnode,iri,literal])
%! ) is semidet.
% For transitive hierarchic relations, we want to make sure that
% some of their deduction rules (RDFS 9, RDFS 11, RDFS 13) do not consider
% reflexive pairs of the relations which would cause loops.
% (We guarantee that the reflexive cases are covered by other rules.)
%
% Semantic identity in RDF is not syntactic identity,
% since simple entailment allows blank nodes to stand for
% resources. This predicate checks for identity while taking the
% blank node mappings into account.

rdf_same(X, X):- !.
rdf_same(X, Y):-
  rdf_is_bnode(X),
  b2r(_, X, Y).
rdf_same(X, Y):-
  rdf_is_bnode(Y),
  b2r(_, Y, X).

rdfs_subclass(M, C1, C2, G):-
  rdfs_subclass(r(t,t), M, C1, C2, G).

rdfs_subclass(_R, M, C1, C2, G):-
  rdf_db_or_axiom(M, C1, rdfs:subClassOf, C2, G).
% RDFS 8
rdfs_subclass(_R, M, C1, C2, G):- M=m(t,_,_),
  % Resource instantiation comes first, otherwise there may be an
  % RDFS8-RDFS9 loop.
  rdf_equal(rdfs:'Resource', C2),
  rdfs_class(M, C1, G),
  debug(rdfs_read, '[RDFS 8] ~w SUBCLASS ~w', [C1,C2]).
% RDFS 10
rdfs_subclass(r(t,_RP), M, C, C, G):- M=m(t,_,_),
  rdfs_class(M, C, G),
  debug(rdfs_read, '[RDFS 10] ~w SUBCLASS ~w', [C,C]).
% RDFS 11
rdfs_subclass(r(_RC,RP), M, C1, C2, G):- M=m(t,_,_),
  (
    nonvar(C1)
  ->
    rdf_db_or_axiom(M, C1, rdfs:subClassOf, C3, G),
    \+ rdf_same(C1, C3),
    rdfs_subclass(r(f,RP), M, C3, C2, G)
  ;
    rdf_db_or_axiom(M, C3, rdfs:subClassOf, C2, G),
    \+ rdf_same(C3, C2),
    rdfs_subclass(r(f,RP), M, C1, C3, G)
  ),
  debug(rdfs_read, '[RDFS 11] ~w SUBCLASS ~w', [C1,C2]).
% RDFS 13
rdfs_subclass(_R, M, C1, C2, G):- M=m(t,_,_),
  % Resource instantiation comes first, otherwise there may be an
  % RDFS13-RDFS9 loop.
  rdf_equal(rdfs:'Literal', C2),
  rdfs_individual(M, C1, rdfs:'Datatype', G),
  debug(rdfs_read, '[RDFS 13] ~w SUBCLASS ~w', [C1,C2]).
% EXT 5
rdfs_subclass(_R, M, C1, C2, G):- M=m(t,t,_),
  rdf_equal(rdfs:'Resource', C1),
  rdfs_subproperty(M, rdf:type, P, G),
  rdfs_domain(M, P, C2, G),
  debug(rdfs_read, '[EXT 5] ~w SUBCLASS ~w', [C1,C2]).
% EXT 6
rdfs_subclass(_R, M, C1, C2, G):- M=m(t,t,_),
  rdf_equal(rdfs:'Class', C1),
  rdfs_subproperty(M, rdfs:subClassOf, P, G),
  rdfs_domain(M, P, C2, G),
  debug(rdfs_read, '[EXT 6] ~w SUBCLASS ~w', [C1,C2]).
% EXT 7
rdfs_subclass(_R, M, C1, C2, G):- M=m(t,t,_),
  rdf_equal(rdfs:'Property', C1),
  rdfs_subproperty(M, rdfs:subPropertyOf, P, G),
  rdfs_domain(M, P, C2, G),
  debug(rdfs_read, '[EXT 7] ~w SUBCLASS ~w', [C1,C2]).
% EXT 8
rdfs_subclass(_R, M, C1, C2, G):- M=m(t,t,_),
  rdf_equal(rdfs:'Class', C1),
  rdfs_subproperty(M, rdfs:subClassOf, P, G),
  rdfs_range(M, P, C2, G),
  debug(rdfs_read, '[EXT 8] ~w SUBCLASS ~w', [C1,C2]).
% EXT 9
rdfs_subclass(_R, M, C1, C2, G):- M=m(t,t,_),
  rdf_equal(rdfs:'Property', C1),
  rdfs_subproperty(M, rdfs:subPropertyOf, P, G),
  rdfs_range(M, P, C2, G),
  debug(rdfs_read, '[EXT 9] ~w SUBCLASS ~w', [C1,C2]).

% RDFS axioms: class
rdfs_subclass_axiom(m(t,_,_), C1, C2):-
  rdf_equal(rdfs:'Datatype', C1),
  rdf_equal(rdfs:'Class',    C2).
% RDFS axioms: container
rdfs_subclass_axiom(m(t,_,_), C1, C2):-
  ( rdf_equal(rdf:'Alt', C1)
  ; rdf_equal(rdf:'Bag', C1)
  ; rdf_equal(rdf:'Seq', C1)
  ),
  rdf_equal(rdfs:'Container', C2).
% RDFS axioms: literal
rdfs_subclass_axiom(m(t,_,_), C1, C2):-
  rdf_equal( rdf:'XMLLiteral', C1),
  rdf_equal(rdfs:'Literal',    C2).
% RDFS axioms: property
rdfs_subclass_axiom(m(t,_,_), C1, C2):-
  rdf_equal(rdfs:'ContainerMembershipProperty', C1),
  rdf_equal( rdf:'Property', C2).

rdfs_subproperty(M, P1, P2, G):-
  rdfs_subproperty(r(t,t), M, P1, P2, G).

rdfs_subproperty(_R, M, P1, P2, G):-
  rdf_db_or_axiom(M, P1, rdfs:subPropertyOf, P2, G).
% RDFS 5
rdfs_subproperty(r(RC,_RP), M, P1, P2, G):- M=m(t,_,_),
  rdf_db_or_axiom(M, P1, rdfs:subPropertyOf, P3, G),
  \+ rdf_same(P1, P3),
  rdfs_subproperty(r(RC,f), M, P3, P2, G),
  debug(rdfs_read, '[RDFS 5] ~w SUBPROP ~w', [P1,P2]).
% RDFS 6
rdfs_subproperty(r(_RC,t), M, P, P, G):- M=m(t,_,_),
  rdfs_property(M, P, G),
  debug(rdfs_read, '[RDFS 6] ~w SUBPROP ~w', [P,P]).
% RDFS 12
rdfs_subproperty(_R, M, P1, P2, G):- M=m(t,_,_),
  rdf_equal(rdfs:member, P2),
  rdfs_individual(M, P1, rdfs:'ContainerMembershipProperty', G),
  debug(rdfs_read, '[RDFS 12] ~w SUBPROP ~w', [P1,P2]).

% RDFS axioms: see also
rdfs_subproperty_axiom(m(t,_,_), P1, P2):-
  rdf_equal(rdfs:isDefinedBy, P1),
  rdf_equal(rdfs:seeAlso,     P2).

