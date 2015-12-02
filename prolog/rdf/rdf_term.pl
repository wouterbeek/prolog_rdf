:- module(
  rdf_term,
  [
    rdf_bnode2/1, % ?BNode:bnode
    rdf_bnode2/2, % ?Graph:atom
                  % ?BNode:bnode
    rdf_datatype_iri/1, % ?Datatype:iri
    rdf_datatype_iri/2, % ?Graph:atom
                        % ?Datatype:iri
    rdf_is_iri/1, % @Term
    rdf_is_name/1, % @Term
    rdf_is_term/1, % @Term
    rdf_literal/1, % ?Literal:compound
    rdf_literal/2, % ?Graph:atom
                   % ?Literal:compound
    rdf_name/1, % ?Name:or([iri,literal])
    rdf_name/2, % ?Graph:atom
                % ?Name:or([iri,literal])
    rdf_node2/1, % ?Node:rdf_term
    rdf_node2/2, % ?Graph:atom
                 % ?Node:rdf_term
    rdf_object/1, % ?Object:rdf_term
    rdf_object/2, % ?Graph:atom
                  % ?Object:rdf_term
    rdf_predicate/1, % ?Predicate:iri
    rdf_predicate/2, % ?Graph:atom
                     % ?Predicate:iri
    rdf_subject/1, % ?Subject:rdf_term
    rdf_subject/2, % ?Graph:atom
                   % ?Subject:rdf_term
    rdf_term/1, % ?Term:rdf_term
    rdf_term/2 % ?Graph:atom
               % ?Term:rdf_term
  ]
).

/** <module> RDF term

Support for RDF 1.1 terms.

### rdf_is_resource/1

The predicate rdf_is_resource/1 in library(semweb/rdf_db) is quite misleading.
A first mistake one may make is to think that this predicate is about
semantics (resources being objects) while it actually is about syntax
(RDF terms that are either IRIs or blank nodes).
A second mistake one may make is to assume that rdf_is_resource/1 will
succeed for precisely those syntactic constructs that have a resource as
their interpretation.
But this is not the case either, since typed literals are mapped onto
resources as well.

---

@author Wouter Beek
@compat RDF 1.1 Concepts and Abstract Syntax
@see http://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/
@version 2015/07-2015/08, 2015/10, 2015/12
*/

:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_literal)).
:- use_module(library(rdf/rdf_read)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(solution_sequences)).
:- use_module(library(typecheck)).

:- rdf_meta(rdf_datatype_iri(r)).
:- rdf_meta(rdf_datatype_iri(?,r)).
:- rdf_meta(rdf_is_iri(r)).
:- rdf_meta(rdf_is_name(o)).
:- rdf_meta(rdf_is_term(o)).
:- rdf_meta(rdf_literal(o)).
:- rdf_meta(rdf_literal(?,o)).
:- rdf_meta(rdf_name(o)).
:- rdf_meta(rdf_name(?,o)).
:- rdf_meta(rdf_node2(o)).
:- rdf_meta(rdf_node2(?,o)).
:- rdf_meta(rdf_object(o)).
:- rdf_meta(rdf_object(?,o)).
:- rdf_meta(rdf_predicate(r)).
:- rdf_meta(rdf_predicate(?,r)).
:- rdf_meta(rdf_term(o)).
:- rdf_meta(rdf_term(?,o)).





%! rdf_bnode2(+BNode:bnode) is semidet.
%! rdf_bnode2(-BNode:bnode) is nondet.
% Enumerates the current blank node terms.
% Ensures that no blank node occurs twice.
%
% @bug Unfortunate naming due to conflict with rdf_bnode/1 in rdf_db.pl.

rdf_bnode2(B):-
  rdf_node2(B),
  rdf_is_bnode(B).



%! rdf_bnode2(+Graph:atom, +BNode:bnode) is semidet.
%! rdf_bnode2(+Graph:atom, -BNode:bnode) is nondet.
%! rdf_bnode2(-Graph:atom, +BNode:bnode) is nondet.
%! rdf_bnode2(-Graph:atom, -BNode:bnode) is nondet.

rdf_bnode2(G, B):-
  rdf_node2(G, B),
  rdf_is_bnode(B).



%! rdf_datatype_iri(+Datatype:iri) is semidet.
%! rdf_datatype__iri(-Datatype:iri) is nondet.

rdf_datatype_iri(D):-
  distinct(D, (
    rdf_literal(Lit),
    rdf_literal_data(datatype, Lit, D)
  )).


%! rdf_datatype_iri(+Graph:atom, +Datatype:iri) is semidet.
%! rdf_datatype_iri(+Graph:atom, -Datatype:iri) is nondet.

rdf_datatype_iri(G, D):-
  distinct(D, (
    rdf_literal(G, Lit),
    rdf_literal_data(datatype, Lit, D)
  )).



%! rdf_is_iri(@Term) is semidet.
% Succeeds for atoms that conform to the syntactic requirement of being
% an IRI RDF term.
%
% This does not imply that the term occurs in an actual triple or graph.

rdf_is_iri(X):-
  is_iri(X).



%! rdf_is_name(@Term) is semidet.
% Succeeds if the given term is syntactically an RDF name,
%  i.e., it need not be present in any current triple.

rdf_is_name(X):-
  rdf_is_literal(X), !.
rdf_is_name(X):-
  rdf_is_iri(X).



%! rdf_is_term(@Term) is semidet.
% Succeeds if the given term is syntactically an RDF term,
%  i.e., it need not be present in any current triple.

rdf_is_term(X):-
  rdf_is_name(X), !.
rdf_is_term(X):-
  rdf_is_bnode(X).



%! rdf_literal(@Term) is semidet.
%! rdf_literal(-Literal:compound) is nondet.

rdf_literal(Lit):-
  var(Lit), !,
  rdf_current_literal(Lit0),
  Lit = literal(Lit0).
rdf_literal(Lit):-
  Lit = literal(Lit0),
  rdf_current_literal(Lit0).



%! rdf_literal(+Graph:atom, +Literal:compound) is semidet.
%! rdf_literal(+Graph:atom, -Literal:compound) is nondet.
%! rdf_literal(-Graph:atom, +Literal:compound) is nondet.
%! rdf_literal(-Graph:atom, -Literal:compound) is nondet.

rdf_literal(G, Lit):-
  rdf_object(G, Lit),
  rdf_is_literal(Lit).



%! rdf_name(+Name:or([iri,literal])) is semidet.
%! rdf_name(-Name:or([iri,literal])) is nondet.

rdf_name(Name):-
  rdf_term(Name),
  \+ rdf_is_bnode(Name).


%! rdf_name(+Graph:atom, +Name:or([iri,literal])) is semidet.
%! rdf_name(+Graph:atom, -Name:or([iri,literal])) is nondet.
%! rdf_name(-Graph:atom, +Name:or([iri,literal])) is nondet.
%! rdf_name(-Graph:atom, -Name:or([iri,literal])) is nondet.

rdf_name(G, Name):-
  rdf_term(G, Name),
  \+ rdf_is_bnode(Name).



%! rdf_node2(+Node:rdf_term) is semidet.
%! rdf_node2(-Node:rdf_term) is nondet.

rdf_node2(S):-
  user:rdf_subject(S).
rdf_node2(O):-
  rdf_object(O),
  % Make sure there are no duplicates.
  \+ user:rdf_subject(O).


%! rdf_node2(+Graph:atom, +Node:rdf_term) is semidet.
%! rdf_node2(+Graph:atom, -Node:rdf_term) is nondet.
%! rdf_node2(-Graph:atom, +Node:rdf_term) is nondet.
%! rdf_node2(-Graph:atom, -Node:rdf_term) is nondet.

rdf_node2(G, S):-
  rdf_subject(G, S).
rdf_node2(G, O):-
  rdf_object(G, O),
  % Make sure there are no duplicates.
  \+ rdf_subject(G, O).



%! rdf_object(+Object:rdf_term) is semidet.
%! rdf_object(-Object:rdf_term) is nondet.

rdf_object(O):-
  rdf_literal(O),
  % Excludes cases in which a literal only appears in deleted statements.
  user:rdf(_, _, O).
rdf_object(O):-
  rdf_resource(O),
  user:rdf(_, _, O).


%! rdf_graph(+Graph:atom, +Object:rdf_term) is semidet.
%! rdf_graph(+Graph:atom, -Object:rdf_term) is nondet.
%! rdf_graph(-Graph:atom, +Object:rdf_term) is nondet.
%! rdf_graph(-Graph:atom, -Object:rdf_term) is nondet.

rdf_object(G, O):-
  rdf_literal(O),
  % Also excludes cases in which a literal only appears in deleted statements.
  user:rdf(_, _, O, G).
rdf_object(G, O):-
  rdf_resource(O),
  user:rdf(_, _, O, G).



%! rdf_predicate(+Predicate:iri) is semidet.
%! rdf_predicate(-Predicate:iri) is nondet.
% @bug Terminologically more consistent than Semweb's rdf_current_predicate/1.

rdf_predicate(P):-
  rdf_current_predicate(P).


%! rdf_predicate(+Graph:atom, +Predicate:iri) is semidet.
%! rdf_predicate(+Graph:atom, -Predicate:iri) is nondet.

rdf_predicate(G, P):-
  rdf_predicate(P),
  user:rdf(_, P, _, G).



%! rdf_subject(+Graph:atom, +Subject:rdf_term) is semidet.
%! rdf_subject(+Graph:atom, -Subject:rdf_term) is nondet.
%! rdf_subject(-Graph:atom, +Subject:rdf_term) is nondet.
%! rdf_subject(-Graph:atom, -Subject:rdf_term) is nondet.

rdf_subject(G, S):-
  user:rdf_subject(S),
  user:rdf(S, _, G).



%! rdf_term(@Term) is semidet.
%! rdf_term(-Term:rdf_term) is nondet.

rdf_term(P):-
  rdf_predicate(P).
rdf_term(Node):-
  rdf_node2(Node),
  % Ensure there are no duplicates.
  \+ rdf_predicate(Node).


%! rdf_term(+Graph:atom, +Term:rdf_term) is semidet.
%! rdf_term(+Graph:atom, -Term:rdf_term) is nondet.
%! rdf_term(-Graph:atom, +Term:rdf_term) is nondet.
%! rdf_term(-Graph:atom, -Term:rdf_term) is nondet.

rdf_term(G, P):-
  rdf_predicate(G, P).
rdf_term(G, Node):-
  rdf_node2(G, Node),
  % Ensure there are no duplicates.
  \+ rdf_predicate(Node).
