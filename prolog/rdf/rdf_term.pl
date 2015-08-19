:- module(
  rdf_term,
  [
    rdf_bnode2/1, % ?BNode:bnode
    rdf_iri/1, % ?Iri:iri
    rdf_is_iri/1, % @Term
    rdf_is_name/1, % @Term
    rdf_is_term/1, % @Term
    rdf_literal/1, % @Term
    rdf_name/1, % ?Name:or([iri,literal])
    rdf_node2/1, % ?Node:rdf_term
    rdf_object/1, % ?Object:rdf_term
    rdf_predicate/1, % ?Predicate:iri
    rdf_term/1, % ?Term:rdf_term
    rdf_term/2 % ?Term:rdf_term
               % ?Graph:atom
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
@compat [RDF 1.1 Concepts and Abstract Syntax](http://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/)
@version 2015/07-2015/08
*/

:- use_module(library(rdf/rdf_read)).
:- use_module(library(typecheck)).

:- rdf_meta(rdf_iri(r)).
:- rdf_meta(rdf_is_iri(o)).
:- rdf_meta(rdf_is_name(o)).
:- rdf_meta(rdf_is_term(o)).
:- rdf_meta(rdf_literal(o)).
:- rdf_meta(rdf_name(o)).
:- rdf_meta(rdf_node2(o)).
:- rdf_meta(rdf_object(o)).
:- rdf_meta(rdf_predicate(r)).
:- rdf_meta(rdf_term(o)).
:- rdf_meta(rdf_term(o,?)).





%! rdf_bnode2(+BNode:bnode) is semidet.
%! rdf_bnode2(-BNode:bnode) is nondet.
% Enumerates the current blank node terms.
% Ensures that no blank node occurs twice.
%
% Unfortunate naming due to conflict with rdf_bnode/1 in rdf_db.pl.

rdf_bnode2(B):-
  rdf_resource(B),
  rdf_is_bnode(B).



%! rdf_iri(+Iri:iri) is semidet.
% Succeeds if the given RDF term is an IRI.
%! rdf_iri(+Iri:iri) is nondet.
% Enumerates the IRIs in the RDF store.
% May contain duplicates!

rdf_iri(X):-
  rdf_current_predicate(X).
rdf_iri(X):-
  rdf_resource(X),
  \+ rdf_is_bnode(X),
  \+ rdf_is_literal(X),
  \+ rdf_current_predicate(X).



%! rdf_is_iri(@Term) is semidet.
% Succeeds for atoms that conform to the syntactic requirement of being
% an IRI RDF term.
%
% This does not imply that the term occurs in an actual triple or graph.

rdf_is_iri(X):-
  is_of_type(uri, X).



%! rdf_is_name(@Term) is semidet.
% Succeeds if the given term is syntactically an RDF name,
%  i.e., it need not be present in any current triple.

rdf_is_name(X):-
  rdf_is_literal(X).
rdf_is_name(X):-
  rdf_is_iri(X).



%! rdf_is_term(@Term) is semidet.
% Succeeds if the given term is syntactically an RDF term,
%  i.e., it need not be present in any current triple.

rdf_is_term(X):-
  rdf_is_name(X).
rdf_is_term(X):-
  rdf_is_bnode(X).



%! rdf_literal(@Term) is semidet.
%! rdf_literal(-Literal:literal) is nondet.

rdf_literal(X):-
  rdf_current_literal(X).



%! rdf_name(+Name:or([iri,literal])) is semidet.
%! rdf_name(-Name:or([iri,literal])) is nondet.

rdf_name(X):-
  rdf_iri(X).
rdf_name(X):-
  rdf_literal(X).



%! rdf_node2(+Node:rdf_term) is semidet.
%! rdf_node2(-Node:rdf_term) is nondet.

rdf_node2(X):-
  rdf_subject(X).
rdf_node2(X):-
  rdf_object(X),
  \+ rdf_subject(X).



%! rdf_object(+Object:rdf_term) is semidet.
%! rdf_object(-Object:rdf_term) is nondet.

rdf_object(X):-
  rdf_current_literal(X).
rdf_object(X):-
  rdf_resource(X),
  rdf2(_, _, X).



%! rdf_predicate(+Predicate:iri) is semidet.
%! rdf_predicate(-Predicate:iri) is nondet.
% @see Terminological variant of rdf_current_predicate/1.

rdf_predicate(X):-
  rdf_current_predicate(X).



%! rdf_term(@Term) is semidet.
%! rdf_term(-Term:rdf_term) is nondet.

rdf_term(X):-
  rdf_predicate(X).
rdf_term(X):-
  rdf_node2(X).



%! rdf_term(+Term:rdf_term, +Graph:atom) is semidet.
%! rdf_term(+Term:rdf_term, -Graph:atom) is nondet.
%! rdf_term(-Term:rdf_term, +Graph:atom) is nondet.
%! rdf_term(-Term:rdf_term, -Graph:atom) is nondet.
% Pairs of graphs and terms that occur in that graph.
%
% Enumerates all RDF terms.
%
% A term is either a subject, predicate or object term
% in an RDF triple.
%
% ### Uniqueness
%
% Duplicates occur only for RDF terms that are
% an RDF node and an RDF predicate term.

rdf_term(X, G):-
  rdf_term(X),
  rdf_term0(X, G).

rdf_term0(X, G):-
  rdf2(X, _, _, G), !.
rdf_term0(X, G):-
  rdf2(_, X, _, G), !.
rdf_term0(X, G):-
  rdf2(_, _, X, G).
