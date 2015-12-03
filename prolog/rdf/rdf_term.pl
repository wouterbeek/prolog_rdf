:- module(
  rdf_term,
  [
    grdf_bnode/1, % ?BlankNode
    grdf_bnode/2, % ?Graph:rdf_graph
                  % ?BlankNode:bnode
    grdf_datatype_iri/1, % ?Datatype
    grdf_datatype_iri/2, % ?Graph:rdf_graph
                         % ?Datatype:iri
    grdf_literal/1, % ?Literal
    grdf_literal/2, % ?Graph:rdf_graph
                    % ?Literal:rdf_literal
    grdf_name/1, % ?Name
    grdf_name/2, % ?Graph:rdf_graph
                 % ?Name:rdf_name
    grdf_node/1, % ?Node
    grdf_node/2, % ?Graph:rdf_graph
                 % ?Node:rdf_term
    grdf_object/1, % ?Object
    grdf_object/2, % ?Graph:rdf_graph
                   % ?Object:rdf_term
    grdf_predicate/1, % ?Predicate
    grdf_predicate/2, % ?Graph:rdf_graph
                      % ?Predicate:iri
    grdf_subject/1, % ?Subject
    grdf_subject/2, % ?Graph:rdf_graph
                    % ?Subject:rdf_term
    grdf_term/1, % ?Term
    grdf_term/2, % ?Graph:rdf_graph
                 % ?Term:rdf_term
    rdf_is_name/1, % @Term
    rdf_is_term/1 % @Term
  ]
).

/** <module> Generalized RDF terms

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
:- use_module(library(solution_sequences)).
:- use_module(library(typecheck)).

:- rdf_meta(grdf_node(o)).
:- rdf_meta(grdf_node(r,o)).
:- rdf_meta(grdf_subject(o)).
:- rdf_meta(grdf_subject(r,o)).
:- rdf_meta(grdf_datatype_iri(r)).
:- rdf_meta(grdf_datatype_iri(r,r)).
:- rdf_meta(grdf_literal(o)).
:- rdf_meta(grdf_literal(r,o)).
:- rdf_meta(grdf_name(o)).
:- rdf_meta(grdf_name(r,o)).
:- rdf_meta(grdf_object(o)).
:- rdf_meta(grdf_object(r,o)).
:- rdf_meta(grdf_predicate(r)).
:- rdf_meta(grdf_predicate(r,r)).
:- rdf_meta(grdf_term(o)).
:- rdf_meta(grdf_term(r,o)).
:- rdf_meta(rdf_is_name(o)).
:- rdf_meta(rdf_is_term(o)).





%! grdf_bnode(+BNode:bnode) is semidet.
%! grdf_bnode(-BNode:bnode) is nondet.
% Enumerates the current blank node terms.
% Ensures that no blank node occurs twice.
%
% @bug Unfortunate naming due to conflict with rdf_bnode/1 in rdf_db.pl.

grdf_bnode(B):-
  grdf_node(B),
  rdf_is_bnode(B).



%! grdf_bnode(+Graph:rdf_graph, +BNode:bnode) is semidet.
%! grdf_bnode(+Graph:rdf_graph, -BNode:bnode) is nondet.
%! grdf_bnode(-Graph:rdf_graph, +BNode:bnode) is nondet.
%! grdf_bnode(-Graph:rdf_graph, -BNode:bnode) is nondet.

grdf_bnode(G, B):-
  grdf_node(G, B),
  rdf_is_bnode(B).



%! grdf_datatype_iri(+Datatype:iri) is semidet.
%! grdf_datatype_iri(-Datatype:iri) is nondet.

grdf_datatype_iri(D):-
  distinct(D, (
    grdf_literal(Lit),
    rdf_literal_data(datatype, Lit, D)
  )).


%! grdf_datatype_iri(+Graph:rdf_graph, +Datatype:iri) is semidet.
%! grdf_datatype_iri(+Graph:rdf_graph, -Datatype:iri) is nondet.

grdf_datatype_iri(G, D):-
  distinct(D, (
    grdf_literal(G, Lit),
    rdf_literal_data(datatype, Lit, D)
  )).



%! rdf_iri(@Term) is semidet.
%! rdf_iri(-Term) is semidet.

grdf_iri(T):-
  nonvar(T), !,
  is_iri(T),
  term_to_id(T, Tid),
  (rdf_current_predicate(Tid) ; rdf_resource(Tid)), !.
grdf_iri(P):-
  grdf_predicate(P).
grdf_iri(N):-
  grdf_resource(N),
  \+ rdf_is_bnode(N),
  % Avoid duplicates.
  \+ grdf_predicate(N).



%! rdf_is_name(@Term) is semidet.
% Succeeds if the given term is syntactically an RDF name,
%  i.e., it need not be present in any current triple.

rdf_is_name(Term):-
  is_of_type(rdf_name, Term).



%! rdf_is_term(@Term) is semidet.
% Succeeds if the given term is syntactically an RDF term,
%  i.e., it need not be present in any current triple.

rdf_is_term(Term):-
  is_of_type(rdf_term, Term).



%! grdf_literal(@Term) is semidet.
%! grdf_literal(-Literal:rdf_literal) is nondet.

grdf_literal(Lit):-
  var(Lit), !,
  rdf_current_literal(Lit0),
  Lit = literal(Lit0).
grdf_literal(Lit):-
  Lit = literal(Lit0),
  rdf_current_literal(Lit0).



%! grdf_literal(+Graph:rdf_graph, +Literal:compound) is semidet.
%! grdf_literal(+Graph:rdf_graph, -Literal:compound) is nondet.
%! grdf_literal(-Graph:rdf_graph, +Literal:compound) is nondet.
%! grdf_literal(-Graph:rdf_graph, -Literal:compound) is nondet.

grdf_literal(G, Lit):-
  grdf_object(G, Lit),
  rdf_is_literal(Lit).



%! grdf_name(+Name:rdf_name) is semidet.
%! grdf_name(-Name:rdf_name) is nondet.

grdf_name(Name):-
  grdf_term(Name),
  \+ rdf_is_bnode(Name).


%! grdf_name(+Graph:rdf_graph, +Name:rdf_name) is semidet.
%! grdf_name(+Graph:rdf_graph, -Name:rdf_name) is nondet.
%! grdf_name(-Graph:rdf_graph, +Name:rdf_name) is nondet.
%! grdf_name(-Graph:rdf_graph, -Name:rdf_name) is nondet.

grdf_name(G, Name):-
  grdf_term(G, Name),
  \+ rdf_is_bnode(Name).



%! grdf_node(+Node:rdf_node) is semidet.
%! grdf_node(-Node:rdf_node) is nondet.
%
% @bug Naming conflict with Semweb's rdf_node/1.

grdf_node(S):-
  grdf_subject(S).
grdf_node(O):-
  grdf_object(O),
  % Make sure there are no duplicates.
  \+ grdf_subject(O).


%! grdf_node(+Graph:rdf_graph, +Node:rdf_node) is semidet.
%! grdf_node(+Graph:rdf_graph, -Node:rdf_node) is nondet.
%! grdf_node(-Graph:rdf_graph, +Node:rdf_node) is nondet.
%! grdf_node(-Graph:rdf_graph, -Node:rdf_node) is nondet.

grdf_node(G, S):-
  grdf_subject(G, S).
grdf_node(G, O):-
  grdf_object(G, O),
  % Make sure there are no duplicates.
  \+ grdf_subject(G, O).



%! grdf_object(+Object:rdf_term) is semidet.
%! grdf_object(-Object:rdf_term) is nondet.

grdf_object(O):-
  grdf_literal(O),
  % Excludes cases in which a literal only appears in deleted statements.
  grdf(_, _, O).
grdf_object(O):-
  var(O), !,
  rdf_resource(Oid),
  \+ rdf_is_literal(Oid),
  grdf(_, _, Oid),
  id_to_term(Oid, O).
grdf_object(O):-
  term_to_id(O, Oid),
  rdf_resource(Oid),
  \+ rdf_is_literal(Oid).
  

%! grdf_object(+Graph:rdf_graph, +Object:rdf_term) is semidet.
%! grdf_object(+Graph:rdf_graph, -Object:rdf_term) is nondet.
%! grdf_object(-Graph:rdf_graph, +Object:rdf_term) is nondet.
%! grdf_object(-Graph:rdf_graph, -Object:rdf_term) is nondet.

grdf_object(G, O):-
  var(O), !,
  grdf(_, _, O, G),
  grdf_object(O).
grdf_object(G, O):-
  grdf_object(O),
  % Also excludes cases in which a literal only appears in deleted statements.
  grdf(_, _, O, G).



%! grdf_predicate(+Predicate:iri) is semidet.
%! grdf_predicate(-Predicate:iri) is nondet.
% @bug Terminologically more consistent than Semweb's rdf_current_predicate/1.

grdf_predicate(P):-
  rdf_current_predicate(P).


%! grdf_predicate(+Graph:rdf_graph, +Predicate:iri) is semidet.
%! grdf_predicate(+Graph:rdf_graph, -Predicate:iri) is nondet.

grdf_predicate(G, P):-
  grdf_predicate(P),
  grdf(_, P, _, G).



%! grdf_subject(+Subject:rdf_term) is semidet.
%! grdf_subject(-Subject:rdf_term) is nondet.

grdf_subject(S):-
  nonvar(S), !,
  (   rdf_is_literal(S)
  ->  literal_to_id(S, Sid),
      rdf_subject(Sid)
  ;   term_to_id(S, Sid),
      rdf_subject(Sid)
  ).
grdf_subject(S):-
  % NONDET
  rdf_subject(Sid),
  (   id_to_literal(Sid, S), !
      % NONDET
  ;   id_to_term(Sid, S)
  ).


%! grdf_subject(+Graph:rdf_graph, +Subject:rdf_term) is semidet.
%! grdf_subject(+Graph:rdf_graph, -Subject:rdf_term) is nondet.
%! grdf_subject(-Graph:rdf_graph, +Subject:rdf_term) is nondet.
%! grdf_subject(-Graph:rdf_graph, -Subject:rdf_term) is nondet.

grdf_subject(G, S):-
  nonvar(S), !,
  grdf_subject(S),
  grdf(S, _, G).
grdf_subject(G, S):-
  grdf(S, _, G),
  grdf_subject(S).



%! grdf_term(@Term) is semidet.
%! grdf_term(-Term:rdf_term) is nondet.

grdf_term(P):-
  grdf_predicate(P).
grdf_term(N):-
  grdf_node(N),
  % Ensure there are no duplicates.
  \+ grdf_predicate(N).


%! grdf_term(+Graph:rdf_graph, +Term:rdf_term) is semidet.
%! grdf_term(+Graph:rdf_graph, -Term:rdf_term) is nondet.
%! grdf_term(-Graph:rdf_graph, +Term:rdf_term) is nondet.
%! grdf_term(-Graph:rdf_graph, -Term:rdf_term) is nondet.

grdf_term(G, P):-
  grdf_predicate(G, P).
grdf_term(G, N):-
  grdf_node(G, N),
  % Ensure there are no duplicates.
  \+ grdf_predicate(N).
