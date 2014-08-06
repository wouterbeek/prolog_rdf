:- module(
  rdf_term,
  [
    rdf_bnode/2, % ?BlankNode:bnode
                 % ?Graph:atom
    rdf_iri/1, % ?Iri:iri
    rdf_iri/2, % ?Iri:iri
               % ?Graph:atom
    rdf_name/2, % ?Name:oneof([iri,literal])
                % ?Graph:atom
    rdf_node/2, % ?RdfNode:or([bnode,iri,literal])
                % ?Graph:atom
    rdf_object/2, % ?Object:oneof([bnode,literal,iri])
                  % ?Graph:atom
    rdf_predicate/2, % ?Predicate:iri
                     % ?Graph:atom
    rdf_subject/2, % ?Subject:oneof([bnode,iri])
                   % ?Graph:atom
    rdf_term/2, % ?Term:or([bnode,iriliteral])
                % ?Graph:atom
    rdf_vocabulary/2 % +Graph:atom
                     % -Vocabulary:ordset(or([iri,literal]))
  ]
).

/** <module> RDF Term

Support for RDF 1.1 terms.
Support for RDF literals is found in [rdf_literal].

@author Wouter Beek
@see CyganiakWoodLanthaler2014
     RDF 1.1 Concepts and Abstract Syntax
     http://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/
@version 2012/01-2013/05, 2013/07-2013/08, 2014/01-2014/03, 2014/05
*/

:- use_module(library(aggregate)).
:- use_module(library(semweb/rdf_db)).

:- use_module(generics(typecheck)).

:- rdf_meta(rdf_iri(r)).
:- rdf_meta(rdf_iri(r,?)).
:- rdf_meta(rdf_name(r,?)).
:- rdf_meta(rdf_node(r,?)).
:- rdf_meta(rdf_object(r,?)).
:- rdf_meta(rdf_predicate(r,?)).
:- rdf_meta(rdf_subject(r,?)).
:- rdf_meta(rdf_term(r,?)).
:- rdf_meta(rdf_vocabulary(?,t)).



%! rdf_bnode(+BlankNode:bnode, +Graph:atom) is semidet.
%! rdf_bnode(-BlankNode:bnode, +Graph:atom) is nondet.
%! rdf_bnode(+BlankNode:bnode, -Graph:atom) is nondet.
%! rdf_bnode(-BlankNode:bnode, -Graph:atom) is nondet.
% Relates blank nodes to RDF graphs in which they occur.
% Ensures that no pairs occurs twice.

rdf_bnode(BNode, Graph):-
  % Enumerates RDF nodes.
  rdf_resource(BNode),
  rdf_is_bnode(BNode),
  % Relate the blank node to a graph.
  rdf_node(BNode, Graph).


%! rdf_iri(+Iri:iri) is semidet.
% Succeeds if the given RDF term is an IRI.
%! rdf_iri(+Iri:iri) is nondet.
% Enumerates the IRIs in the RDF store.

rdf_iri(Iri):-
  nonvar(Iri), !,
  rdf_is_iri(Iri).
rdf_iri(Iri):-
  rdf_iri(Iri, _).


%! rdf_iri(+Iri:iri, +Graph:atom) is semidet.
%! rdf_iri(-Iri:iri, +Graph:atom) is nondet.
%! rdf_iri(+Iri:iri, -Graph:atom) is nondet.
%! rdf_iri(-Iri:iri, -Graph:atom) is nondet.

rdf_iri(Iri, Graph):-
  rdf_resource(Iri),
  rdf_is_iri(Iri),
  rdf_term(Iri, Graph).


%! rdf_is_iri(+Term) is semidet.
% Succeeds for atoms that conform to the syntactic requirement of being
% an IRI RDF term.
%
% This does not imply that the term occurs in an actual triple or graph.
%
% @see The predicate rdf_is_resource/1 in library(semweb/rdf_db)
%      is quite misleading.
%      A first mistake one may make is to think that
%      this predicate is about semantics (resources being objects)
%      while it actually is about syntax
%      (RDF terms that are either IRIs or blank nodes).
%      A second mistake one may make is to assume that
%      rdf_is_resource/1 will succeed for precisely those
%      syntactic constructs that have a resource as their interpretation.
%      But this is not the case either,
%      since typed literals are mapped onto resources as well.

rdf_is_iri(IRI):-
  is_of_type(iri, IRI).


%! rdf_name(+Name:or([iri,literal]), +Graph:atom) is semidet.
%! rdf_name(-Name:or([iri,literal]), +Graph:atom) is nondet.
%! rdf_name(+Name:or([iri,literal]), -Graph:atom) is nondet.
%! rdf_name(-Name:or([iri,literal]), -Graph:atom) is nondet.
% RDF names are IRIs and RDF literals.
%
% ### Uniqueness
%
% We cannot ensure that all occurrences are unique.
% Specifically, we will return resources that are
% both predicates and nodes twice.
%
% ### Instatiations
%
% | ++ | semidet | Does this RDF graph contain this RDF name?              |
% | +- | nondet  | Enumerate the RDF names in this RDF graph.\c
%                  An RDF graph may have zero or more RDF names.           |
% | -+ | nondet  | Enumerate the RDF graphs in which this RDF name occurs. |
%                  An RDF name may occur nowhere.                          |
% | -- | nondet  | Enumerate pars of RDF graphs and RDF names.             |
%
% @arg Name Either an IRI or an RDF literal.
% @arg Graph The atomic name of an RDF graph.
%
% @see RDF Semantics http://www.w3.org/TR/2004/REC-rdf-mt-20040210/

rdf_name(Name, Graph):-
  nonvar(Name),
  nonvar(Graph), !,
  rdf_name_(Name, Graph), !.
rdf_name(Name, Graph):-
  rdf_name_(Name, Graph).

rdf_name_(Name, Graph):-
  (
    rdf_resource(Name)
  ;
    rdf_current_predicate(Name)
  ),
  % Exclude blank nodes.
  \+ rdf_is_bnode(Name),
  % Relate to an RDF graph.
  (
    rdf_subject(Name, Graph)
  ;
    rdf_predicate(Name, Graph)
  ;
    rdf_object(Name, Graph)
  ).


%! rdf_node(+RdfNode:or([bnode,iri,literal]), +Graph:atom) is semidet.
%! rdf_node(-RdfNode:or([bnode,iri,literal]), +Graph:atom) is nondet.
%! rdf_node(+RdfNode:or([bnode,iri,literal]), -Graph:atom) is nondet.
%! rdf_node(-RdfNode:or([bnode,iri,literal]), -Graph:atom) is nondet.
% Pairs of RDF nodes and RDF graphs in which they occur.
%
% The set of RDF nodes of an RDF graph is
% the set of subjects and objects of triples in the graph.
%
% It is possible for a predicate IRI to also occur as a node
% in the same graph.

% Semidet case.
rdf_node(Node, Graph):-
  nonvar(Node),
  nonvar(Graph), !,
  rdf_node_(Node, Graph), !.
% Nondet cases,
rdf_node(Node, Graph):-
  rdf_resource(Node),
  rdf_node_(Node, Graph).

rdf_node_(Node, Graph):-
  rdf_subject(Node, Graph).
rdf_node_(Node, Graph):-
  rdf_object(Node, Graph).


%! rdf_object(+Object:or([bnode,iri,literal]), +Graph:atom) is semidet.
%! rdf_object(-Object:or([bnode,iri,literal]), +Graph:atom) is nondet.
%! rdf_object(+Object:or([bnode,iri,literal]), -Graph:atom) is nondet.
%! rdf_object(-Object:or([bnode,iri,literal]), -Graph:atom) is nondet.

% Semidet case.
rdf_object(Object, Graph):-
  nonvar(Object),
  nonvar(Graph), !,
  rdf_object_(Object, Graph), !.
% Nondet cases.
rdf_object(Object, Graph):-
  rdf_object_(Object, Graph).

rdf_object_(Object, Graph):-
  rdf_resource(Object),
  rdf(_, _, Object, Graph).


%! rdf_predicate(+Predicate:iri, +Graph:atom) is semidet.
%! rdf_predicate(-Predicate:iri, +Graph:atom) is nondet.
%! rdf_predicate(+Predicate:iri, -Graph:atom) is nondet.
%! rdf_predicate(-Predicate:iri, -Graph:atom) is nondet.

% Semidet case.
rdf_predicate(Predicate, Graph):-
  nonvar(Predicate),
  nonvar(Graph), !,
  rdf_predicate_(Predicate, Graph), !.
% Nondet cases.
rdf_predicate(Predicate, Graph):-
  rdf_predicate_(Predicate, Graph).

rdf_predicate_(Predicate, Graph):-
  rdf_current_predicate(Predicate),
  once(rdf(_, Predicate, _, Graph)).


%! rdf_subject(+Subject:or([bnode,iri]), +Graph:atom) is semidet.
%! rdf_subject(-Subject:or([bnode,iri]), +Graph:atom) is nondet.
%! rdf_subject(+Subject:or([bnode,iri]), -Graph:atom) is nondet.
%! rdf_subject(-Subject:or([bnode,iri]), -Graph:atom) is nondet.
% Triples consist of the following kinds of terms:
%   * _Subject_, which is an RDF URI reference or a blank node
%   * _Predicate_, which is an RDF URI reference
%   * _Object_, which is an RDF URI reference, a literal or a blank node
%
% Terms can therefore be named by
%  the position in which they occur in a triple.
%
% Whether a term is either of these three is _relative_
%  to a given triple.
%
% These predicates check whether a term occurs in a graph;
%  relate terms to graphs and vice versa,
%  and generate the terms in a graphs
%  and the graphs in which a term occurs.

% Semidet case.
rdf_subject(Subject, Graph):-
  nonvar(Subject),
  nonvar(Graph), !,
  rdf_subject_(Subject, Graph), !.
% Nondet cases.
rdf_subject(Subject, Graph):-
  rdf_subject_(Subject, Graph).

rdf_subject_(Subject, Graph):-
  rdf_resource(Subject),
  once(rdf(Subject, _, _, Graph)).



%! rdf_term(+Term:or([bnode,iri,literal]), +Graph:atom) is semidet.
%! rdf_term(+Term:or([bnode,iri,literal]), -Graph:atom) is nondet.
%! rdf_term(-Term:or([bnode,iri,literal]), +Graph:atom) is nondet.
%! rdf_term(-Term:or([bnode,iri,literal]), -Graph:atom) is nondet.
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

% Semidet case.
rdf_term(Term, Graph):-
  nonvar(Term),
  nonvar(Graph), !,
  rdf_term_(Term, Graph), !.
% Nondet cases.
rdf_term(Term, Graph):-
  rdf_term_(Term, Graph).

rdf_term_(Term, Graph):-
  rdf_node(Term, Graph).
rdf_term_(Term, Graph):-
  rdf_predicate(Term, Graph).


%! rdf_vocabulary(+Graph:atom, -Vocabulary:ordset([literal,iri])) is det.
% Returns the vocabulary of the given graph.
%
% The vocabulary of a graph is the set of RDF names that occur
% in the triples of the graph.
%
% @see RDF Semantics http://www.w3.org/TR/2004/REC-rdf-mt-20040210/

rdf_vocabulary(Graph, Vocabulary):-
  aggregate_all(
    set(Name),
    rdf_name(Name, Graph),
    Vocabulary
  ).

