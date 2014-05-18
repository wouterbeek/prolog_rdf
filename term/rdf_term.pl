:- module(
  rdf_term,
  [
    rdf_bnode_/1, % ?BlankNode:bnode
    rdf_bnode/2, % ?BlankNode:bnode
                 % ?RdfGraph:atom
    rdf_iri/1, % ?Iri:iri
    rdf_iri/2, % ?Iri:iri
               % ?RdfGraph:atom
    rdf_name/1, % ?Name:oneof([iri,literal])
    rdf_name/2, % ?Name:oneof([iri,literal])
                % ?RdfGraph:atom
    rdf_node_/1, % ?RdfNode:or([bnode,iri,literal])
    rdf_node/2, % ?RdfNode:or([bnode,iri,literal])
                % ?RdfGraph:atom
    rdf_object/1, % ?Object:oneof([bnode,literal,iri])
    rdf_object/2, % ?Object:oneof([bnode,literal,iri])
                  % ?RdfGraph:atom
    rdf_predicate/1, % ?Predicate:iri
    rdf_predicate/2, % ?Predicate:iri
                     % ?RdfGraph:atom
    rdf_subject_/1, % ?Subject:oneof([bnode,iri])
    rdf_subject/2, % ?Subject:oneof([bnode,iri])
                   % ?RdfGraph:atom
    rdf_term/1, % ?Term:or([bnode,iriliteral])
    rdf_term/2, % ?Term:or([bnode,iriliteral])
                % ?RdfGraph:atom
    rdf_vocabulary/2 % +RdfGraph:atom
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
@version 2012/01-2013/05, 2013/07-2013/08, 2014/01-2014/03
*/

:- use_module(library(aggregate)).
:- use_module(library(semweb/rdf_db)).

:- use_module(generics(typecheck)).

:- rdf_meta(rdf_iri(r)).
:- rdf_meta(rdf_iri(r,?)).
:- rdf_meta(rdf_name(r)).
:- rdf_meta(rdf_name(r,?)).
:- rdf_meta(rdf_node_(r)).
:- rdf_meta(rdf_node(r,?)).
:- rdf_meta(rdf_object(r)).
:- rdf_meta(rdf_object(r,?)).
:- rdf_meta(rdf_predicate(r)).
:- rdf_meta(rdf_predicate(r,?)).
:- rdf_meta(rdf_subject_(r)).
:- rdf_meta(rdf_subject(r,?)).
:- rdf_meta(rdf_term(r)).
:- rdf_meta(rdf_term(r,?)).
:- rdf_meta(rdf_vocabulary(?,t)).



%! rdf_bnode_(+BlankNode:bnode) is semidet.
% Succeeds if the given RDF term is a blank node.
%! rdf_bnode_(-BlankNode:bnode) is nondet.
% Enumerates the blank nodes in the RDF store.

rdf_bnode_(BNode):-
  nonvar(BNode), !,
  rdf_is_bnode(BNode).
rdf_bnode_(BNode):-
  rdf_bnode(BNode, _).


%! rdf_bnode(+BlankNode:bnode, +RdfGraph:atom) is semidet.
%! rdf_bnode(-BlankNode:bnode, +RdfGraph:atom) is nondet.
%! rdf_bnode(+BlankNode:bnode, -RdfGraph:atom) is nondet.
%! rdf_bnode(-BlankNode:bnode, -RdfGraph:atom) is nondet.
% Relates blank nodes to RDF graphs in which they occur.
% Ensures that no pairs occurs twice.

rdf_bnode(BNode, G):-
  % Enumerates RDF nodes.
  rdf_resource(BNode),
  rdf_is_bnode(BNode),
  % Relate the blank node to a graph.
  (
    rdf_subject(BNode, G)
  ;
    rdf_object(BNode, G)
  ).


%! rdf_iri(+Iri:iri) is semidet.
% Succeeds if the given RDF term is an IRI.
%! rdf_iri(+Iri:iri) is nondet.
% Enumerates the IRIs in the RDF store.

rdf_iri(Iri):-
  nonvar(Iri), !,
  rdf_is_iri(Iri).
rdf_iri(Iri):-
  rdf_iri(Iri, _).


%! rdf_iri(+Iri:iri, +RdfGraph:atom) is semidet.
%! rdf_iri(-Iri:iri, +RdfGraph:atom) is nondet.
%! rdf_iri(+Iri:iri, -RdfGraph:atom) is nondet.
%! rdf_iri(-Iri:iri, -RdfGraph:atom) is nondet.

rdf_iri(Iri, G):-
  rdf_resource(Iri),
  rdf_is_iri(Iri),
  rdf_term(Iri, G).


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


%! rdf_name(+Name:or([iri,literal])) is semidet.
%! rdf_name(-Name:or([iri,literal])) is nondet.

rdf_name(Name):-
  nonvar(Name), !,
  rdf_is_name(Name).
rdf_name(Name):-
  rdf_name(Name, _).


%! rdf_name(+Name:or([iri,literal]), +RdfGraph:atom) is semidet.
%! rdf_name(-Name:or([iri,literal]), +RdfGraph:atom) is nondet.
%! rdf_name(+Name:or([iri,literal]), -RdfGraph:atom) is nondet.
%! rdf_name(-Name:or([iri,literal]), -RdfGraph:atom) is nondet.
% RDF names are IRIs and RDF literals.
%
% # Instatiations
%
% | ++ | semidet | Does this RDF graph contain this RDF name?              |
% | +- | nondet  | Enumerate the RDF names in this RDF graph.\c
%                  An RDF graph may have zero or more RDF names.           |
% | -+ | nondet  | Enumerate the RDF graphs in which this RDF name occurs. |
%                  An RDF name may occur nowhere.                          |
% | -- | nondet  | Enumerate pars of RDF graphs and RDF names.             |
%
% @arg Name Either an IRI or an RDF literal.
% @arg RdfGraph The atomic name of an RDF graph.
%
% @see RDF Semantics http://www.w3.org/TR/2004/REC-rdf-mt-20040210/

rdf_name(Name, G):-
  % Enumerate all subject, predicate, and object terms.
  (
    rdf_resource(Name)
  ;
    rdf_current_predicate(Name)
  ),
  % Exclude blank nodes.
  \+ rdf_is_bnode(Name),
  % Relate to an RDF graph.
  (
    rdf_subject(Name, G)
  ;
    rdf_predicate(Name, G)
  ;
    rdf_object(Name, G)
  ).


%! rdf_is_name(+Term) is semidet.
% Succeeds if the given term is an RDF name.

rdf_is_name(Name):-
  rdf_is_literal(Name).
rdf_is_name(Name):-
  rdf_is_iri(Name).


%! rdf_node_(+RdfNode:or([bnode,iri,literal])) is semidet.
%! rdf_node_(-RdfNode:or([bnode,iri,literal])) is nondet.

rdf_node_(Node):-
  nonvar(Node), !,
  rdf_is_node(Node).
rdf_node_(Node):-
  rdf_node(Node, _).


%! rdf_node(+RdfNode:or([bnode,iri,literal]), +RdfGraph:atom) is semidet.
%! rdf_node(-RdfNode:or([bnode,iri,literal]), +RdfGraph:atom) is nondet.
%! rdf_node(+RdfNode:or([bnode,iri,literal]), -RdfGraph:atom) is nondet.
%! rdf_node(-RdfNode:or([bnode,iri,literal]), -RdfGraph:atom) is nondet.
% Pairs of RDF nodes and RDF graphs in which they occur.
%
% The set of RDF nodes of an RDF graph is
% the set of subjects and objects of triples in the graph.
%
% It is possible for a predicate IRI to also occur as a node
% in the same graph.

rdf_node(Node, G):-
  nonvar(Node),
  % Enumerates the nodes.
  rdf_resource(Node),
  % Relate node to graph.
  (
    rdf_subject(Node, G)
  ;
    rdf_object(Node, G)
  ).


%! rdf_is_node(+RdfTerm:or([bnode,iri,literal])) is semidet.

rdf_is_node(Node):-
  rdf_is_object(Node).


%! rdf_object(+Term) is nondet.
% Succeeds if the given term occurs in the object position of some RDF triple.
%! rdf_object(-Object:or([bnode,iri,literal])) is nondet.
% Enumerates RDF object terms.

rdf_object(O):-
  nonvar(O), !,
  rdf_is_object(O).
rdf_object(O):-
  rdf_resource(O),
  once(rdf(_, _, O, _)).


%! rdf_object(+Object:or([bnode,iri,literal]), +RdfGraph:atom) is semidet.
%! rdf_object(-Object:or([bnode,iri,literal]), +RdfGraph:atom) is nondet.
%! rdf_object(+Object:or([bnode,iri,literal]), -RdfGraph:atom) is nondet.
%! rdf_object(-Object:or([bnode,iri,literal]), -RdfGraph:atom) is nondet.

rdf_object(O, G):-
  % Enumerates the subject and object terms.
  rdf_resource(O),
  % Excludes RDF nodes that only occur in the subject position
  % and relates to an RDF graph.
  rdf(_, _, O, G).


%! rdf_is_object(+Term) is semidet.
% Succeeds if the given term can occur in the object position
% of an RDF triple.
%
% This is independent of the term occurring in an actual triple or graph.
% of an RDF triple.

rdf_is_object(O):-
  rdf_is_subject(O), !.
rdf_is_object(O):-
  rdf_is_literal(O).


%! rdf_predicate(+Term) is semidet.
% Succeeds if the given RDF term occurs in the predicate position
% of an actual triple.
%! rdf_predicate(-Predicate:iri) is nondet.
% Enumerates the RDF predicate terms in the triple store.
% Ensures there are no duplicates.

rdf_predicate(P):-
  nonvar(P), !,
  rdf_is_predicate(P).
rdf_predicate(P):-
  rdf_current_predicate(P).


%! rdf_predicate(+Predicate:iri, +RdfGraph:atom) is semidet.
%! rdf_predicate(-Predicate:iri, +RdfGraph:atom) is nondet.
%! rdf_predicate(+Predicate:iri, -RdfGraph:atom) is nondet.
%! rdf_predicate(-Predicate:iri, -RdfGraph:atom) is nondet.

rdf_predicate(P, G):-
  % Enumerates the RDF predicate terms.
  rdf_current_predicate(P),
  % Relate to an RDF graph.
  rdf(_, P, _, G).


%! rdf_is_predicate(+RdfTerm:or([bnode,iri,literal])) is semidet.
% Succeeds if the given term can occur in the predicate position
% of an RDF triple.
%
% This is independent of the term occurring in an actual triple or graph.

rdf_is_predicate(P):-
  rdf_is_iri(P).


%! rdf_subject_(+Term) is semidet.
% Succeeds if the given term occurs in the subject position of some RDF triple.
%! rdf_subject_(-Subject:or([bnode,iri])) is nondet.
% Enumerates the RDF terms that occur in the subject position
% of some RDF triple.
% Ensures there are no duplicates.

rdf_subject_(S):-
  nonvar(S), !,
  rdf_is_subject(S).
rdf_subject_(S):-
  rdf_resource(S),
  once(rdf(S, _, _, _)).


%! rdf_subject(+Subject:or([bnode,iri]), +RdfGraph:atom) is semidet.
%! rdf_subject(-Subject:or([bnode,iri]), +RdfGraph:atom) is nondet.
%! rdf_subject(+Subject:or([bnode,iri]), -RdfGraph:atom) is nondet.
%! rdf_subject(-Subject:or([bnode,iri]), -RdfGraph:atom) is nondet.
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

rdf_subject(S, G):-
  % Enumerate the RDF nodes.
  rdf_resource(S),
  % Relate to an RDF graph.
  rdf(S, _, _, G).


%! rdf_is_subject(+Term) is semidet.
% Succeeds if the given term can occur in the subject position
% of an RDF triple.
%
% This is independent of the term occurring in an actual triple or graph.

rdf_is_subject(S):-
  rdf_is_bnode(S), !.
rdf_is_subject(S):-
  rdf_is_iri(S).


%! rdf_term(+Term:or([bnode,iri,literal])) is semidet.
%! rdf_term(-Term:or([bnode,iri,literal])) is nondet.
% Enumerates all RDF terms.
%
% Duplicates occur only for RDF terms that are
% an RDF node and an RDF predicate term.

rdf_term(Term):-
  rdf_resource(Term).
rdf_term(Term):-
  rdf_current_predicate(Term).


%! rdf_term(+Term:or([bnode,iri,literal]), +RdfGraph:atom) is semidet.
%! rdf_term(+Term:or([bnode,iri,literal]), -RdfGraph:atom) is nondet.
%! rdf_term(-Term:or([bnode,iri,literal]), +RdfGraph:atom) is nondet.
%! rdf_term(-Term:or([bnode,iri,literal]), -RdfGraph:atom) is nondet.
% Pairs of graphs and terms that occur in that graph.
%
% A term is either a subject, predicate or object term
% in an RDF triple.

rdf_term(Term, G):-
  rdf_term(Term),
  (
    rdf_node(Term, G)
  ;
    rdf_predicate(Term, G)
  ).


%! rdf_vocabulary(+Graph:atom, -Vocabulary:ordset([literal,iri])) is det.
% Returns the vocabulary of the given graph.
%
% The vocabulary of a graph is the set of RDF names that occur
% in the triples of the graph.
%
% @see RDF Semantics http://www.w3.org/TR/2004/REC-rdf-mt-20040210/

rdf_vocabulary(G, Vocabulary):-
  aggregate_all(
    set(Name),
    rdf_name(Name, G),
    Vocabulary
  ).

