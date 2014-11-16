:- module(
  rdf_term,
  [
    rdf_bnode/2, % ?BNode:bnode
                 % ?Graph:atom
    rdf_iri/1, % ?Iri:iri
    rdf_iri/2, % ?Iri:iri
               % ?Graph:atom
    rdf_is_iri/1, % @Term
    rdf_is_name/1, % @Term
    rdf_is_term/1, % @Term
    rdf_name/1, % ?Name:or([iri,literal])
    rdf_name/2, % ?Name:or([iri,literal])
                % ?Graph:atom
    rdf_node/1, % ?Node:rdf_term
    rdf_node/2, % ?Node:rdf_term
                % ?Graph:atom
    rdf_object/1, % ?Object:rdf_term
    rdf_object/2, % ?Object:rdf_term
                  % ?Graph:atom
    rdf_predicate/1, % ?Predicate:iri
    rdf_predicate/2, % ?Predicate:iri
                     % ?Graph:atom
    rdf_subject/1, % ?Subject:or([bnode,iri])
    rdf_subject/2, % ?Subject:or([bnode,iri])
                   % ?Graph:atom
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

--

@author Wouter Beek
@compat [RDF 1.1 Concepts and Abstract Syntax](http://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/)
@version 2012/01-2013/05, 2013/07-2013/08, 2014/01-2014/03, 2014/05,
         2014/09, 2014/11
*/

:- use_module(library(semweb/rdf_db)).

:- use_module(generics(typecheck)).
:- use_module(pl(pl_mode)).

:- use_module(plRdf(term/rdf_literal)).

:- rdf_meta(rdf_iri(r)).
:- rdf_meta(rdf_iri(r,?)).
:- rdf_meta(rdf_is_iri(o)).
:- rdf_meta(rdf_is_name(o)).
:- rdf_meta(rdf_is_term(o)).
:- rdf_meta(rdf_name(o)).
:- rdf_meta(rdf_name(o,?)).
:- rdf_meta(rdf_node(o)).
:- rdf_meta(rdf_node(o,?)).
:- rdf_meta(rdf_object(o)).
:- rdf_meta(rdf_object(o,?)).
:- rdf_meta(rdf_predicate(r)).
:- rdf_meta(rdf_predicate(r,?)).
:- rdf_meta(rdf_subject(r)).
:- rdf_meta(rdf_subject(r,?)).
:- rdf_meta(rdf_term(o)).
:- rdf_meta(rdf_term(o,?)).




%! rdf_bnode(+BNode) is semidet.
%! rdf_bnode(-BNode) is nondet.
% Enumerates the current blank node terms.
% Ensures that no blank node occurs twice.

rdf_bnode(BNode, Graph):-
  rdf_resource(BNode),
  rdf_is_bnode(BNode),



%! rdf_bnode(+BNode:bnode, +Graph:atom) is semidet.
%! rdf_bnode(-BNode:bnode, +Graph:atom) is nondet.
%! rdf_bnode(+BNode:bnode, -Graph:atom) is nondet.
%! rdf_bnode(-BNode:bnode, -Graph:atom) is nondet.
% Relates blank nodes to RDF graphs in which they occur.
% Ensures that no pair occurs twice.

rdf_bnode(BNode, Graph):-
  rdf_bnode(BNode),
  % Relate blank node to graph.
  rdf_node(BNode, Graph).



%! rdf_iri(+Iri:iri) is semidet.
% Succeeds if the given RDF term is an IRI.
%! rdf_iri(+Iri:iri) is nondet.
% Enumerates the IRIs in the RDF store.
% May contain duplicates!

rdf_iri(Iri):-
  rdf_current_predicate(Iri).
rdf_iri(Iri):-
  rdf_resource(Iri),
  \+ rdf_is_bnode(Iri),
  \+ rdf_is_literal(Iri).



%! rdf_iri(+Iri:iri, +Graph:atom) is semidet.
%! rdf_iri(-Iri:iri, +Graph:atom) is nondet.
%! rdf_iri(+Iri:iri, -Graph:atom) is nondet.
%! rdf_iri(-Iri:iri, -Graph:atom) is nondet.

rdf_iri(Iri, Graph):-
  rdf_iri(Iri),
  rdf_term(Iri, Graph).



%! rdf_is_iri(+Term) is semidet.
% Succeeds for atoms that conform to the syntactic requirement of being
% an IRI RDF term.
%
% This does not imply that the term occurs in an actual triple or graph.
%
rdf_is_iri(IRI):-
  is_of_type(iri, IRI).



%! rdf_is_name(@Term) is semidet.
% Succeeds if the given term is syntactically an RDF name,
%  i.e., it need not be present in any current triple.

rdf_is_name(Name):-
  rdf_is_literal(Name).
rdf_is_name(Name):-
  rdf_is_resource(Name).



%! rdf_is_term(@Term) is semidet.
% Succeeds if the given term is syntactically an RDF term,
%  i.e., it need not be present in any current triple.

rdf_is_term(Term):-
  rdf_is_name(Term).
rdf_is_term(Term):-
  rdf_is_bnode(Term).



%! rdf_name(+Name:or([iri,literal])) is semidet.
%! rdf_name(-Name:or([iri,literal])) is nondet.

rdf_name(Name):-
  (   % Subject or object terms.
      rdf_resource(Name)
  ;   % Predicate terms.
      rdf_current_predicate(Name)
  ),
  % Exclude blank nodes.
  \+ rdf_is_bnode(Name).



%! rdf_name(+Name:or([iri,literal]), +Graph:atom) is semidet.
%! rdf_name(-Name:or([iri,literal]), +Graph:atom) is nondet.
%! rdf_name(+Name:or([iri,literal]), -Graph:atom) is nondet.
%! rdf_name(-Name:or([iri,literal]), -Graph:atom) is nondet.
% **RDF names** are either IRIs or RDF literals.
%
% Notice that RDF names are the RDF ground terms.
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
  rdf_name(Name),
  rdf_term(Name, Graph).



%! rdf_node(+Node:rdf_term) is semidet.
%! rdf_node(-Node:rdf_term) is nondet.

rdf_node(Node):-
  rdf_resource(Node),
  once(
    (   rdf_subject(Node)
    ;   rdf_object(Node)
    )
  ).



%! rdf_node(+Node:rdf_term, +Graph:atom) is semidet.
%! rdf_node(-Node:rdf_term, +Graph:atom) is nondet.
%! rdf_node(+Node:rdf_term, -Graph:atom) is nondet.
%! rdf_node(-Node:rdf_term, -Graph:atom) is nondet.
% Pairs of RDF nodes and RDF graphs in which they occur.
%
% The set of RDF nodes of an RDF graph is
% the set of subjects and objects of triples in the graph.
%
% It is possible for a predicate IRI to also occur as a node
% in the same graph.

rdf_node(Node, Graph):-
  rdf_resource(Node),
  (   rdf_subject(Node, Graph)
  ;   rdf_object(Node, Graph)
  ).



%! rdf_object(+Object:rdf_term) is semidet.
%! rdf_object(-Object:rdf_term) is nondet.

rdf_object(Object):-
  rdf_resource(Object),
  rdf(_, _, Object).



%! rdf_object(+Object:rdf_term, +Graph:atom) is semidet.
%! rdf_object(-Object:rdf_term, +Graph:atom) is nondet.
%! rdf_object(+Object:rdf_term, -Graph:atom) is nondet.
%! rdf_object(-Object:rdf_term, -Graph:atom) is nondet.

rdf_object(Object, Graph):-
  rdf_resource(Object),
  rdf(_, _, Object, Graph).



%! rdf_predicate(+Predicate:iri) is semidet.
%! rdf_predicate(-Predicate:iri) is nondet.
% @see Terminological variant of rdf_current_predicate/1.

rdf_predicate(Predicate):-
  rdf_current_predicate(Predicate).



%! rdf_predicate(+Predicate:iri, +Graph:atom) is semidet.
%! rdf_predicate(-Predicate:iri, +Graph:atom) is nondet.
%! rdf_predicate(+Predicate:iri, -Graph:atom) is nondet.
%! rdf_predicate(-Predicate:iri, -Graph:atom) is nondet.

% Semidet case.
rdf_predicate(Predicate, Graph):-
  rdf_current_predicate(Predicate),
  rdf(_, Predicate, _, Graph).



%! rdf_subject(+Subject:or([bnode,iri])) is semidet.
%! rdf_subject(-Subject:or([bnode,iri])) is nondet.

rdf_subject(Subject):-
  rdf_resource(Subject),
  rdf(Subject, _, _).



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

rdf_subject(Subject, Graph):-
  rdf_resource(Subject),
  rdf(Subject, _, _, Graph).



%! rdf_term(+Term:rdf_term) is semidet.
%! rdf_term(-Term:rdf_term) is nondet.

rdf_term(Term):-
  rdf_node(Term).
rdf_term(Term):-
  rdf_predicate(Term).



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

rdf_term(Term, Graph):-
  rdf_node(Term, Graph).
rdf_term(Term, Graph):-
  rdf_predicate(Term, Graph).
