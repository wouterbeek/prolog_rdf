:- module(
  rdf_read,
  [
    rdf/5, % +Options:list(nvpair)
           % ?Subject:or([bnode,iri])
           % ?Predicate:iri
           % ?Object:or([bnode,iri,label])
           % ?Graph:atom
    rdf_direction/4, % +Direction:oneof([backward,both,forward]),
                     % +Resource:iri,
                     % +Graph:atom,
                     % -Triples:ordset(compound)
    rdf_equiv/2, % ?Resource1:or([bnode,iri,literal])
                 % ?Resource2:or([bnode,iri,literal])
    rdf_find/4, % +Subject:or([bnode,iri]),
                % +Predicate:iri,
                % +Object:or([bnode,iri,literal]),
                % +Graph:atom
    rdf_member/2, % ?Member:uri
                  % ?Members:list(uri)
    rdf_memberchk/2, % ?Member:uri
                     % ?Members:list(uri)
    rdf_property/2, % +Graph:atom
                    % ?Property:iri
    rdfg/4 % ?Subject:or([bnode,iri]),
           % ?Predicate:iri,
           % ?Object:or([bnode,iri,literal]),
           % ?RdfGraph:atom
  ]
).

/** <module> RDF read

Predicates for reading from RDF, customized for specific datatypes and
literals.

@author Wouter Beek
@version 2011/08, 2012/01, 2012/03, 2012/09, 2012/11-2013/04, 2013/07-2013/10
         2014/01, 2014/06-2014/07
*/

:- use_module(library(apply)).
:- use_module(library(semweb/rdf_db)).

:- use_module(plRdf(rdf_graph)).
:- use_module(plRdf(rdfs_read)).

:- rdf_register_prefix(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').

:- rdf_meta(rdf(+,r,r,o,?)).
:- rdf_meta(rdf_direction(+,r,+,-)).
:- rdf_meta(rdf_member(r,+)).
:- rdf_meta(rdf_memberchk(r,+)).
:- rdf_meta(rdf_property(+,r)).



%! rdf(
%!   +Options:list(nvpair),
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:or([bnode,iri,literal]),
%!   ?Graph:atom
%! ) is nondet.
% The following options are supported:
%   * =|graph_mode(+Graph:oneof([normal,no_index,no_inst)|=
%     Whether the index that sometimes appears alongside the graph name
%     should be returned (`normal`) or not (`no_index`).
%     `no_inst` means that variable graphs are not instantiated,
%     but use rdf/3 instead.

rdf(O1, S, P, O, Graph):-
  var(Graph), !,
  option(graph_mode(Mode), O1, normal),
  (
    Mode == no_index
  ->
    rdf(S, P, O, Graph0),
    rdf_graph:rdf_graph(Graph0, Graph)
  ;
    Mode == no_inst
  ->
    rdf(S, P, O)
  ;
    rdf(S, P, O, Graph)
  ).
rdf(_, S, P, O, Graph):-
  rdf(S, P, O, Graph).


%! rdf_both_bnode(
%!   +RDF_Term1:or([bnode,iri,literal]),
%!   +RDF_Term2:or([bnode,iri,literal])
%! ) is semidet.
% Fails if only either of the RDF terms is a blank node.

rdf_both_bnode(X, Y):-
  rdf_is_bnode(X), !,
  rdf_is_bnode(Y).
rdf_both_bnode(_, _).


%! rdf_direction(
%!   +Direction:oneof([backward,both,forward]),
%!   +Resource:iri,
%!   +Graph:atom,
%!   -Triples:ordset(compound)
%! ) is det.

rdf_direction(backward, Resource, Graph, Triples):- !,
  aggregate_all(
    set(rdf(S,P,Resource)),
    rdf(S, P, Resource, Graph),
    Triples
  ).
rdf_direction(both, Resource, Graph, Triples):- !,
  rdf_direction(backward, Resource, Graph, Propositions1),
  rdf_direction(forward, Resource, Graph, Propositions2),
  ord_union(Propositions1, Propositions2, Triples).
rdf_direction(forward, Resource, Graph, Triples):- !,
  aggregate_all(
    set(rdf(Resource,P,O)),
    rdf(Resource, P, O, Graph),
    Triples
  ).


rdf_equiv(X, Y):-
  rdf_equal(X, Y).
rdf_equiv(X, Y):-
  rdf_has(X, owl:sameAs, Y).


%! rdf_find(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Object:or([bnode,iri,literal]),
%!   +Graph:atom
%! ) is semidet.
% Finds an RDF triple according to an RDF triple.
% This is different from rdf/[3,4], which are not RDF triples
% (since their parameters may hold argments that are variables).
%
% Since we cannot match blank nodes directly, we replace them with variables.
% This is valid under graph equivalence.

rdf_find(S, P, O, G):-
  maplist(rdf_bnode_to_var, [S,P,O], [SS,PP,OO]),
  rdf(SS, PP, OO, G),
  maplist(rdf_both_bnode, [S,P,O], [SS,PP,OO]).


rdf_member(Member, List):-
  member(Member0, List),
  rdf_global_id(Member0, Member).


rdf_memberchk(Member, List):-
  once(rdf_member(Member, List)).


rdf_property(G, P):-
  rdfs_individual(m(f,f,f), P, rdf:'Property', G).


%! rdfg(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:or([bnode,iri,literal]),
%!   ?RdfGraph:atom
%! ) is nondet.
% Variant of rdf/4 which does not take the line number / index annotation
% of the graph argument into account.

rdfg(S, P, O, G):-
  rdf([graph_mode(no_index)], S, P, O, G).



% Helpers.

%! rdf_bnode_to_var(
%!   +Term:or([bnode,iri,literal]),
%!   -TermOrVar:or([iri,literal])
%! ) is det.
% Replaces blank nodes with uninstantiated variables,
% and returns IRIs and literals unchanged.

rdf_bnode_to_var(X, _):-
  rdf_is_bnode(X), !.
rdf_bnode_to_var(X, X).

