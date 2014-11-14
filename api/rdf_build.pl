:- module(
  rdf_build,
  [
    rdf_assert_instance/3, % +Instance:iri
                           % +Class:iri
                           % ?Graph:atom
    rdf_assert_property/2, % +Property:iri
                           % ?Graph:atom
    rdf_assert2/4, % +Subject:or([bnode,iri])
                   % +Predicate:iri
                   % +Object:or([bnode,iri,literal])
                   % ?Graph:atom
    rdf_copy/5, % +FromGraph:atom
                % ?Subject:or([bnode,iri])
                % ?Predicate:iri
                % ?Object:or([bnode,iri,literal])
                % +ToGraph:atom
    rdf_create_next_resource/5, % +Prefix:atom
                                % +SubPaths:list(atom)
                                % ?Class:iri
                                % ?Graph:atom
                                % -Resource:iri
    rdf_remove_resource/2, % +Resource:rdf_term
                           % ?Graph:atom
    rdf_remove_term/2 % +Term:rdf_term
                      % ?Graph:atom
  ]
).

/** <module> RDF API: Build RDF

Simple asserion and retraction predicates for RDF.
Triples with literals are treated in dedicated modules.

@author Wouter Beek
@version 2013/10, 2013/12-2014/01, 2014/06, 2014/08-2014/11
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(library(uri)).

:- use_module(plRdf(api/rdf_read)).
:- use_module(plRdf(term/rdf_bnode_map)).

:- rdf_meta(rdf_assert_instance(r,r,?)).
:- rdf_meta(rdf_assert_property(r,?)).
:- rdf_meta(rdf_assert2(r,r,o,?)).
:- rdf_meta(rdf_copy(+,r,r,o,+)).
:- rdf_meta(rdf_create_next_resource(+,+,r,?,-)).
:- rdf_meta(rdf_remove_resource(r,?)).
:- rdf_meta(rdf_remove_term(r,?)).



%! rdf_assert_instance(+Instance:iri, +Class:iri, ?Graph:graph) is det.
% Asserts an instance/class relationship.
%
% The following triples are added to the database:
%
% ```nquads
% <INSTANCE,rdf:type,CLASS,GRAPH>
% ```

rdf_assert_instance(Instance, Class, Graph):-
  rdf_assert2(Instance, rdf:type, Class, Graph).



%! rdf_assert_property(+Property:iri, ?Graph:atom) is det.
% Asserts an RDF property.
%
% The following triples are added to the database:
%
% ```nquads
% <PROPERTY,rdf:type,rdf:Property,GRAPH>
% ```

rdf_assert_property(Property, Graph):-
  rdf_assert_instance(Property, rdf:'Property', Graph).



%! rdf_assert2(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Object:rdf_term,
%!   ?Graph:atom
%! ) is det.
% Like rdf/4 in [rdf_db], but allows Graph to be uninstantiated.
%
% @see rdf_db:rdf/4

rdf_assert2(S, P, O, G):-
  var(G), !,
  rdf_assert(S, P, O).
rdf_assert2(S, P, O, G):-
  rdf_assert(S, P, O, G).



%! rdf_copy(
%!   +FromGraph:atom,
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:or([bnode,iri,literal]),
%!   +ToGraph:atom
%! ) is det.
% Copies triples between graphs.
%
% @tbd Perform blank node renaming.

rdf_copy(FromGraph, S, P, O, ToGraph):-
  forall(
    rdf(S, P, O, FromGraph),
    rdf_assert(S, P, O, ToGraph)
  ).



%! rdf_create_next_resource(
%!   +Prefix:atom,
%!   +SubPaths:list(atom),
%!   ?Class:iri,
%!   ?Graph:atom,
%!   -Resource:iri
%! ) is det.
% Creates new resource-denoting IRIs in a uniform way.
%
% @arg Prefix is a registered RDF prefix name.
%      The replacing IRI is used as the base IRI for the resource.
%      See rdf_register_prefix/2.
% @arg SubPaths is a list of path names that are suffixed to the base IRI.
% @arg Class An optional IRI denoting an RDFS class.
%      See rdf_assert_instance/3.
% @arg Graph An optional RDF graph name.
% @arg Resource The newly created IRI.
%
% The Prefix + Subpaths combination is used as the unique flag name
% for counting the created IRIs.

rdf_create_next_resource(Prefix, SubPaths1, Class, Graph, Resource):-
  % A counter keeps track of the integer identifier of the IRI.
  with_output_to(atom(FlagTerm), write_term([Prefix|SubPaths1], [])),
  rdf_atom_md5(FlagTerm, 1, Flag),
  flag(Flag, Id, Id + 1),
  
  % The identifier is appended to the IRI path.
  append(SubPaths1, [Id], SubPaths2),
  atomic_list_concat(SubPaths2, '/', Path),
  
  % Resolve the absolute IRI against the base IRI denoted by the RDF prefix.
  rdf_global_id(Prefix:'', Base),
  uri_normalized(Path, Base, Resource),
  
  (   nonvar(Class)
  ->  rdf_assert_instance(Resource, Class, Graph)
  ;   true
  ).



%! rdf_remove_resource(+Resource:rdf_term, ?Graph:atom) is det.
% Removes all triples in which the resource denoted by the given RDF term
%  occurs.

rdf_remove_resource(Term, Graph):-
  forall(
    rdf_id(Term, Term0),
    rdf_remove_term(Term0, Graph)
  ).



%! rdf_remove_term(+Term:rdf_term, ?Graph:atom) is det.
% Removes all triples in which the given RDF term occurs.

rdf_remove_term(Term, Graph):-
  % Remove the mapped blank node, if it exists.
  term_to_bnode(Term, BNode),
  rdf_remove_term(BNode, Graph),
  
  % Remove the plain triples in which the term occurs.
  rdf_retractall(Term, _, _, Graph),
  rdf_retractall(_, Term, _, Graph),
  rdf_retractall(_, _, Term, Graph).
