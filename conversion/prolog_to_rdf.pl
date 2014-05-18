:- module(
  prolog_to_rdf,
  [
    prolog_to_rdf/4 % +Graph:atom
                    % +Module:atom
                    % +Term:term
                    % -Individual:iri
  ]
).

/** <module> Prolog to RDF

Automated conversion from Prolog terms to RDF triples.

@author Wouter Beek
@version 2014/01
*/

:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).
:- use_module(library(apply)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf_term(rdf_datatype)).
:- use_module(rdf_term(rdf_literal)).
:- use_module(rdfs(rdfs_build)).
:- use_module(xml(xml_namespace)).
:- use_module(xsd(xsd)).



prolog_to_rdf(Graph, Module, Term, Individual):-
  % Namespace.
  (
    xml_current_namespace(Module, _), !
  ;
    atomic_list_concat(['http://www.wouterbeek.com',Module,''], '/', URL),
    xml_register_namespace(Module, URL)
  ),

  % Class.
  Term =.. [Functor|Args],
  once(dcg_phrase(capitalize, Functor, ClassName)),
  rdf_global_id(Module:ClassName, Class),
  rdfs_assert_class(Class, Graph),

  % Individual.
  rdf_bnode(Individual),
  rdf_assert_individual(Individual, Class, Graph),

  % Propositions.
  Module:legend(Functor, _, ArgRequirements),
  maplist(prolog_to_rdf(Graph, Module, Individual), ArgRequirements, Args).

prolog_to_rdf(
  Graph,
  Module,
  Individual1,
  PredicateName-Type-Optional,
  Value
):-
  rdf_global_id(Module:PredicateName, Predicate),
  (
    Type =.. [list,InnerType]
  ->
    is_list(Value),
    maplist(
      prolog_to_rdf(
        Graph,
        Module,
        Individual1,
        PredicateName-InnerType-Optional
      ),
      Value
    )
  ;
    Type = _/_
  ->
    prolog_to_rdf(Graph, Module, Value, Individual2),
    rdf_assert(Individual1, Predicate, Individual2, Graph)
  ;
    xsd_datatype(Type, Datatype)
  ->
    rdf_assert_datatype(Individual1, Predicate, Value, Datatype, Graph)
  ;
    Optional = true
  ).

