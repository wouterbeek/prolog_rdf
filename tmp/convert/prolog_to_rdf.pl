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
@version 2014/01, 2014/11, 2015/12
*/

:- use_module(library(apply)).
:- use_module(library(rdf/rdf_build)).





prolog_to_rdf(Graph, Module, Term, Individual):-
  % Namespace.
  (
    rdf_current_prefix(Module, _), !
  ;
    atomic_list_concat(['http://www.wouterbeek.com',Module,''], /, URL),
    rdf_register_prefix(Module, URL)
  ),

  % Class.
  Term =.. [Functor|Args],
  once(atom_phrase(atom_capitalize, Functor, ClassName)),
  rdf_global_id(Module:ClassName, Class),
  rdfs_assert_class(Class, Graph),

  % Individual.
  rdf_bnode(Individual),
  rdf_assert_instance(Individual, Class, Graph),

  % Propositions.
  Module:legend(Functor, ArgRequirements),
  maplist(prolog_to_rdf(Graph, Module, Individual), ArgRequirements, Args).

prolog_to_rdf(
  Graph,
  Module,
  Individual1,
  PredicateName-PrologType-Optional,
  Value
):-
  rdf_global_id(Module:PredicateName, Predicate),
  (   PrologType =.. [list,InnerPrologType]
  ->  is_list(Value),
      maplist(
        prolog_to_rdf(
          Graph,
          Module,
          Individual1,
          PredicateName-InnerPrologType-Optional
        ),
        Value
      )
  ;   PrologType = _/_
  ->  prolog_to_rdf(Graph, Module, Value, Individual2),
      rdf_assert(Individual1, Predicate, Individual2, Graph)
  ;   rdf_datatype(Datatype, PrologType)
  ->  rdf_assert_typed_literal(Individual1, Predicate, Value, Datatype, Graph)
  ;   Optional = true
  ).

