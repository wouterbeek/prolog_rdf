:- module(
  prolog_to_rdf,
  [
    prolog_to_rdf/4 % +Graph:atom
                    % +Module:atom
                    % +Term:term
                    % -Instance:iri
  ]
).

/** <module> Prolog to RDF

Automated conversion from Prolog terms to RDF triples.

@author Wouter Beek
@version 2014/01, 2014/11, 2015/12
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_atom)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(iri/iri_ext)).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(rdfs/rdfs_api)).





prolog_to_rdf(G, Mod, Term, I) :-
  % Namespace.
  (   rdf_current_prefix(Mod, _), !
  ;   atomic_list_concat([Mod,''], /, Path),
      iri_comps(Iri, uri_components(http,'www.wouterbeek.com',Path,_,_)),
      rdf_register_prefix(Mod, Iri)
  ),

  % Class.
  Term =.. [Functor|Args],
  once(atom_phrase(atom_capitalize, Functor, ClassName)),
  rdf_global_id(Mod:ClassName, Class),
  rdfs_assert_class(Class, G),

  % Instance.
  rdf_create_bnode(I),
  rdf_assert_instance(I, Class, G),

  % Propositions.
  Mod:legend(Functor, ArgRequirements),
  maplist(prolog_to_rdf(G, Mod, I), ArgRequirements, Args).

prolog_to_rdf(
  G,
  Mod,
  I1,
  PName-PrologType-Optional,
  Val
) :-
  rdf_equal(Mod:PName, P),
  (   PrologType =.. [list,InnerPrologType]
  ->  is_list(Val),
      maplist(
        prolog_to_rdf(
          G,
          Mod,
          I1,
          PName-InnerPrologType-Optional
        ),
        Val
      )
  ;   PrologType = _/_
  ->  prolog_to_rdf(G, Mod, Val, I2),
      rdf_assert(I1, P, I2, G)
  ;   rdf_datatype(D, PrologType)
  ->  rdf_assert(I1, P, Val^^D, G)
  ;   Optional = true
  ).
