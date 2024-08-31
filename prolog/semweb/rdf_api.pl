:- encoding(utf8).
:- module(
  rdf_api,
  [
    rdf_container_membership_property/1, % ?Property
    rdf_container_membership_property/2, % ?Property, ?Number
    rdf_dataset_create/3,                % +DefaultGraph, +NamedGraphs, -Dataset
    rdf_triple_pattern/2                 % +Dataset, ?TriplePattern
  ]
).

/** <module> RDF API

A lightweight API layer on top of the SWI-Prolog standard library for
RDF.

*/

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(settings)).

:- use_module(library(assoc_ext)).

:- rdf_meta
   rdf_container_membership_property(r),
   rdf_container_membership_property(r, ?),
   rdf_dataset_create(r, t, -),
   rdf_triple(+, t).

:- setting(
     rdf_container_membership_property_max,
     nonneg,
     0,
     "The maximum number of RDF container membership properties. Used to make the number of axioms finite.").

error:has_type(rdf_dataset, Dataset) :-
  error:has_type(compound, Dataset),
  Dataset = rdf_dataset(ActiveGraph,Map),
  error:has_type(rdf_graph, ActiveGraph),
  error:has_type(assoc(rdf_graph,rdf_graph), Map).
error:has_type(rdf_graph, Graph) :-
  error:has_type(atom, Graph).
error:has_type(rdf_triple_pattern, TriplePattern) :-
  error:has_type(compound, TriplePattern),
  TriplePattern = tp(S,P,O),
  maplist(error:has_type(any), [S,P,O]).



%! rdf_container_membership_property(+Property:iri) is semidet.
%
% True when Property is a container membership property (rdf:_1,
% rdf:_2, ...).

rdf_container_membership_property(P) :-
  rdf_container_membership_property(P, _).


%! rdf_container_membership_property(+Property:iri, +Number:nonneg) is semidet.
%! rdf_container_membership_property(+Property:iri, -Number:nonneg) is semidet.
%! rdf_container_membership_property(-Property:iri, +Number:nonneg) is semidet.
%! rdf_container_membership_property(-Property:iri, -Number:nonneg) is nondet.
%
% True when Property is the Nth container membership property.
%
% Success of this goal does not imply that Property is present in the
% database.

rdf_container_membership_property(P, N) :-
  (  var(P)
  -> setting(rdf_container_membership_property_max, Max),
     between(1, Max, N),
     rdf_equal(rdf:'_', Prefix),
     atom_concat(Prefix, N, P)
  ;  atom(P),
     rdf_equal(rdf:'_', Prefix),
     string_concat(Prefix, NumS, P),
     number_string(N, NumS),
     integer(N),
     N >= 0
  ).



%! rdf_dataset_create(+DefaultGraph:rdf_graph,
%!                    +NamedGraphs:list(rdf_graph),
%!                    -Dataset:rdf_dataset) is det.

:- det(rdf_dataset_create/3).
rdf_dataset_create(DefaultGraph, NamedGraphs, Dataset) :-
  maplist(rdf_create_graph, [DefaultGraph|NamedGraphs]),
  pairs_keys_values(Pairs, NamedGraphs, NamedGraphs),
  list_to_assoc([default-DefaultGraph|Pairs], Map),
  Dataset = rdf_dataset(default,Map).



%! rdf_triple_pattern(+Dataset:rdf_dataset, +TriplePattern:rdf_triple_pattern) is semidet.
%! rdf_triple_pattern(+Dataset:rdf_dataset, -TriplePattern:rdf_triple_pattern) is nondet.

rdf_triple_pattern(rdf_dataset(ActiveGraph,Map), tp(S,P,O)) :-
  get_assoc(ActiveGraph, Map, G),
  rdf(S, P, O, G).
