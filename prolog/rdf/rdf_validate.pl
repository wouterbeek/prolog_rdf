:- module(
  rdf_validate,
  [
    rdf_validate/2 % +Input:compound
                   % +Options:list(compound)
  ]
).

/** <module> RDF validate

Validate RDF data streams.

@author Wouter Beek
@version 2015/09
*/

:- use_module(library(count_ext)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(nlp/nlp)).
:- use_module(library(rdf/rdf_load)).





%! rdf_validate(+Input:compound, +Options:list(compound)) is det.

rdf_validate(Spec, _):-
  N = Spec,
  create_counter(N),
  rdf_load_triple(Spec, rdf_count_triples(N)),
  delete_counter(N, C),
  dcg_with_output_to(user_output, (
    "Counted ",
    integer(C),
    " ",
    plural(C,triple),
    ".",
    nl
  )).

rdf_count_triples(N, Ts, _):-
  length(Ts, L),
  increment_counter(N, L).
