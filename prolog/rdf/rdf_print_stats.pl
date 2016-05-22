:- module(
  rdf_print_stats,
  [
    rdf_print_stats/1 % +P
  ]
).

/** <module> RDF print statistics

@author Wouter Beek
@version 2016/05
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(dcg/dcg_pl)).
:- use_module(library(pair_ext)).
:- use_module(library(print_ext)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(rdf/rdf_stats)).
:- use_module(library(semweb/rdf11)).

:- rdf_meta
     rdf_print_stats(r).





rdf_print_stats(P) :-
  aggregate_all(set(O), rdf(_, P, O), Os),
  maplist(rdf_number_of_subjects(P), Os, Ns),
  pairs_keys_values(Pairs, Ns, Os),
  keysort(Pairs, SortedPairs),
  maplist(pair_list, SortedPairs, Rows),
  print_table([head(["#Occurrences","Object"])|Rows], [cell(print_value0)]).

print_value0(Term) --> dcg_print_term(Term), !.
print_value0(Term) --> term(Term).
