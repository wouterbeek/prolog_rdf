:- module(
  q_sort,
  [
    q_sort_terms/3,     % +Terms, :Goal_2, -Sorted
      pair_label_key/4, % +M, +G | +Pair, -Key
      term_label_key/4  % +M, +G | +Term, -Key
  ]
).

/** <module> Quine: Sort RDF terms and statements

@author Wouter Beek
@version 2016/11
*/

:- use_module(library(pair_ext)).
:- use_module(library(q/q_rdfs)).
:- use_module(library(semweb/rdf11)).

:- meta_predicate
   q_sort_terms(+, 2, -).

:- rdf_meta
   pair_label_key(+, r, +, -),
   term_label_key(+, r, +, -).





%! q_sort_terms(+Terms, :Goal_2, -Sorted) is det.
%
% Sort a list of RDF terms by their (human-readable) label using
% locale-based ordering.

q_sort_terms(Terms, Goal_2, Sorted) :-
  map_list_to_pairs(Goal_2, Terms, LabelPairs),
  asc_pairs_values(LabelPairs, Sorted).


pair_label_key(M, G, Term-_, Key) :-
  term_label_key(M, G, Term, Key).


term_label_key(M, G, Term, Key) :-
  q_pref_label_lex(M, Term, Lex, G),
  collation_key(Lex, Key).
