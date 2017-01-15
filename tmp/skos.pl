:- use_module(library(rdf/rdf_term)).

%! skosmap(+Lit, +Opts)// is det.
%
% Component that emits a =td= cell with links to SKOS concepts that
% are labeled Lit.

skosmap(Lit, Opts) -->
  {
    q_is_literal(Lit),
    findall(Concept-Scheme, skos_find(Lit, Concept, Scheme), Pairs),
    Pairs \== [],
    q_sort_terms(pair_label_key, Pairs, Sorted)
  },
  html(td(\html_seplist(skos_reference, Sorted))).


skos_find(Lit, Concept, Scheme) :-
  rdf_has(Concept, skos:prefLabel, Lit),
  rdf_has(Concept, skos:inScheme, Scheme).


skos_references([]) --> [].
skos_references([H|T]) -->
  skos_reference(H),
  (   {T == []}
  ->  []
  ;   html("; "),
      skos_references(T)
  ).

skos_reference(Concept-Scheme) -->
  html([\rdf_link(Concept), " in ", \rdf_link(Scheme)]).
