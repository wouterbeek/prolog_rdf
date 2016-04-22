:- module(
  lov,
  [
    lov/1 % -Namespace
  ]
).

/** <module> Linked Open Vocabularies (LOV)

@author Wouter Beek
@version 2016/04
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(lists)).
:- use_module(library(sparql/sparql_query)).





lov(Iri) :-
  atom_phrase(
    sparql_build_select(
      [rdf,voaf],
      [vocab],
      [rdf(var(vocab),rdf:type,voaf:'Vocabulary')]
    ),
    Q
  ),
  sparql_select('http://lov.okfn.org/dataset/lov/sparql', Q, Results),
  member([Iri], Results).
