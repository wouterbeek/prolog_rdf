:- module(
  lov,
  [
    iri_vocab/2, % +Iri, -Vocab
    lov/1,       % -Vocab
    vocab/1      % -Vocab
  ]
).

/** <module> Linked Open Vocabularies (LOV)

@author Wouter Beek
@version 2016/04
*/

:- use_module(library(atom_ext)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(lists)).
:- use_module(library(persistency)).
:- use_module(library(rdf/rdf_prefix), []).
:- use_module(library(sparql/sparql_query)).

:- initialization(db_attach('lov.db', [])).

:- persistent
   vocab(iri:atom).





init_lov :-
  forall(lov(Vocab), assert_vocab(Vocab)).



iri_vocab(Iri, Vocab) :-
  vocab(Vocab),
  atom_prefix(Iri, Vocab).



lov(Vocab) :-
  atom_phrase(
    sparql_build_select(
      [rdf,voaf],
      [vocab],
      [rdf(var(vocab),rdf:type,voaf:'Vocabulary')]
    ),
    Q
  ),
  sparql_select('http://lov.okfn.org/dataset/lov/sparql', Q, Results),
  member([Vocab], Results).
