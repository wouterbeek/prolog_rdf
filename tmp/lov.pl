:- module(
  lov,
  [
    init_lov/1,   % +Method
    iri_vocab/2,  % +Iri,    -Vocab
    lov/2,        % +Method, -Vocab
    vocab/1       % -Vocab
  ]
).

/** <module> Linked Open Vocabularies (LOV)

Support for the OKF-managed list of open vocabularies.

@author Wouter Beek
@see http://lov.okfn.org/dataset/lov/
@version 2016/04-2016/05
*/

:- use_module(library(aggregate)).
:- use_module(library(atom_ext)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(lists)).
:- use_module(library(persistency)).
:- use_module(library(q/q_prefix), []).
:- use_module(library(rdf/rdf__io)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(sparql/sparql_query_client)).
:- use_module(library(yall)).

:- rdf_create_alias(voaf, 'http://purl.org/vocommons/voaf#').

:- initialization(db_attach('lov.db', [])).

:- persistent
   vocab(iri:atom).





%! init_lov(+Method) is det.
% Method is either `datadump` or `sparql`.

init_lov(Method) :-
  forall(lov(Method, Vocab), assert_vocab(Vocab)).



%! iri_vocab(+Iri, -Vocab) is semidet.
% IRIs are vocabularies they belong to.

iri_vocab(Iri, Vocab) :-
  vocab(Vocab),
  atom_prefix(Iri, Vocab).



%! lov(+Method, -Vocab) is nondet.
% Query the remove LOV server for vocabularies.
%
% Method is either `datadump` or `sparql`.

lov(datadump, Vocab) :- !,
  rdf_call_on_graph('http://lov.okfn.org/dataset/lov/lov.rdf',
    {Vocab}/[G,M,M]>>lov_datadump0(G, Vocab)
  ).
lov(sparql, Vocab) :-
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

lov_datadump0(G, Vocab) :-
  rdf(Vocab, rdf:type, voaf:'Vocabulary', G).
