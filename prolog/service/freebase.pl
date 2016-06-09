:- module(
  freebase,
  [
    freebase_load_latest/1 % -Iri
  ]
).

/** <module> Freebase

Support for the Freebase dataset.

@author Wouter Beek
@version 2014/04, 2016/05
*/

:- use_module(library(http/http_download)).
:- use_module(library(pair_ext)).
:- use_module(library(rdf/rdfio)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(uri)).
:- use_module(library(xpath)).





freebase_load_latest(Iri) :-
  freebase_latest_iri(Iri),
  rdf_load_file(Iri).


freebase_latest_iri(LatestIri) :-
  Iri0 = 'http://commondatastorage.googleapis.com/freebase-public/',
  xml_download(Iri0, Dom),
  findall(LMod-Iri, freebase_entry(Iri0, Dom, Iri, LMod), Pairs),
  desc_pairs_values(Pairs, [LatestIri|_]).


freebase_entry(Iri0, Dom, Iri, LMod) :-
  xpath(Dom, //'Contents', Entry),
  xpath_chk(Entry, //'LastModified'(text), Lex),
  rdf11:post_object(LMod^^xsd:dateTime, literal(type(xsd:dateTime,Lex))),
  xpath_chk(Entry, //'Key'(text), Key),
  uri_resolve(Key, Iri0, Iri).
