:- module(
  void_store,
  [
    void_store_iri/1 % -Iri
  ]
).

/** <module> VoID store

@author Wouter Beek
@see http://void.rkbexplorer.com/
@version 2014/05
*/

:- use_module(library(aggregate)).
:- use_module(library(http/http_download)).
:- use_module(library(xpath)).





void_store_iri(Iri) :-
  xml_download('http://void.rkbexplorer.com/browse/', Dom),
  xpath_chk(Dom, //div(@class=content), Div),
  xpath(Div, //li, Li),
  xpath_chk(Li, a(@href), Iri).
