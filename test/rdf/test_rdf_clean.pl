:- module(
  test_rdf_clean,
  [
    test_rdf_clean/2 % -From, -To
  ]
).

/* <module> Test RDF clean

@author Wouter Beek
@version 2016/01
*/

:- use_module(library(debug_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(gui_tracer)).
:- use_module(library(hash_ext)).
:- use_module(library(lodapi/lodapi_document)).
:- use_module(library(lodapi/lodapi_metadata)).
:- use_module(library(os/archive_ext)).
:- use_module(library(os/open_any2)).
:- use_module(library(rdf/rdf_clean)).
:- use_module(library(rdf/rdf_clean_metadata)).
:- use_module(library(rdf/rdf_load)).
:- use_module(library(swi_ide)).

:- guitracer.
%:- prolog_ide(debug_monitor).

:- debug(http(parse)).
:- debug(rdf(clean)).





test_rdf_clean(From, To):-
  test_source(From),
  md5(From, Hash),
  catch(rdf_download_to_file(From, Hash), error(existence_error(open_any2,M),_), rdf_clean_metadata(M)),
  catch(rdf_clean(From, To), error(existence_error(open_any2,M),_), rdf_clean_metadata(M)).





% HELPERS %

test_source(Source):-
  document(Doc),
  metadata(Doc, llo:url, Source).
