:- module(
  test_rdf_clean,
  [
    test_rdf_clean/2, % -From, -To
    test_rdf_clean/3  % -From, -To, +Skip:nonneg
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
:- use_module(library(lists)).
:- use_module(library(lodapi/lodapi_document)).
:- use_module(library(lodapi/lodapi_metadata)).
:- use_module(library(os/archive_ext)).
:- use_module(library(os/open_any2)).
:- use_module(library(rdf/rdf_clean)).
:- use_module(library(rdf/rdf_load)).
:- use_module(library(rdf/rdf_metadata_print)).

:- meta_predicate
    catch_metadata(0).

:- debug(http(parse)).
:- debug(rdf(clean)).





test_rdf_clean(From, To):-
  test_rdf_clean(From, To, _).

test_rdf_clean(From, To, N):-
  (   var(N)
  ->  test_source(From)
  ;   findnsols(N, From, test_source(From), Froms),
      last(Froms, From)
  ),
  md5(From, Hash),
  catch_metadata(rdf_download_to_file(From, Hash)),
  catch_metadata(rdf_clean(From, To)).

test_source(Source):-
  document(Doc),
  metadata(Doc, llo:url, Source).

catch_metadata(Goal_0) :-
  catch(Goal_0, error(existence_error(open_any2,M),_), rdf_metadata_print(M)).
