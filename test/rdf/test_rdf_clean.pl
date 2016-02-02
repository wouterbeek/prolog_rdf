:- module(
  test_rdf_clean,
  [
    test_rdf_clean/1, % -From
    test_rdf_clean/2  % -From, +Skip:nonneg
  ]
).

/* <module> Test RDF clean

@author Wouter Beek
@version 2016/01-2016/02
*/

:- use_module(library(debug_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(gui_tracer)).
:- use_module(library(hash_ext)).
:- use_module(library(json_ext)).
:- use_module(library(jsonld/jsonld_metadata)).
:- use_module(library(jsonld/jsonld_read)).
:- use_module(library(lists)).
:- use_module(library(lodapi/lodapi_document)).
:- use_module(library(lodapi/lodapi_generics)).
:- use_module(library(lodapi/lodapi_metadata)).
:- use_module(library(os/archive_ext)).
:- use_module(library(os/open_any2)).
:- use_module(library(rdf/rdf_clean)).
:- use_module(library(rdf/rdf_debug)).
:- use_module(library(rdf/rdf_load)).
:- use_module(library(rdf/rdf_print)).

:- meta_predicate
    catch_metadata(0).

:- debug(http(parse)).
:- debug(rdf(clean)).





test_rdf_clean(From):-
  test_rdf_clean(From, _).

test_rdf_clean(From, N):-
  (   var(N)
  ->  test_source(From)
  ;   findnsols(N, From, test_source(From), Froms),
      last(Froms, From)
  ),
  md5(From, Hash),
  document_name(Doc, Hash),
  document_path(Doc, Dir),
  make_directory_path(Dir),
  Opts = [access(write),relative_to(Dir)],
  absolute_file_name(dirty, DTo, Opts),
  absolute_file_name('clean.nq', CTo, Opts),
  absolute_file_name('metadata.nq', MTo, Opts),
  rdf_download_to_file(From, DTo),
  setup_call_cleanup(
    open_any2(MTo, append, Write, Close_0),
    with_output_to(Write,
      rdf_store_messages(Doc, (
        rdf_clean(From, CTo, [metadata(M)]),
        rdf_store_metadata(Doc, M)
      ))
    ),
    close_any2(Close_0)
  ).

test_source(Source):-
  document(Doc),
  metadata(Doc, llo:url, Source).

catch_metadata(Goal_0) :-
  catch(Goal_0, error(existence_error(open_any2,M),_), print_metadata(M)).

print_metadata(M1) :-
  jsonld_metadata(M1, M2),
  forall(jsonld_to_triple(M2, Triple), rdf_print_triple(Triple)).
