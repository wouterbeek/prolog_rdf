:- module(rdf_test, [load_manifest/0,run_rdf_tests/0]).

/** <module> Run W3C tests for RDF

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(apply)).
:- use_module(library(io_ext)).
:- use_module(library(rdf/rdf_convert)).
:- use_module(library(rdf/rdf_list)).
:- use_module(library(rdf/rdf_read)).
:- use_module(library(rdfs/rdfs_read)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/turtle)).

:- dynamic(current_manifest/1).

:- rdf_register_prefix(mf, 'http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#').
:- rdf_register_prefix(rdft, 'http://www.w3.org/ns/rdftest#').

manifest_uri('http://www.w3.org/2013/TurtleTests/manifest.ttl').

load_manifest:-
  clean_current_manifest,
  manifest_uri(Uri),
  rdf_load(Uri, [base_uri(Uri),format(turtle),graph(manifest)]),
  assert(current_manifest(Uri)).

clean_current_manifest:-
  rdf_retractall(_, _, _, manifest),
  retractall(current_manifest(_)).

mf_test(Test):-
  rdf_instance(MF, mf:'Manifest', manifest),
  rdf(MF, mf:entries, Entries, manifest),
  rdf_list_member_raw(Test, Entries, manifest).

run_rdf_tests:-
  forall(mf_test(Test), run_rdf_test(Test)).

run_rdf_test(Test):-
  announce_test(Test), !,
  rdf(Test, mf:action, Uri, manifest),
  print_input(Uri, [indent(2)]),
  rdf_convert(Uri, To, []),
  print_input(To, [indent(2)]).

announce_test(Test):-
  rdf_literal(Test, mf:name, xsd:string, Name, manifest),
  rdfs_comment(Test, Comment),
  format('Test ~a:~nComment: ~a~n', [Name,Comment]).
