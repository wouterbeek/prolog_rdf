:- module(rdf_test, [load_manifest/0,run_tests/0]).

:- use_module(library(apply)).
:- use_module(library(http/http_request)).
:- use_module(library(io_ext)).
:- use_module(library(rdf/rdf_guess)).
:- use_module(library(rdf/rdf_list)).
:- use_module(library(rdfs/rdfs_read)).
:- use_module(library(semweb/turtle)).

:- dynamic(current_manifest/1).

:- rdf_register_prefix(mf, 'http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#').
:- rdf_register_prefix(rdft, 'http://www.w3.org/ns/rdftest#').

manifest_uri('http://www.w3.org/2013/TurtleTests/manifest.ttl').

load_manifest:-
  clean_current_manifest,
  manifest_uri(Uri),
  atom_concat('__', Uri, BNPrefix),
  Opts = [anon_prefix(BNPrefix),base_uri(Uri),format(turtle),graph(manifest)],
  http_get(Uri, rdf_load0(Opts)),
  assert(current_manifest(Uri)).

clean_current_manifest:-
  rdf_retractall(_, _, _, manifest),
  retractall(current_manifest(_)).

rdf_load0(Opts, _, _, Read):-
  rdf_load(Read, Opts).

mf_test(Test):-
  rdf_instance(MF, mf:'Manifest', manifest),
  rdf(MF, mf:entries, Entries, manifest),
  rdf_list_member_raw(Test, Entries, manifest).

run_tests:-
  forall(mf_test(Test),
	 run_test(Test)).

run_test(Test) :-
  announce_test(Test), !,
  rdf(Test, mf:action, Uri, manifest),
  show_data(Uri),
  rdf_guess_format(Uri, Format),
  format('  Format: ~a~n~n~n', [Format]).

announce_test(Test):-
  rdf_literal(Test, mf:name, xsd:string, Name, manifest),
  rdfs_comment(Test, Comment),
  format('Test ~a:~nComment: ~a~n', [Name,Comment]).

show_data(Uri):-
  http_get(Uri, print0).
print0(_, _, Read):-
  read_stream_to_string(Read, S),
  format('```turtle~n~s```~n', [S]).
