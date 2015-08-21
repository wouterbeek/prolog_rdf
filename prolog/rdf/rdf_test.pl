:- module(rdf_test, [load_manifest/0,run_tests/0]).

:- use_module(library(apply)).
:- use_module(library(http/http_request)).
:- use_module(library(rdf/rdf_guess)).
:- use_module(library(rdfs/rdfs_read)).
:- use_module(library(semweb/turtle)).

:- dynamic(mf_manifest/1).
:- dynamic(mf_rdf/3).

:- rdf_meta(mf_rdf(r,r,o)).

:- rdf_register_prefix(mf, 'http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#').
:- rdf_register_prefix(rdft, 'http://www.w3.org/ns/rdftest#').

manifest_uri('http://www.w3.org/2013/TurtleTests/manifest.ttl').

load_manifest:-
  clean_manifests,
  manifest_uri(Uri),
  atom_concat('__', Uri, BNPrefix),
  http_get(Uri, rdf_read_turtle0([anon_prefix(BNPrefix)])),
  assert(mf_manifest(Uri)).

rdf_read_turtle0(Opts, _, _, Read):-
  rdf_read_turtle(Read, Ts, Opts),
  maplist(mf_assert, Ts).

clean_manifests:-
  retractall(mf_manifest(_)),
  retractall(mf_rdf(_,_,_)).

mf_assert(rdf(S,P,O)):-
  assert(mf_rdf(S,P,O)).

mf_test(Test):-
  mf_rdf(MF, rdf:type, mf:'Manifest'),
  mf_rdf(MF, mf:entries, Entries),
  mf_member(Test, Entries).

mf_member(Test, Collection) :-
  mf_rdf(Collection, rdf:first, Test).
mf_member(Test, Collection) :-
  mf_rdf(Collection, rdf:rest, Next),
  mf_member(Test, Next).

run_tests:-
  rdf_reset_db,
  retractall(test_result(_,_,_)),
  forall(mf_test(Test),
	 run_test(Test)).

run_test(Test) :-
  rdf_retractall(_,_,_,_),
  announce_test(Test), !,
  mf_rdf(Test, mf:action, Uri),
  rdf_guess_format(Uri, Format),
  format('Format: ~a~n', [Format]).

announce_test(Test):-
  mf_rdf(Test, mf:name, literal(Name)),
  format('Test ~q ...~n', [Name]).
