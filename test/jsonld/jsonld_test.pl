:- module(
  jsonld_test,
  [
    run_test/1, % ?Number:positive_integer
    run_tests/0
  ]
).

/** <module> JSON-LD tests

Largely derived from the JSON-LD 1.0 specification (W3C).

@author Wouter Beek
@version 2016/02-2016/03
*/

:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(debug_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(error)).
:- use_module(library(http/http_download)).
:- use_module(library(json_ext)).
:- use_module(library(jsonld/jsonld_build)).
:- use_module(library(jsonld/jsonld_read)).
:- use_module(library(lists)).
:- use_module(library(print_ext)).
:- use_module(library(q/q_print)).
:- use_module(library(rdf/rdf__io)).
:- use_module(library(terms)).





run_test(I) :-
  var(I), !,
  between(1, inf, I),
  run_test(I).
run_test(I) :-
  integer_padding(I, 4, Id0),
  atomic_list_concat([toRdf,Id0,in], -, Base),
  file_name_extension(Base, jsonld, Local),
  (   access_file(Local, read)
  ->  string_concat("#t", Id0, Id),
      json_read_any('toRdf-manifest.jsonld', Manf),
      Ds = Manf.sequence,
      member(D, Ds),
      D.'@id' == Id, !,
      run_test0(D)
  ;   formatln("Test ~D does not exist.", [I])
  ).

run_tests :-
  catch(
    forall(between(1, inf, I), run_test(I)),
    E,
    writeln(E)
  ).


%! run_test0(+D) is det.

run_test0(Dict):-
  % Metadata.
  formatln("Id: ~s", [Dict.'@id']),
  write("Type: "), forall(member(Type, Dict.'@type'), write(Type)), nl,
  formatln("Name: ~s", [Dict.name]),
  formatln("Purpose: ~s", [Dict.purpose]),

  % Print JSON-LD.
  json_read_any(Dict.input, DictIn),
  print_dict(DictIn),
  rdf_load_quads(Dict.expect, Quads2),

  % Read RDF from JSON-LD.
  atomic_concat('http://json-ld.org/test-suite/tests/', Dict.input, Base),
  rdf_unload_db,
  (   rdf_load_quads(Dict.input, Quads1, [base_iri(Base)]),
      formatln("Parsed tuples:"),
      q_print_quads(Quads1),
      
      % Compare to RDF from N-Quads.
      isomorphic_tuples(Quads1, Quads2)
  ->  true
  ;   ansi_format(user_output, [fg(red)], "Expected tuples:~n", []),
      q_print_quads(Quads2)
  ), !.
run_test0(Dict) :-
  ansi_format(user_output, [fg(red)], "Test ~w failed.~n", [Dict]).
