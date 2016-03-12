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
:- use_module(library(ansi_ext)).
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
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(rdf/rdf_load)).
:- use_module(library(terms)).





run_test(I) :-
  var(I), !,
  between(1, inf, I),
  run_test(I).
run_test(I) :-
  format_integer(I, 4, Id0),
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

run_test0(D):-
  % Metadata.
  formatln("Id: ~s", [D.'@id']),
  write("Type: "), forall(member(Type, D.'@type'), write(Type)), nl,
  formatln("Name: ~s", [D.name]),
  formatln("Purpose: ~s", [D.purpose]),

  % Print JSON-LD.
  json_read_any(D.input, DIn),
  print_dict(DIn), nl,
  rdf_load_tuples(D.expect, Stmts2),

  % Read RDF from JSON-LD.
  atomic_concat('http://json-ld.org/test-suite/tests/', D.input, Base),
  rdf_unload_db,
  (   rdf_load_tuples(D.input, Stmts1, [base_iri(Base)]),
      formatln("Parsed tuples:"),
      rdf_print(Stmts1),
      
      % Compare to RDF from N-Quads.
      isomorphic_tuples(Stmts1, Stmts2)
  ->  true
  ;   ansi_formatln([fg(red)], "Expected tuples:", []),
      rdf_print(Stmts2)
  ), !.
run_test0(D) :-
  ansi_formatln([fg(red)], "Test ~w failed.", [D]).

%! isomorphic_graphs(+G1, +G2) is semidet.
%! isomorphic_tuples(+Stmts1, +Stmts2) is semidet.
% Is true if there is a consistent mapping between the blank nodes in 1
% and the blank nodes in 2 that makes both structures equal.  This maps to
% the Prolog notion of *variant* if there was a canonical ordering of triples.

isomorphic_graphs(G1, G2) :-
  maplist(rdf_graph_to_triples, [G1,G2], [Triples1,Triples2]),
  isomorphic_tuples(Triples1, Triples2).

isomorphic_tuples(Stmts1, Stmts2) :-
  once(tuples_permutation(Stmts1, Perm1)),
  % NONDET.
  tuples_permutation(Stmts2, Perm2),
  variant(Perm1, Perm2), !.

tuples_permutation(Stmts1, Perm) :-
  replace_bnodes_with_vars(Stmts1, Stmts2),
  partition(ground, Stmts2, Ground, NonGround),
  sort(Ground, Sorted),
  append(Sorted, NonGroundPermutation, Perm),
  permutation(NonGround, NonGroundPermutation).

replace_bnodes_with_vars(L1, L2) :-
  replace_bnodes_with_vars(L1, [], L2).

replace_bnodes_with_vars([], _, []).
replace_bnodes_with_vars([H1|T1], Map1, [H2|T2]) :-
  replace_bnodes_with_vars0(H1, Map1, H2, Map2),
  replace_bnodes_with_vars(T1, Map2, T2).

replace_bnodes_with_vars0(rdf(S1,P,O1), Map1, rdf(S2,P,O2), Map3) :- !,
  replace_bnode_with_var0(S1, Map1, S2, Map2),
  replace_bnode_with_var0(O1, Map2, O2, Map3).
replace_bnodes_with_vars0(rdf(S1,P,O1,G), Map1, rdf(S2,P,O2,G), Map3) :-
  replace_bnode_with_var0(S1, Map1, S2, Map2),
  replace_bnode_with_var0(O1, Map2, O2, Map3).

replace_bnode_with_var0(B, Map, Var, Map) :-
  rdf_is_bnode(B),
  memberchk(B-Var, Map), !.
replace_bnode_with_var0(B, Map, Var, [B-Var|Map]) :-
  rdf_is_bnode(B), !.
replace_bnode_with_var0(T, Map, T, Map).
