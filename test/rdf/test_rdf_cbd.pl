:- use_module(library(print_ext)).
:- use_module(library(rdf/rdf_isomorphism)).
:- use_module(library(rdf/rdfio)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(z/z_cbd)).
:- use_module(library(z/z_print)).

:- meta_predicate
    test(2, +).





test :-
  test(z_cbd, _{bnode_counter: 0}),
  test(z_scbd, _{bnode_counter: 0}).


test(Mod:Mode, Opts) :-
  rdf_reset_db,
  load_and_print_graph0(sourcegraph, Opts),
  StartNode = 'http://example.org/aReallyGreatBook',
  call(Mod:Mode, StartNode, Triples),
  msg_notification("Computed ~a:~n", [Mode]),
  z_print_triples(Triples, Opts),
  load_and_print_graph0(Mode, Opts),
  rdf_isomorphic_graphs(Triples, Mode).


load_and_print_graph0(G, Opts) :-
  file_name_extension(G, rdf, File),
  absolute_file_name(File, Path, [access(read)]),
  rdf_load_file(Path, [graph(G)]),
  msg_notification("~a:~n", [G]),
  z_print_graph(G, Opts).
