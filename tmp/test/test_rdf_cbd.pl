:- use_module(library(print_ext)).
:- use_module(library(rdf/rdf__io)).
:- use_module(library(rdf/rdf_isomorphism)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(rdf/rdf_shape)).

:- meta_predicate
    test(2, +).





test :-
  test(q_cbd, _{bnode_counter: 0}),
  test(q_scbd, _{bnode_counter: 0}).


test(Mod:Mode, Opts) :-
  rdf_reset_db,
  load_and_print_graph0(sourcegraph, Opts),
  StartNode = 'http://example.org/aReallyGreatBook',
  call(Mod:Mode, StartNode, Triples),
  msg_notification("Computed ~a:~n", [Mode]),
  rdf_print_triples(Triples, Opts),
  load_and_print_graph0(Mode, Opts),
  rdf_isomorphic_graphs(Triples, Mode).


load_and_print_graph0(G, Opts) :-
  file_name_extension(G, rdf, File),
  absolute_file_name(File, Path, [access(read)]),
  rdf_load_file(Path, [graph(G)]),
  msg_notification("~a:~n", [G]),
  rdf_print_graph(G, Opts).