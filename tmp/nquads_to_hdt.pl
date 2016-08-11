% No longer needed now that N-Quads are converted to N-Triples in the
% Quine store.

% N-Quads → N-Triples
% @tbd Move this to source2store.
q_store2cache(hdt, G) :-
  q_graph_to_file(store, G, nquads, FromFile),
  q_graph_to_file(store, G, ntriples, ToFile),
  create_file_directory(ToFile),
  setup_call_cleanup(
    rdf_change_format(
      FromFile,
      ToFile,
      [from_format(nquads),to_format(ntriples)]
    ),
    indent_debug_call(
      q(q_io),
      "N-Quads → N-Triples",
      q_store2cache(hdt, G)
    ),
    delete_file(ToFile)
  ).
