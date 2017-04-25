dcg_rdf_print_dataset_term(D, _) -->
  {rdf_dataset_label(D, Lbl)}, !,
  atom(Lbl).

dcg_rdf_print_graph_term(G, Opts) -->
  {
    rdf_dataset_graph(D, G),
    rdf_graph_label(G, Lbl)
  }, !,
  dcg_rdf_print_dataset_term(D, Opts),
  "/",
  atom(Lbl).
