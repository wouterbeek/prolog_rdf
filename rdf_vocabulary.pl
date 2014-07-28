:- module(
  rdf_vocabulary,
  [
    load_rdf_vocabulary/1, % ?RdfGraph:atom
    rdf_vocabulary_gif/1, % -Gif:compound
    rdfs_vocabulary_gif/1 % -Gif:compound
  ]
).

/** <module> RDFS vocabulary

Exports the vocabulary for RDFS.

@author Wouter Beek
@version 2013/08, 2013/11, 2014/03, 2014/06-2014/07
*/

:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(semweb/rdf_db)).

:- use_module(xml(xml_dom)).

:- use_module(plRdf(rdf_export)).
:- use_module(plRdf(rdf_graph)).
:- use_module(plRdf_ent(rdf_mat)).
:- use_module(plRdf_ser(rdf_serial)).

:- rdf_register_prefix(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').



%! load_rdf_vocabulary(?RdfGraph:atom) is det.
% Pre-load the RDF(S) vocabulary.
% This means that materialization has to make less deductions
% (tested on 163 less), and there are some labels and comments
% that deduction would not produce.

load_rdf_vocabulary(Graph):-
  rdfs_vocabulary_url(Url),
  rdf_load_any(Url, [graph(Graph)]),
  rdf_materialize(
    Graph,
    [
      max_enumerator(3),
      entailment_regimes([rdf,rdfs]),
      multiple_justifications(false)
    ]
  ).

rdfs_vocabulary_url('http://www.w3.org/1999/02/22-rdf-syntax-ns#').


%! rdf_vocabulary_gif(-Gif:compound) is det.
% Returns the RDF vocabulary in graph-interchange-format.

rdf_vocabulary_gif(Gif):-
  setup_call_cleanup(
    load_rdf_vocabulary(Graph),
    rdf_vocabulary_gif(Graph, Gif),
    rdf_unload_graph(Graph)
  ).

%! rdf_vocabulary_gif(+Graph:atom, -Gif:compound) is det.

rdf_vocabulary_gif(Graph, Gif):-
  % Customization.
  rdf_retractall(_, rdfs:isDefinedBy, _, Graph),
  rdf_register_prefix_color(Graph, rdf, darkblue),

  % Remove the RDFS-only triples.
  forall(
    (
      rdf(S, P, O, Graph),
      rdf_global_id(rdfs:_, S),
      rdf_global_id(rdfs:_, P),
      rdf_global_id(rdfs:_, O)
    ),
    rdf_retractall(S, P, O, Graph)
  ),

  % Thats it, let's export the RDF graph to GIF.
  rdf_graph_to_gif(
    [
      colorscheme(svg),
      edge_labels(replace),
      language(en),
      literals(preferred_label),
      iri_description(iri_only)
    ],
    Graph,
    Gif
  ).


%! rdfs_vocabulary_gif(?File:atom) is det.
% Returns the RDFS vocabulary in graph-interchange-format.

rdfs_vocabulary_gif(Gif):-
  load_rdf_vocabulary(Graph),

  % Customization.
  rdf_retractall(_, rdfs:isDefinedBy, _, Graph),
  rdf_register_prefix_color(Graph, rdf, darkblue),
  rdf_register_prefix_color(Graph, rdfs, darkgreen),

  % Thats it, let's export the RDF graph to GIF.
  rdf_graph_to_gif(
    [
      colorscheme(svg),
      edge_labels(replace),
      language(en),
      literals(all),
      iri_description(iri_only)
    ],
    Graph,
    Gif
  ).

