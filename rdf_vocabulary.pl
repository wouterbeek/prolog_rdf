:- module(
  rdf_vocabulary,
  [
    load_rdf_vocabulary/1, % ?RdfGraph:atom
    rdf_vocabulary_pdf/1, % ?File:atom
    rdfs_vocabulary_pdf/1 % ?File:atom
  ]
).

/** <module> RDFS vocabulary

Exports the vocabulary for RDFS.

@author Wouter Beek
@version 2013/08, 2013/11, 2014/03
*/

:- use_module(gv(gv_file)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_export)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf_file(rdf_serial)).
:- use_module(rdf_reasoning(rdf_mat)).
:- use_module(xml(xml_dom)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').

http:location(rdf, root(rdf), []).
:- http_handler(rdf(vocabulary), rdf_vocabulary, []).

user:web_module('RDF vocabulary', rdf_vocabulary).



%! load_rdf_vocabulary(?RdfGraph:atom) is det.
% Pre-load the RDF(S) vocabulary.
% This means that materialization has to make less deductions
% (tested on 163 less), and there are some labels and comments
% that deduction would not produce.

load_rdf_vocabulary(G):-
  rdfs_vocabulary_url(Url),
  (
    nonvar(G)
  ->
    rdf_load_any([graph(G)], Url)
  ;
    rdf_load_any([], Url, [_-G])
  ),
  materialize(
    [entailment_regimes([rdf,rdfs]),multiple_justifications(false)],
    G
  ).


rdf_vocabulary(_Request):-
  reply_html_page(
    app_style,
    title('RDF vocabulary'),
    html([
      h2('RDF vocabulary'),
      \rdf_vocabulary,
      h2('RDFS vocabulary'),
      \rdfs_vocabulary
    ])
  ).

%! rdf_vocabulary// is det.
% Generates an HTML representation of the RDF vocabulary.

rdf_vocabulary -->
  {
    rdf_vocabulary_gif(Gif),
    graph_to_svg_dom([method(sfdp)], Gif, SvgDom)
  },
  html(\xml_dom_as_atom(SvgDom)).


%! rdf_vocabulary_gif(-Gif:compound) is det.
% Returns the RDF vocabulary in graph-interchange-format.

rdf_vocabulary_gif(Gif):-
  load_rdf_vocabulary(G),

  % Customization.
  rdf_retractall(_, rdfs:isDefinedBy, _, G),
  rdf_register_namespace_color(G, rdf, darkblue),

  % Remove the RDFS-only triples.
  forall(
    (
      rdf(S, P, O, G),
      rdf_global_id(rdfs:_, S),
      rdf_global_id(rdfs:_, P),
      rdf_global_id(rdfs:_, O)
    ),
    rdf_retractall(S, P, O, G)
  ),

  % Thats it, let's export the RDF graph to GIF.
  export_rdf_graph(
    [
      colorscheme(svg),
      edge_labels(replace),
      language(en),
      literals(preferred_label),
      uri_desc(uri_only)
    ],
    G,
    Gif
  ).


%! rdf_vocabulary_pdf(?File:atom) is det.
% Saves the RDF vocabulary to a PDF file.

rdf_vocabulary_pdf(File):-
  (
    nonvar(File)
  ->
    access_file(File, write)
  ;
    true
  ),
  rdf_vocabulary_gif(Gif),
  graph_to_gv_file([method(sfdp),to_file_type(pdf)], Gif, File).


%! rdfs_vocabulary// is det.
% Generates an HTML description of the RDFS vocabulary.

rdfs_vocabulary -->
  {
    rdfs_voc(Gif),
    graph_to_svg_dom([method(sfdp)], Gif, SvgDom)
  },
  html(\xml_dom_as_atom(SvgDom)).


%! rdfs_vocabulary_gif(?File:atom) is det.
% Returns the RDFS vocabulary in graph-interchange-format.

rdfs_vocabulary_gif(Gif):-
  load_rdf_vocabulary(G),

  % Customization.
  rdf_retractall(_, rdfs:isDefinedBy, _, G),
  rdf_register_namespace_color(G, rdf, darkblue),
  rdf_register_namespace_color(G, rdfs, darkgreen),

  % Thats it, let's export the RDF graph to GIF.
  export_rdf_graph(
    [
      colorscheme(svg),
      edge_labels(replace),
      language(en),
      literals(all),
      uri_desc(uri_only)
    ],
    G,
    Gif
  ).


%! rdfs_vocabulary_pdf(?File:atom) is det.
% Returns the RDFS vocabulary in graph-interchange-format.

rdfs_vocabulary_pdf(File):-
  (
    nonvar(File)
  ->
    access_file(File, write)
  ;
    true
  ),
  rdfs_voc(Gif),
  graph_to_gv_file([method(sfdp),to_file_type(pdf)], Gif, File).


rdfs_vocabulary_url('http://www.w3.org/1999/02/22-rdf-syntax-ns#').

