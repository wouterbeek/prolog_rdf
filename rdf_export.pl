:- module(
  rdf_export,
  [
    rdf_graph_to_gif/3, % +Graph:atom
                        % -Gif:compound
                        % +Options:list(nvpair)
    rdf_register_class_color/3, % +Graph:atom
                                % +Class:class
                                % +Color:atom
    rdf_register_edge_style/3, % +Graph:atom
                               % +Term:or([bnode,iri,literal])
                               % -EdgeStyle:atom
    rdf_register_predicate_label/3, % +Graph:atom
                                    % +Predicate:iri
                                    % -Label:atom
    rdf_register_prefix_color/3, % +Graph:graph
                                 % +Prefix:atom
                                 % +Color:atom
    rdf_register_prefix_colors/1, % +Options:list(nvpair)
    rdf_term_to_gif/3 % +Term:or([bnode,iri,literal])
                      % -Gif:compound
                      % +Options:list(nvpair)
  ]
).

/** <module> RDF export

Predicates for exporting RDF graphs to the Graph Interchange Format (GIF).

@author Wouter Beek
@version 2013/01-2013/03, 2013/07-2013/09, 2014/01, 2014/07
*/

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(ordsets)).
:- use_module(library(predicate_options)). % Declarations.
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- use_module(dcg(dcg_generic)).
:- use_module(generics(db_ext)).
:- use_module(generics(list_ext)).
:- use_module(graph_theory(graph_theory)).
:- use_module(graph_theory(graph_trav)).
:- use_module(graph_theory(random_vertex_coordinates)).
:- use_module(svg(svg_colors)).

:- use_module(plGraphViz(gv_gif)).

:- use_module(plRdf(rdf_graph)).
:- use_module(plRdf(rdf_graph_theory)).
:- use_module(plRdf(rdf_image)).
:- use_module(plRdf(rdf_name)).
:- use_module(plRdf(rdf_prefix)).
:- use_module(plRdf(rdf_read)).
:- use_module(plRdf_term(rdf_datatype)).
:- use_module(plRdf_term(rdf_term)).

:- rdf_register_prefix(rdf_image, 'http://www.wouterbeek.com/RDF-Image.owl#').

:- dynamic(rdf_class_color0/3).
:- dynamic(rdf_edge_style0/3).
:- dynamic(rdf_predicate_label0/3).
:- dynamic(rdf_prefix_color0/3).

:- rdf_meta(rdf_register_class_color(+,r,+)).
:- rdf_meta(rdf_term_to_gif(r,-,+)).
:- rdf_meta(rdf_vertex_color_by_prefix(+,+,r,-)).

:- predicate_options(rdf_graph_to_gif/3, 3, [
     pass_to(rdf_directed_edge/3, 3),
     pass_to(rdf_graph_to_gif/4, 4)
   ]).
:- predicate_options(rdf_graph_to_gif/4, 4, [
     colorscheme(+oneof([none,svg,x11])),
     pass_to(rdf_term_name//2, 1)
   ]).
:- predicate_options(rdf_register_prefix_colors/1, 1, [
     colorscheme(+oneof([none,svg,x11])),
     graph(+atom)
   ]).
:- predicate_options(rdf_term_to_gif/3, 3, [
     depth(+nonneg),
     graph(+atom),
     pass_to(rdf_directed_edge/3, 3),
     pass_to(rdf_graph_to_gif/4, 4)
   ]).



%! rdf_graph_to_gif(+Graph:atom, +Gif:compound, +Options:list(nvpair)) is det.
% The following options are supported:
%   * `colorscheme(+oneof([none,svg,x11]))`
%     The colorscheme for the colors assigned to vertices and edges.
%     Supported values are `svg`, `x11`, and `none` (for black and white).
%     Default: `svg`.
%   * `iri_description(+oneof([only_all_literals,iri_only,with_all_literals,with_preferred_label]))`
%     Passed to rdf_term_name//2.
%   * `language_preferences(+LanguageTags:list(atom))`
%     Passed to rdf_term_name//2.
%   * `literal_filter(+boolean)`
%     Passed to rdf_directed_edge/3.
%   * `rdf_list_filter(+boolean)`
%     Passed to rdf_directed_edge/3.

rdf_graph_to_gif(Graph, Gif, Options):-
  aggregate_all(
    set(FromV-P-ToV),
    rdf_directed_edge(Graph, FromV-P-ToV, Options),
    Es
  ),
  edges_to_vertices(Es, Vs),
  rdf_graph_to_gif(Vs, Es, Gif, Options).

rdf_graph_to_gif(Vs, Es, Gif, Options1):-
  select_option(colorscheme(Colorscheme), Options1, Options2, svg),
  create_gif(
    Vs,
    Es,
    Gif,
    [
      edge_arrowhead(rdf_export:rdf_edge_arrowhead),
      edge_color(rdf_export:rdf_edge_color(Colorscheme, Graph)),
      edge_label(rdf_export:rdf_edge_label(Graph)),
      edge_style(rdf_export:rdf_edge_style(Graph)),
      graph_colorscheme(Colorscheme),
      graph_label(Graph),
      vertex_color(rdf_export:rdf_vertex_color(Colorscheme, Graph)),
      vertex_image(rdf_export:rdf_vertex_image(Graph)),
      vertex_label(rdf_export:rdf_vertex_label(Options2)),
      vertex_peripheries(rdf_export:rdf_vertex_peripheries),
      vertex_shape(rdf_export:rdf_vertex_shape)
    ]
  ).


%! rdf_register_class_color(+Graph:atom, +Class:iri, +ClassColor:atom) is det.
% A class color can either be registered for a specific RDF graph,
% or for all RDF graph, but leaving the graph argument uninstantiated.

rdf_register_class_color(Graph, Class, ClassColor):-
  db_replace(rdf_class_color0(Graph,Class,ClassColor), [e,e,r]).


%! rdf_register_edge_style(
%!   +Graph:atom,
%!   +Predicate:iri,
%!   +EdgeStyle:atom
%! ) is det.
% An edge style can either be registered for a specific RDF graph,
% or for all RDF graph, but leaving the graph argument uninstantiated.

rdf_register_edge_style(Graph, Predicate, EdgeStyle):-
  db_replace(rdf_edge_style0(Graph,Predicate,EdgeStyle), [e,e,r]).


%! rdf_register_predicate_label(
%!   +Graph:atom,
%!   +Predicate:iri,
%!   -Label:atom
%! ) is det.

rdf_register_predicate_label(Graph, Predicate, Label):-
  db_replace(rdf_predicate_label0(Graph,Predicate,Label), [e,e,r]).


%! rdf_register_prefix_color(
%!   +Graph:atom,
%!   +Prefix:atom,
%!   +PrefixColor:atom
%! ) is det.
% A prefix color can either be registered for a specific RDF graph,
% or for all RDF graph, but leaving the graph argument uninstantiated.

rdf_register_prefix_color(Graph, Prefix, PrefixColor):-
  db_replace(rdf_prefix_color0(Graph,Prefix,PrefixColor), [e,e,r]).


%! rdf_register_prefix_colors(+Options:list(nvpair)) is det.
% Uses colors from the given color scheme to colorize the namespaces in the
% given graph.
%
% The following options are supported:
%   * =|colorscheme(+oneof([none,svg,x11]))
%     The colorscheme that is used for registering colors.
%     Defaut: `svg`.
%   * =|graph(+atom)|=
%     The RDF graph for whose RDF prefixes colors are registered.
%     No default.
%
% @tbd Add support for colorscheme x11.

rdf_register_prefix_colors(Options):-
  % Graph option.
  (
    option(graph(Graph), Options)
  ->
    rdf_prefixes(Graph, Prefixes)
  ;
    rdf_prefixes(Prefixes)
  ),
  length(Prefixes, NumberOfPrefixes),
  NumberOfPrefixes > 0,

  % Colorscheme option.
  option(colorscheme(svg), Options, svg),
  svg_colors(Colors),
  length(Colors, NumberOfColors),

  % Register colors for RDF prefixes.
  Delta is NumberOfColors // NumberOfPrefixes,
  forall(
    nth1(I, Prefixes, Prefix),
    (
      % In case there are more namespaces than colors, Delta=1 and we use
      % the same color for all namespaces with index I mod M.
      J is (I * Delta) mod NumberOfColors,
      % J can be 0 becasue of the modulus function, so do not use nth1/3.
      nth0chk(J, Colors, Color),
      assert(rdf_prefix_color0(Graph, Prefix, Color))
    )
  ).


%! rdf_term_to_gif(
%!   +Term:or([bnode,iri,literal]),
%!   -Gif:compound,
%!   +Options:list(nvpair)
%! ) is det.
% The following options are supported:
%   * =|depth(+nonneg)|=
%     How much context is taken into account when exporting an RDF term.
%     Default: `1`.
%   * =|graph(+atom)|=
%     The name of the RDF graph to which the term description is restricted.
%     No default.
%   * Other options are passed to rdf_graph_to_gif/4 and rdf_directed_edge/3.

rdf_term_to_gif(Term, Gif, Options1):-
  select_option(depth(Depth), Options1, Options2, 1),
  option(graph(Graph), Options2, _VAR),
  bounded_breadthfirst_graph_search(
    rdf_directed_edge(Options2, Graph),
    Depth,
    Term,
    Vs,
    Es
  ),
  rdf_graph_to_gif(Vs, Es, Gif, Options2).
% Aux.
rdf_directed_edge(Options, Graph, FromV, ToV):-
  rdf_directed_edge(Graph, Edge, Options),
  edge(Edge, FromV, ToV).



% Attribute-setting predicates.

%! rdf_edge_arrowhead(+Edge:compound, -ArrowHead:atom) is det.

rdf_edge_arrowhead(_-P-_, box):-
  rdf_memberchk(P, [rdfs:subClassOf]), !.
rdf_edge_arrowhead(_-P-_, diamond):-
  rdf_memberchk(P, [rdfs:subPropertyOf]), !.
rdf_edge_arrowhead(_-P-_, empty):-
  rdf_memberchk(P, [rdf:type]), !.
rdf_edge_arrowhead(_-P-_, none):-
  rdf_memberchk(P, [rdfs:label]), !.
% Others.
rdf_edge_arrowhead(_, normal).


%! rdf_edge_color(
%!   +Colorscheme:oneof([none,svg,x11]),
%!   +Graph:atom,
%!   +Edge:compound,
%!   -Color:atom
%! ) is det.

rdf_edge_color(none, _, _, black):- !.
% The edge color is based on the predicate term.
rdf_edge_color(Colorscheme, Graph, _-P-_, EColor):-
  rdf_vertex_color(Colorscheme, Graph, P, EColor), !.
% If the edge color is not specified, then see whether its vertices
% agree on their color.
rdf_edge_color(Colorscheme, Graph, FromV-_-ToV, EColor):-
  rdf_vertex_color(Colorscheme, Graph, FromV, FromVColor),
  rdf_vertex_color(Colorscheme, Graph, ToV, ToVColor),
  FromVColor == ToVColor, !,
  EColor = FromVColor.
% Others.
rdf_edge_color(_, _, _, black).


%! rdf_edge_label(+Graph:atom, +Edge:compound, -Label:atom) is det.

% Some edge labels are not displayed (e.g., RDF(S) terminology).
rdf_edge_label(_, _-P-_, ''):-
  rdf_memberchk(
    P,
    [rdf:type,rdfs:label,rdfs:subClassOf,rdfs:subPropertyOf]
  ), !.
% Explicitly registered replacements.
rdf_edge_label(Graph, _-P-_, ELabel):-
  rdf_predicate_label0(Graph, P, ELabel), !.
% The edge label is based on the corresponding predicate term.
rdf_edge_label(_, _-P-_, ELabel):-
  % The edge name is the name of the predicate term.
  dcg_with_output_to(atom(ELabel), rdf_term_name([], P)), !.
% The empty label.
rdf_edge_label(_, _, '').


%! rdf_edge_style(+Graph:atom, +Edge:compound, -Style:atom) is det.

% Based on registrations.
rdf_edge_style(Graph, E, EStyle):-
  rdf_edge_style0(Graph, E, EStyle), !.
% Hierarchy edges.
rdf_edge_style(_, _-P-_, solid):-
  rdf_memberchk(P, [rdf:type,rdfs:subClassOf,rdfs:subPropertyOf]), !.
% Label edges.
rdf_edge_style(_, _-P-_, dotted):-
  rdf_memberchk(P, [rdfs:label]), !.
% Others.
rdf_edge_style(_, _, solid).



%! rdf_vertex_color(
%!   +Colorscheme:oneof([none,svg,x11]),
%!   +Graph:atom,
%!   +Term:or([bnode,iri,literal]),
%!   -Color:atom
%! ) is det.
% Returns a color name for the given vertex.

rdf_vertex_color(none, _, _, black):- !.
% Literals.
rdf_vertex_color(_, _, literal(_), blue):- !.
rdf_vertex_color(_, _, literal(lang(_, _)), blue):- !.
rdf_vertex_color(_, _, literal(type(_,_)), blue):- !.
% Individual or subclass of a color-registered class.
rdf_vertex_color(_, Graph, V, VColor):-
  (
    rdfs_individual_of(V, Class)
  ;
    rdfs_subclass_of(V, Class)
  ),
  rdf_class_color0(Graph, Class, VColor), !.
% Resource colored based on its namespace.
rdf_vertex_color(Colorscheme, Graph, V, VColor):-
  (
    % IRI terms with registered namespace/prefix.
    rdf_global_id(_:_, V),
    rdf_vertex_color_by_prefix(Graph, Colorscheme, V, VColor0)
  ->
     VColor = VColor0
  ;
    % IRI terms with unregistered namespace/prefix.
    rdf_iri(V)
  ->
    VColor = red
  ;
    % Non-URI terms, probably blank nodes.
    VColor = purple
  ).


%! rdf_vertex_color_by_prefix(
%!   +Graph:atom,
%!   +Colorscheme:atom,
%!   +Vertex,
%!   -VertexColor:atom
%! ) is det.
% Returns the automatically assigned color name.
% The color names belong to the given colorscheme.
% The color assignments are based on the RDF node's namespace.
% Note that the same node may have different colors in different graphs.
%
% @arg Graph The atomic name of a graph.
% @arg Colorscheme The atomic name of a colorscheme.
%      Currently supported:
%        1. `svg`
%        2. `x11`
% @arg Vertex A resource.
% @arg VertexColor The atomic name of a color within the colorscheme.

rdf_vertex_color_by_prefix(Graph, _, V, VColor):-
  rdf_global_id(Prefix:_, V),
  rdf_prefix_color0(Graph, Prefix, VColor), !.
rdf_vertex_color_by_prefix(Graph, Colorscheme, V, VColor):-
  rdf_register_prefix_colors([colorscheme(Colorscheme),graph(Graph)]),
  rdf_vertex_color_by_prefix(Graph, Colorscheme, V, VColor).


%! rdf_vertex_image(
%!   +Graph:atom,
%!   +Term:or([bnode,iri,literal]),
%!   -ImageFile:atom
%! ) is det.

rdf_vertex_image(Graph, V, VImage):-
  % Only display the first picture that is found for this vertex.
  once(rdf_image(V, _, _, VImage, Graph)).


%! rdf_vertex_label(
%!   +Options:list(nvpair),
%!   +Term:or([bnode,iri,literal]),
%!   -Label:atom
%! ) is det.

rdf_vertex_label(Options1, V, VLabel):-
  merge_options(Options1, [literal_ellipsis(50)], Options2),
  dcg_with_output_to(atom(VLabel), rdf_term_name(Options2, V)).


%! rdf_vertex_peripheries(
%!   +Term:oneof([bnode,literal,uri]),
%!   -Peripheries:integer
%!) is det.

% RDF literals.
rdf_vertex_peripheries(literal(_), 0):- !.
% RDFS classes.
rdf_vertex_peripheries(Term, 2):-
  rdfs_individual_of(Term, rdfs:'Class'), !.
% RDF properties.
rdf_vertex_peripheries(Term, 1):-
  rdfs_individual_of(Term, rdf:'Property'), !.
% RDFS resources that are not RDF properties or RDFS classes.
rdf_vertex_peripheries(Term, 1):-
  rdfs_individual_of(Term, rdfs:'Resource'), !.
% Blank nodes.
rdf_vertex_peripheries(Term, 1):-
  rdf_is_bnode(Term), !.
% Others.
rdf_vertex_peripheries(_, 1).


%! rdf_vertex_shape(+Term:oneof([bnode,iri,literal]), -Shape:atom) is det.
% The following shapes are used:
%   * `circle`
%   * `ellipse`
%   * `hexagon`
%   * `octagon`
%   * `plaintext`

% RDF literals.
rdf_vertex_shape(literal(_), plaintext):- !.
% RDFS class.
rdf_vertex_shape(Term, octagon):-
  rdfs_individual_of(Term, rdfs:'Class'), !.
% RDF property.
rdf_vertex_shape(Term, hexagon):-
  rdfs_individual_of(Term, rdf:'Property'), !.
% RDFS resources that are not RDF properties or RDFS classes.
rdf_vertex_shape(Term, ellipse):-
  rdfs_individual_of(Term, rdfs:'Resource'), !.
% Blank nodes.
rdf_vertex_shape(Term, circle):-
  rdf_is_bnode(Term), !.
% Others.
rdf_vertex_shape(_, ellipse).

