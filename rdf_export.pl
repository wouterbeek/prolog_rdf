:- module(
  rdf_export,
  [
% COLORS
    rdf_register_prefix_color/3, % +Graph:graph
                                 % +Prefix:atom
                                 % +Color:atom
    rdf_register_prefix_colors/1, % +Options:list(nvpair)
    rdf_register_class_color/3, % +Graph:atom
                                % +Class:class
                                % +Color:atom
    rdf_register_edge_style/2, % +Term:or([bnode,iri,literal])
                               % -EdgeStyle:atom

% GRAPH EXPORT
    rdf_graph_to_gif/3, % +Graph:atom
                        % -Gif:compound
                        % +Options:list(nvpair)
    rdf_edge_term/5, % +Graph:atom
                     % +Vertices:list(iri)
                     % +Edge:pair(iri)
                     % -EdgeTerm:compound
                     % +Options:list(nvpair)
    rdf_vertex_term/5 % +Graph:atom
                      % +Vertices:list(iri)
                      % +Vertex:iri
                      % -VertexTerm:compound
                      % +Options:list(nvpair)
  ]
).

/** <module> RDF export

Predicates for exporting RDF.

### Edge naming

Specific edge labels can be hidden by adding clauses to
rdf_edge_label_replace/2.

### Edge styling

### Vertex coloring

The procedure for determining the color of a vertex:
  1. Look whether the =colorscheme= option is not set to =none=.
  2. See whether the vertex is an individual of a colored class.
  3. See whether the vertex belongs to a colored namespace.
  4. If at least one vertex is not colored by class or namespace, then all
     namespaces are (re)assigned colors.

### Vertex naming

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
:- use_module(graph_theory(graph_generic)).
:- use_module(graph_theory(random_vertex_coordinates)).
:- use_module(Graph(rdf_graph_theory)).
:- use_module(svg(svg_colors)).

:- use_module(plRdf(rdf_graph)).
:- use_module(plRdf(rdf_name)).
:- use_module(plRdf(rdf_namespace)).
:- use_module(plRdf(rdf_read)).
:- use_module(plRdf_term(rdf_datatype)).
:- use_module(plRdf_term(rdf_term)).

:- rdf_register_prefix(rdf_image, 'http://www.wouterbeek.com/RDF-Image.owl#').

:- dynamic(rdf_class_color0/3).
:- dynamic(rdf_edge_style0/3).
:- dynamic(rdf_prefix_color0/3).

:- meta_predicate(rdf_vertex_term(+,+,+,4,+,-)).

% Color.
:- rdf_meta(rdf_register_class_color(+,r,+)).
:- rdf_meta(rdf_vertex_color_by_namespace(+,+,r,-)).

% Vertices.
:- rdf_meta(rdf_vertex_color(+,+,r,-)).
:- rdf_meta(rdf_vertex_peripheries(r,-)).
:- rdf_meta(rdf_vertex_picture(+,r,-)).
:- rdf_meta(rdf_vertex_shape(r,-)).
:- rdf_meta(rdf_vertex_term(+,+,+,:,r,-)).

:- multifile(rdf_edge_name/2).



% COLOR %

%! rdf_register_prefix_color(
%!   +Graph:atom,
%!   +Prefix:atom,
%!   +PrefixColor:atom
%! ) is det.
% A prefix color can either be registered for a specific RDF graph,
% or for all RDF graph, but leaving the graph argument uninstantiated.

rdf_register_prefix_color(Graph, Prefix, PrefixColor):-
  db_replace_novel(
    rdf_prefix_color0(Graph, Prefix, PrefixColor),
    [e,e,r]
  ).


%! rdf_register_prefix_colors(+Options:list(nvpair)) is det.
% Uses colors from the given color scheme to colorize the namespaces in the
% given graph.
%
% The following options are supported:
%   * =|graph(+atom)|=
%     The RDF graph for whose RDF prefixes colors are registered.
%     No default.
%   * =|colorscheme(+oneof([svg,x11]))
%     The colorscheme that is used for registering colors.
%     Defaut: `svg`.
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


%! rdf_register_class_color(+Graph:atom, +Class:iri, +ClassColor:atom) is det.
% A class color can either be registered for a specific RDF graph,
% or for all RDF graph, but leaving the graph argument uninstantiated.

rdf_register_class_color(Graph, Class, ClassColor):-
  db_replace_novel(rdf_class_color0(Graph, Class, ClassColor), [e,e,r]).


%! rdf_register_edge_style(
%!   +Graph:atom,
%!   +Predicate:iri,
%!   +EdgeStyle:atom
%! ) is det.
% An edge style can either be registered for a specific RDF graph,
% or for all RDF graph, but leaving the graph argument uninstantiated.

rdf_register_edge_style(Graph, Predicate, EdgeStyle):-
  db_replace_novel(rdf_edge_style0(Graph, Predicate, EdgeStyle), [e,e,r]).


%! rdf_vertex_color_by_namespace(
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

rdf_vertex_color_by_namespace(Graph, _, V, VColor):-
  rdf_global_id(Prefix:_, V),
  rdf_prefix_color0(Graph, Prefix, VColor), !.
rdf_vertex_color_by_namespace(Graph, Colorscheme, V, VColor):-
  rdf_register_prefix_colors(Graph, Colorscheme),
  rdf_vertex_color_by_namespace(Graph, Colorscheme, V, VColor).



% GRAPH EXPORT %

%! rdf_graph_to_gif(+Graph:atom, +Gif:compound, +Options:list(nvpair)) is det.
% The following options are supported:
%   1. `colorscheme(+Colorscheme:atom)`
%      The colorscheme for the colors assigned to vertices and edges.
%      Supported values are `svg`, `x11` (default), and the
%      Brewer colorschemes (see module [brewer.pl].
%   2. `coordinate_function(+Function)`
%      The function that is used to determine the coordinates of the vertices.
%      Default: random_vertex_coordinate/2.
%   3. `edge_labels(oneof([all,none,replace]))`
%      Whether edge labels are included (`all`),
%      not included (`none`), or
%      replaced by alternative labels (`replace`, default).
%   4. `language(+Language:atom)`
%      The atomic language tag of the language that is preferred for
%      use in the RDF term's name.
%      The default value is `en`.
%   5. `literals(+Include:oneof([all,none,preferred_label]))`
%      Whether all (`all`, default), none (`none`) or only preferred label
%      literals (`preferred_label`) are included as vertices.
%   6. `uri_desc(+DescriptionMode:oneof([only_literals,uri_only,with_literals,with_preferred_label]))`
%      Whether or not literals are included in the name of the RDF term.
%      The default value is `uri_only`.

rdf_graph_to_gif(Graph, graph(VTerms,ETerms,GAttrs), Options):-
  % First edges, them vertices.
  rdf_edges(Graph, Es, Options),
  % @tbd
  edges_to_vertices(Es, Vs),

  maplist(rdf_edge_term(Options, Graph, Vs), Es, ETerms),
  maplist(rdf_vertex_term(Options, Graph, Vs), Vs, VTerms),

  % Graph
  rdf_graph_name(Graph, GName),
  option(colorscheme(Colorscheme), Options, x11),
  GAttrs = [colorscheme(Colorscheme),dir(forward),label(GName)].


%! rdf_graph_name(+Graph:Graph, -GraphName:atom) is det.
% Returns a name for the given graph.

rdf_graph_name(Graph, Graph):- !.



% EDGE EXPORT %

%! rdf_edge_arrow_head(+Edge:edge, -E_ArrowHead:atom) is det.

rdf_edge_arrow_head(_FromV-P-_ToV, box):-
  rdf_memberchk(P, [rdfs:subClassOf]), !.
rdf_edge_arrow_head(_FromV-P-_ToV, diamond):-
  rdf_memberchk(P, [rdfs:subPropertyOf]), !.
rdf_edge_arrow_head(_FromV-P-_ToV, empty):-
  rdf_memberchk(P, [rdf:type]), !.
rdf_edge_arrow_head(_FromV-P-_ToV, none):-
  rdf_memberchk(P, [rdfs:label]), !.
rdf_edge_arrow_head(_E, normal).

rdf_edge_color(O1, _G, _E, black):-
  option(colorscheme(none), O1, none), !.
% The edge color is based on the predicate term.
rdf_edge_color(O1, Graph, _FromV-P-_ToV, E_Color):-
  rdf_vertex_color(O1, Graph, P, E_Color), !.
% If the edge color is not specified, then see whether its vertices
% agree on their color.
rdf_edge_color(O1, Graph, FromV-_P-ToV, E_Color):-
  rdf_vertex_color(O1, Graph, FromV, FromV_Color),
  rdf_vertex_color(O1, Graph, ToV, ToV_Color),
  FromV_Color = ToV_Color, !,
  E_Color = FromV_Color.

%! rdf_edge_name(
%!   +Options:list(nvpair),
%!   +Edge:compound,
%!   -EdgeName:list(nvpair)
%! ) is det.
% Returns a name for the given edge.
%
% The following options are supported:
%   1. `edge_labels(oneof([all,none,replace]))`
%      Whether edge labels are included (`all`),
%      not included (`none`), or
%      replaced by alternative labels (`replace`, default).
%   2. `language(+Language:atom)`
%      The atomic language tag of the language that is preferred for
%      use in the RDF term's name.
%      The default value is `en`.
%      (Passed to rdf_term_name//2.)
%   3. `uri_desc(+DescriptionMode:oneof([uri_only,with_literals,with_preferred_label]))`
%      Whether or not literals are included in the name of the RDF term.
%      The default value is `uri_only`.
%      (Passed to rdf_term_name//2.)

% Make use of explicit replacements.
rdf_edge_name(O1, _FromV-P-_ToV, [label(E_Name)]):-
  option(edge_labels(replace), O1, replace), !,
  (
    rdf_edge_name(P, Replacement)
  ->
    E_Name = Replacement
  ;
    dcg_with_output_to(atom(E_Name), rdf_term_name(O1, P))
  ).
rdf_edge_name(O1, _FromV-P-_ToV, [label(E_Name)]):-
  option(edge_labels(all), O1, replace), !,
  % The edge name is the name of the predicate term.
  dcg_with_output_to(atom(E_Name), rdf_term_name(O1, P)).
rdf_edge_name(_O, _E, []).

%! rdf_edge_name(+Edge:uri, -Replacement:atom) is det.
% Some edge labels are not displayed (e.g., RDF(S) terminology).

rdf_edge_name(P, ''):-
  rdf_memberchk(
    P,
    [rdf:type,rdfs:label,rdfs:subClassOf,rdfs:subPropertyOf]
  ).

%! rdf_edge_style(+Edge:edge, -E_Style:atom) is det.

rdf_edge_style(_FromV-P-_ToV, solid):-
  rdf_memberchk(P, [rdf:type, rdfs:subClassOf, rdfs:subPropertyOf]), !.
rdf_edge_style(_FromV-P-_ToV, dotted):-
  rdf_memberchk(P, [rdfs:label]), !.
% Dynamic assertion.
rdf_edge_style(E, E_Style):-
  rdf_edge_style0(E, E_Style), !.
rdf_edge_style(_E, solid).

rdf_edge_term(O1, Graph, Vs, E, edge(FromV_Id,ToV_Id,E_Attrs)):-
  % Ids.
  (E = FromV-_P-ToV, ! ; E = FromV-ToV),
  nth0chk(FromV_Id, Vs, FromV),
  nth0chk(ToV_Id, Vs, ToV),

  % Arrow head.
  rdf_edge_arrow_head(E, E_ArrowHead),

  % Color.
  rdf_edge_color(O1, Graph, E, E_Color),

  % Label.
  rdf_edge_name(O1, E, E_NameLIST),

  % Style.
  rdf_edge_style(E, E_Style),

  % The colorscheme cannot be set on the graph or shared edge, apparently.
  (
    option(colorscheme(Colorscheme), O1)
  ->
    ColorschemeAttrs = [colorscheme(Colorscheme)]
  ;
    ColorschemeAttrs = []
  ),
  merge_options(
    E_NameLIST,
    [arrowhead(E_ArrowHead),color(E_Color),style(E_Style)|ColorschemeAttrs],
    E_Attrs
  ).



% VERTEX EXPORT %

%! rdf_vertex_color(
%!   +Options:list(nvpair),
%!   +Graph:atom,
%!   +Vertex:vertex,
%!   -Color:atom
%! ) is det.
% Returns a color name for the given vertex.
%
% The following options are supported:
%   1. `colorscheme(+Colorscheme:atom)`
%      The colorscheme for the colors assigned to vertices and edges.
%      Supported values are `svg`, `x11` (default), and the
%      Brewer colorschemes (see module [brewer.pl].
%
% @arg Options A list of name-value pairs.
% @arg Vertex A vertex.
% @arg Color A color name.

rdf_vertex_color(O1, _G, _V, black):-
  option(colorscheme(none), O1), !.
% Literals.
rdf_vertex_color(_O, _G, literal(_Value), blue):- !.
rdf_vertex_color(_O, _G, literal(lang(_Lang, _Value)), blue):- !.
rdf_vertex_color(_O, _G, literal(type(_Datatype, _LexicalValue)), blue):- !.
% Individual or subclass of a color-registered class.
rdf_vertex_color(_O, Graph, V, VColor):-
  (
    rdfs_individual_of(V, Class)
  ;
    rdfs_subclass_of(V, Class)
  ),
  rdf_class_color0(Graph, Class, VColor), !.
% Resource colored based on its namespace.
rdf_vertex_color(O1, Graph, V, VColor):-
  option(colorscheme(Colorscheme), O1, svg),
  (
    % URI resources with registered namespace/prefix.
    rdf_global_id(_:_, V),
    rdf_vertex_color_by_namespace(Graph, Colorscheme, V, V_NamespaceColor)
  ->
     VColor = V_NamespaceColor
  ;
    % URI resources with unregistered namespace/prefix.
    rdf_iri(V)
  ->
    VColor = red
  ;
    % Non-URI resources, probably blank nodes.
    VColor = purple
  ).

rdf_vertex_label(V, VLabel):-
  dcg_with_output_to(atom(VLabel), rdf_term_name([literal_ellipsis(50)], V)).

%! rdf_vertex_peripheries(
%!   +Term:oneof([bnode,literal,uri]),
%!   -Peripheries:integer
%!) is det.

% RDF literals.
rdf_vertex_peripheries(literal(_Literal), 0):- !.
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
% Catch-all.
rdf_vertex_peripheries(_RDF_Term, 1).

rdf_vertex_picture(Graph, V, V_Picture):-
  % Only display the first picture that is related to this vertex.
  once(rdf_datatype(V, _, V_Picture, rdf_image:image, Graph)).

%! rdf_vertex_shape(+Term:oneof([bnode,literal,uri]), -Shape:atom) is det.
% The following shapes are supported:
%   * `circle`
%   * `ellipse`
%   * `hexagon`
%   * `octagon`
%   * `plaintext`
%
% @arg Shape The atomic name of a vertex shape.

% RDF literals.
rdf_vertex_shape(literal(_Literal), plaintext):- !.
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
% Catch-all.
rdf_vertex_shape(_RDF_Term, ellipse).

rdf_vertex_term(O1, Graph, Vs, V, Gif):-
  rdf_vertex_term(O1, Graph, Vs, random_vertex_coordinate, V, Gif).

rdf_vertex_term(O1, Graph, Vs, CoordFunc, V, vertex(V_Id,V,V_Attrs3)):-
  nth0chk(V_Id, Vs, V),
  dcg_with_output_to(atom(V_Name), rdf_term_name(O1, V)),
  rdf_vertex_color(O1, Graph, V, VColor),
  call(CoordFunc, O1, Vs, V, V_Coord),
  rdf_vertex_peripheries(V, V_Peripheries),
  rdf_vertex_shape(V, V_Shape),
  V_Attrs1 = [
    color(VColor),
    coord(V_Coord),
    label(V_Name),
    peripheries(V_Peripheries),
    shape(V_Shape)
  ],
  (
    rdf_vertex_picture(Graph, V, V_Picture)
  ->
    merge_options([image(V_Picture)], V_Attrs1, V_Attrs2)
  ;
    V_Attrs2 = V_Attrs1
  ),
  % The colorscheme cannot be set on the graph or shared edge, apparently.
  (
    option(colorscheme(Colorscheme), O1)
  ->
    merge_options([colorscheme(Colorscheme)], V_Attrs2, V_Attrs3)
  ;
    V_Attrs3 = V_Attrs2
  ).

