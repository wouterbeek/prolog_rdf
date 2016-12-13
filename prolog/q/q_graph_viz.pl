:- module(
  q_graph_viz,
  [
    q_graph_to_export_graph/2, % +G, -ExportG
    q_graph_to_export_graph/3  % +G, -ExportG, +Opts
  ]
).

/** <module> RDF graph visualization

Predicates for exporting RDF graphs to the graph export format
handled by plGraphViz.

@author Wouter Beek
@version 2015/07, 2015/10, 2015/12-2016/03, 2016/12
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(graph/build_export_graph)).
:- use_module(library(graph/l/l_graph)).
:- use_module(library(gv/gv_color)).
:- use_module(library(list_ext)).
:- use_module(library(option)).
:- use_module(library(q/q_graph_theory)).
:- use_module(library(q/q_prefix), []).
:- use_module(library(q/q_print)).
:- use_module(library(q/q_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs), [
     rdfs_individual_of/2,
     rdfs_subclass_of/2
   ]).
:- use_module(library(typecheck)).

:- multifile
    rdf:q_class_color/2,
    rdf:q_edge_style/2,
    rdf:q_predicate_label//1.

:- rdf_meta
   q_term_to_export_graph(+, r, -, +).





%! create_namespace_map(
%!   +Scheme:oneof([svg,x11]),
%!   -AliasColors:list(pair)
%! ) is det.

create_namespace_map(Scheme, Map) :-
  aggregate_all(set(Alias), q_alias(Alias), Aliases),
  create_namespace_map(Aliases, Scheme, Map).



%! create_namespace_map(
%!   +Aliases:list(atom),
%!   +Colorscheme:oneof([svg,x11]),
%!   -AliasColors:list(pair)
%! ) is det.

create_namespace_map(Aliases, Scheme, Map) :-
  length(Aliases, NP),
  aggregate_all(set(C), gv_color(Scheme, C), Cs),
  length(Cs, NC),
  Delta is NC // NP,
  findall(
    Alias-C,
    (
      nth0(I, Aliases, Alias),
      % In case there are more namespaces than colors, Delta=1 and we use
      % the same color for all namespaces with index I mod M.
      J is (I * Delta) mod NC,
      % J can be 0 because of the modulus function so we do not use nth1/3.
      nth0chk(J, Cs, C)
    ),
    Map
  ).



%! q_edges_to_export_graph(+Es, -ExportG, +Opts) is det.
%
% The following options are supported:
%
%   * colorscheme(+oneof([none,svg,x11])) The colorscheme for the
%   colors assigned to vertices and edges.  Supported values are
%   `svg`, `x11`, and `none` (for black and white).  Default is `svg`.
%
%   * alias_colors(+list(pair)) The default is [].

q_edges_to_export_graph(Es, ExportG, Opts1) :-
  % Options `colorscheme' and `namespace_colors' are special
  % since their values are reused by other options.
  select_option(colorscheme(Colorscheme), Opts1, Opts2, x11),
  (   select_option(alias_colors(Map), Opts2, Opts3)
  ->  true
  ;   create_namespace_map(Colorscheme, Map)
  ),
  merge_options(
    Opts3,
    [
      edge_arrowhead(q_edge_arrowhead),
      edge_color(q_edge_color(Colorscheme,Map)),
      edge_label(q_edge_label),
      edge_style(q_edge_style),
      graph_charset('UTF-8'),
      graph_colorscheme(Colorscheme),
      graph_directed(true),
      vertex_color(q_vertex_color(Colorscheme,Map)),
      vertex_image(q_vertex_image),
      vertex_label(q_vertex_label(Opts1)),
      vertex_peripheries(q_vertex_peripheries),
      vertex_shape(q_vertex_shape)
    ],
    Opts4
  ),
  l_edges_vertices(Es, Vs),
  build_export_graph(graph(Vs,Es), ExportG, Opts4).



%! q_graph_to_export_graph(+G, -ExportG) is det.
%! q_graph_to_export_graph(+G, -ExportG, +Opts) is det.

q_graph_to_export_graph(G, ExportG) :-
  q_graph_to_export_graph(G, ExportG, []).


q_graph_to_export_graph(G, ExportG, Opts) :-
  q_graph_edges(G, Es),
  q_edges_to_export_graph(Es, ExportG, Opts).



%! q_term_to_export_graph(+M, +Term, -ExportG, +Opts) is det.

q_term_to_export_graph(M, Term, ExportG, Opts) :-
  q_cbd_triples(M, Term, Triples),
  maplist(q_triple_edge, Triples, Es),
  q_edges_to_export_graph(Es, ExportG, Opts).





% ATTRIBUTE-SETTING PREDICATES %

%! q_edge_arrowhead(+E, -ArrowHead) is det.

% RDFS subclass.
q_edge_arrowhead(edge(_,P,_), box) :-
  rdf_equal(P, rdfs:subClassOf), !.
% RDFS subproperty.
q_edge_arrowhead(edge(_,P,_), diamond) :-
  rdf_equal(P, rdfs:subPropertyOf), !.
% RDF type.
q_edge_arrowhead(edge(_,P,_), empty) :-
  rdf_equal(P, rdf:type), !.
% RDFS label.
q_edge_arrowhead(edge(_,P,_), none) :-
  rdf_equal(P, rdfs:label), !.
% Others.
q_edge_arrowhead(_, normal).



%! q_edge_color(
%!   +Colorscheme:oneof([none,svg,x11]),
%!   +AliasColors:list(pair),
%!   +E,
%!   -Color
%! ) is det.

% Disable colorization.
q_edge_color(none, _, _, black) :- !.
% The edge color is based on the predicate term.
q_edge_color(Colorscheme, Map, edge(_,P,_), EColor) :-
  q_vertex_color(Colorscheme, Map, P, EColor), !.
% If the edge color is not specified, then see whether its vertices
% agree on their color.
q_edge_color(Colorscheme, Map, edge(FromV,_,ToV), EColor) :-
  q_vertex_color(Colorscheme, Map, FromV, FromVColor),
  q_vertex_color(Colorscheme, Map, ToV, ToVColor),
  FromVColor == ToVColor, !,
  EColor = FromVColor.
% Others.
q_edge_color(_, _, _, black).



%! q_edge_label(+E)// is det.

% User-supplied customization.
q_edge_label(edge(_,P,_)) --> rdf:q_predicate_label(P), !.
% Some edge labels are not displayed.
q_edge_label(edge(_,P,_)) -->
  {(  rdf_equal(rdf:type, P)
   ;  rdf_equal(rdfs:label, P)
   ;  rdf_equal(rdfs:subClassOf, P)
   ;  rdf_equal(rdfs:subPropertyOf, P)
   )}, !,
  "".
% Others: the edge name is the predicate term.
q_edge_label(edge(_,P,_)) --> dcg_q_print_term(P).



%! q_edge_style(+E, -Style) is det.

% User-supplied customization.
q_edge_style(E, EStyle) :-
  rdf:q_edge_style(E, EStyle), !.
% Certain RDFS schema terms.
q_edge_style(edge(_,P,_), solid) :-
  (   rdf_equal(rdf:type, P)
  ;   rdf_equal(rdfs:subClassOf, P)
  ;   rdf_equal(rdfs:subPropertyOf, P)
  ), !.
% RDFS label.
q_edge_style(edge(_,P,_), dotted) :-
  rdf_equal(rdfs:label, P), !.
% Others.
q_edge_style(_, solid).



%! q_vertex_color(
%!   +Colorscheme:oneof([none,svg,x11]),
%!   +AliasColors:list(pair),
%!   +Term,
%!   -Color
%! ) is det.
% Returns a color name for the given vertex.

% No color scheme.
q_vertex_color(none, _, _, black) :- !.
% Literal
q_vertex_color(_, _, T, blue) :-
  q_is_literal(T), !.
% Blank node
q_vertex_color(_, _, T, purple) :-
  q_is_bnode(T), !.
% Individual or subclass of a color-registered class.
q_vertex_color(_, _, V, VColor) :-
  (   rdfs_individual_of(V, C)
  ;   rdfs_subclass_of(V, C)
  ),
  rdf:q_class_color(C, VColor), !.
% IRI with a colored namespace.
q_vertex_color(_, Map, T, VColor) :-
  q_iri_alias(T, Alias),
  memberchk(Alias-VColor, Map), !.
% Other IRI.
q_vertex_color(_, _, _, black).



%! q_vertex_image(+M, +G, +V, -Img) is det.
%
% Only display the first picture that is found for Term.

q_vertex_image(M, G, V, Img) :-
  once(q_image(M, V, Img, G)).



%! q_vertex_label(+Opts, +Term, -Lbl) is det.

q_vertex_label(_Opts1, V) -->
  %{merge_options(Opts1, [literal_ellipsis(50)], Opts2)},
  dcg_q_print_term(V).



%! q_vertex_peripheries(+Term, -Peripheries:nonnneg) is det.

% Literal
q_vertex_peripheries(literal(_), 0) :- !.
% Blank node
q_vertex_peripheries(Term, 1) :-
  q_is_bnode(Term), !.
% Class
q_vertex_peripheries(Term, 2) :-
  rdfs_individual_of(Term, rdfs:'Class'), !.
% Property
q_vertex_peripheries(Term, 1) :-
  rdfs_individual_of(Term, rdf:'Property'), !.
% IRIs that are not classes or properties.
q_vertex_peripheries(_, 1).



%! q_vertex_shape(+Term, -Shape) is det.

% Literal
q_vertex_shape(literal(_), plaintext) :- !.
% Blank node
q_vertex_shape(T, circle) :-
  q_is_bnode(T), !.
% Class
q_vertex_shape(T, octagon) :-
  rdfs_individual_of(T, rdfs:'Class'), !.
% Property
q_vertex_shape(T, hexagon) :-
  rdfs_individual_of(T, rdf:'Property'), !.
% IRIs that are not classes or properties.
q_vertex_shape(_, ellipse).
