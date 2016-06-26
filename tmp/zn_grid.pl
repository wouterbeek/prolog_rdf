:- module(
  zh_grid,
  [
    zh_grid//1 % +G
  ]
).

/** <module> Z HTML grid

@author Wouter Beek
@version 2016/02-2016/03, 2016/06
*/

:- use_module(library(html/html_ext)).
:- use_module(library(html/html_grid)).
:- use_module(library(html/zh)).
:- use_module(library(rdf/rdf_grid)).
:- use_module(library(semweb/rdf11)).

:- rdf_meta
   zh_grid(r, ?, ?).





%! zh_grid(+G)// is det.
%
% Generates an HTML DOM grid whose widgets show aspects of the data in
% graph G.

zh_grid(G) -->
  {rdf_grid(G, Widgets)},
  html_grid(zh_grid:Widgets).


archive_entry(S, P, Pairs) -->
  html(
    div(about=S, [
      h1(\zh_property(P)),
      \html_maplist(po_pair0, Pairs)
    ])
  ).


%! http_header(+S, +P, +Values)// is det.
% HTTP resource S has HTTP header P set to value O.

http_header(S, P, L) -->
  html(
    div(about=S, [
      h1(\zh_property(P)),
      span(property=P, \html_maplist(http_header_value, L))
    ])
  ).


http_header_value(L) -->
  {is_list(L)}, !,
  list(zh_literal, L).
http_header_value(O) -->
  {rdf_is_literal(O)}, !,
  html(p(\zh_literal(O))).
http_header_value(media_type(Type,Subtype,Params)) --> !,
  html(
    p(class='media-type', [
      span(class=type, \zh_literal(Type)),
      "/",
      span(class=subtype, \zh_literal(Subtype)),
      "; ",
      span(class=parameters, \html_seplist(http_parameter, " ", Params))
    ])
  ).
http_header_value(product(Name,Version)) --> !,
  html(
    p(class=product, [
      span(class=name, \zh_literal(Name)),
      ": ",
      span(class=version, \zh_literal(Version))
    ])
  ).
http_header_value(uri(Scheme,Host,Path)) -->
  html(
    p(class=uri, [
      span(class=scheme, \zh_literal(Scheme)),
      "://",
      span(class=host, \zh_literal(Host)),
      "/",
      span(class=path, \zh_literal(Path))
    ])
  ).


http_parameter(param(Key,Value)) -->
  html([\zh_literal(Key),"=",\zh_literal(Value)]).


http_version(S, P, version(Major,Minor)) -->
  html(
    div(about=S, [
      h1(\zh_property(P)),
      p(class=version, [
        span(class=major, \zh_literal(Major)),
        ".",
        span(class=minor, \zh_literal(Minor))
      ])
    ])
  ).


po_pair0(P-O) -->
  html(
    p([
      \zh_property(P),
      ": ",
      span(property=P, \zh_object(O))
    ])
  ).


rdf_tuples(_, Quads, Triples, Duplicates) -->
  html([
    p(["Triples: ",\html_thousands(Triples)]),
    p(["Quads: ",\html_thousands(Quads)]),
    p(["Duplicates: ",\html_thousands(Duplicates)])
  ]).
