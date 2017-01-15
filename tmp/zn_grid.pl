:- module(
  qh_grid,
  [
    qh_grid//1 % +G
  ]
).

/** <module> Z HTML grid

@author Wouter Beek
@version 2016/02-2016/03, 2016/06
*/

:- use_module(library(html/rdf_html)).
:- use_module(library(rdf/rdf_grid)).

:- rdf_meta
   qh_grid(r, ?, ?).





%! qh_grid(+G)// is det.
%
% Generates an HTML DOM grid whose widgets show aspects of the data in
% graph G.

qh_grid(G) -->
  {rdf_grid(G, Widgets)},
  grid(qh_grid:Widgets).


archive_entry(S, P, Pairs) -->
  html(
    div(about=S, [
      h1(\qh_property(P)),
      \html_maplist(po_pair0, Pairs)
    ])
  ).


%! http_header(+S, +P, +Values)// is det.
% HTTP resource S has HTTP header P set to value O.

http_header(S, P, L) -->
  html(
    div(about=S, [
      h1(\qh_property(P)),
      span(property=P, \html_maplist(http_header_value, L))
    ])
  ).


http_header_value(L) -->
  {is_list(L)}, !,
  list(qh_literal, L).
http_header_value(O) -->
  {rdf_is_literal(O)}, !,
  html(p(\qh_literal(O))).
http_header_value(media(Type/Subtype,Params)) --> !,
  html(
    p(class='media-type', [
      span(class=type, \qh_literal(Type)),
      "/",
      span(class=subtype, \qh_literal(Subtype)),
      "; ",
      span(class=parameters, \html_seplist(http_parameter, " ", Params))
    ])
  ).
http_header_value(product(Name,Version)) --> !,
  html(
    p(class=product, [
      span(class=name, \qh_literal(Name)),
      ": ",
      span(class=version, \qh_literal(Version))
    ])
  ).
http_header_value(uri(Scheme,Host,Path)) -->
  html(
    p(class=uri, [
      span(class=scheme, \qh_literal(Scheme)),
      "://",
      span(class=host, \qh_literal(Host)),
      "/",
      span(class=path, \qh_literal(Path))
    ])
  ).


http_parameter(param(Key,Value)) -->
  html([\qh_literal(Key),"=",\qh_literal(Value)]).


http_version(S, P, version(Major,Minor)) -->
  html(
    div(about=S, [
      h1(\qh_property(P)),
      p(class=version, [
        span(class=major, \qh_literal(Major)),
        ".",
        span(class=minor, \qh_literal(Minor))
      ])
    ])
  ).


po_pair0(P-O) -->
  html(
    p([
      \qh_property(P),
      ": ",
      span(property=P, \qh_object(O))
    ])
  ).


rdf_tuples(_, Quads, Triples, Duplicates) -->
  html([
    p(["Triples: ",\thousands(Triples)]),
    p(["Quads: ",\thousands(Quads)]),
    p(["Duplicates: ",\thousands(Duplicates)])
  ]).
