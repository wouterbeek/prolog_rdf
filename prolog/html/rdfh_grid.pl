:- module(
  rdfh_grid,
  [
    rdfh_grid//1 % +G
  ]
).

/** <module> RDF HTML grid

@author Wouter Beek
@version 2016/02-2016/03
*/

:- use_module(library(html/html_ext)).
:- use_module(library(html/html_grid)).
:- use_module(library(html/rdfh)).
:- use_module(library(rdf/rdf_grid)).
:- use_module(library(semweb/rdf11)).

:- rdf_meta
   rdfh_grid(r, ?, ?).





%! rdfh_grid(+G)// is det.
% Generates an HTML DOM grid whose widgets show aspects of the data in G.

rdfh_grid(G) -->
  {rdf_grid(G, Widgets)},
  html_grid(rdfh_grid:Widgets).


archive_entry(S, P, Pairs) -->
  html(
    div(about=S, [
      h1(\rdfh_property(P)),
      \html_maplist(po_pair0, Pairs)
    ])
  ).

%! http_header(+S, +P, +Values)// is det.
% HTTP resource S has HTTP header P set to value O.

http_header(S, P, L) -->
  html(
    div(about=S, [
      h1(\rdfh_property(P)),
      span(property=P, \html_maplist(http_header_value, L))
    ])
  ).

http_header_value(L) -->
  {is_list(L)}, !,
  list(rdfh_literal, L).
http_header_value(O) -->
  {rdf_is_literal(O)}, !,
  html(p(\rdfh_literal(O))).
http_header_value(media_type(Type,Subtype,Params)) --> !,
  html(
    p(class='media-type', [
      span(class=type, \rdfh_literal(Type)),
      "/",
      span(class=subtype, \rdfh_literal(Subtype)),
      "; ",
      span(class=parameters, \html_seplist(http_parameter, Params))
    ])
  ).
http_header_value(product(Name,Version)) --> !,
  html(
    p(class=product, [
      span(class=name, \rdfh_literal(Name)),
      ": ",
      span(class=version, \rdfh_literal(Version))
    ])
  ).
http_header_value(uri(Scheme,Host,Path)) -->
  html(
    p(class=uri, [
      span(class=scheme, \rdfh_literal(Scheme)),
      "://",
      span(class=host, \rdfh_literal(Host)),
      "/",
      span(class=path, \rdfh_literal(Path))
    ])
  ).

http_parameter(param(Key,Value)) -->
  html([\rdfh_literal(Key),"=",\rdfh_literal(Value)]).

http_version(S, P, version(Major,Minor)) -->
  html(
    div(about=S, [
      h1(\rdfh_property(P)),
      p(class=version, [
        span(class=major, \rdfh_literal(Major)),
        ".",
        span(class=minor, \rdfh_literal(Minor))
      ])
    ])
  ).

po_pair0(P-O) -->
  html(
    p([
      \rdfh_property(P),
      ": ",
      span(property=P, \rdfh_object(O))
    ])
  ).

rdf_tuples(_, Quads, Triples, Duplicates) -->
  html([
    p(["Triples: ",\html_thousands(Triples)]),
    p(["Quads: ",\html_thousands(Quads)]),
    p(["Duplicates: ",\html_thousands(Duplicates)])
  ]).
