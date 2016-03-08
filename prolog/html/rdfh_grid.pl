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


%! http_header(+S, +P, +O)// is det.
% HTTP resource S has HTTP header P set to value O.

http_header(S, P, O) -->
  html(div(about=S, [h1(\rdfh_predicate(P)), p(\http_header_value(O))])).

http_header_value(L) -->
  {is_list(L)}, !,
  list(rdfh_literal, L).
http_header_value(O) -->
  {rdf_is_literal(O)}, !,
  rdfh_literal(O).
http_header_value(media_type(Type,Subtype,Params)) --> !,
  html([
    \rdfh_literal(Type),
    "/",
    \rdfh_literal(Subtype),
    "; ",
    \seplist(http_parameter, Params)
  ]).
http_header_value(product(Name,Version)) -->
  html([\rdfh_literal(Name),": ",\rdfh_literal(Version)]).

http_parameter(param(Key,Value)) -->
  html([\rdfh_literal(Key),"=",\rdfh_literal(Value)]).
