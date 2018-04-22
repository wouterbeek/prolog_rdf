:- module(
  rdf_http,
  [
    rdf_http_graph_query/2 % ?G, -Query
  ]
).

/** <module> HTTP support for RDF

@author Wouter Beek
@version 2018
*/

:- use_module(library(error)).

:- use_module(library(sw/rdf_term)).

:- multifile
    http:convert_parameter/3,
    http:error_status_message/3.

http:convert_parameter(rdf_term, Atom, G) :-
  rdf_atom_to_term(Atom, G).

http:error_status_message(rdf(cannot_parse(rdf_term,Atom)), 400, Msg) :-
  format(
    string(Msg),
    "😿 Your request is incorrect!  You have specified the value ‘~a’, but this cannot be parsed as an RDF term.",
    [Atom]
  ).





%! rdf_http_graph_query(+G:rdf_graph, -Query:list(compound)) is det.

rdf_http_graph_query(G, []) :-
  var(G), !.
rdf_http_graph_query(G, []) :-
  rdf_default_graph(G), !.
rdf_http_graph_query(G, [g(GAtom)]) :-
  rdf_term_to_atom(G, GAtom).
