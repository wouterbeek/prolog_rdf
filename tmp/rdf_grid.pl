:- module(
  rdf_grid,
  [
    rdf_grid/2 % +G, -Widgets
  ]
).

/** <module> RDF grid

Build grid compound terms based on RDF data.

@author Wouter Beek
@version 2016/03
*/

:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(html/zh)).
:- use_module(library(rdf/rdf_graph)).
:- use_module(library(trp/trp_update)).
:- use_module(library(uri/uri_ext)).

:- rdf_meta
   pop_triple(r, r, o, r),
   pop_triples(r, r, o, r, -).





%! rdf_grid(+G, -Widgets) is det.

rdf_grid(G, Widgets) :-
  setup_call_cleanup(
    (
      uri_segments_uuid(TmpG, [graph]),
      rdf_cp_graph(G, TmpG)
    ),
    graph_to_widgets(TmpG, Widgets),
    rdf_unload_graph(TmpG)
  ).

graph_to_widgets(G, [H|T]) :-
  graph_to_widget(G, H), !,
  graph_to_widgets(G, T).
graph_to_widgets(_, []).

% Archive entry.
graph_to_widget(G, archive_entry(S, P, Pairs)) :-
  rdf(O, rdf:type, llo:'ArchiveEntry', G),
  pop_triple(S, P, O, G), !,
  pop_triples(O, _, _, G, po(Pairs)),
  rdf_retractall(O, rdf:type, llo:'ArchiveEntry', G).
% HTTP header.
graph_to_widget(G, http_header(S, P, L)) :-
  rdf(O, rdf:type, llo:'ValidHttpHeader', G), !,
  pop_triple(S, P, O, G),
  findall(
    V2,
    (
      rdf(O, llo:value, V1, G),
      http_header_value(V1, V2, G)
    ),
    L
  ),
  rdf_retractall(O, llo:value, _, G),
  rdf_retractall(O, rdf:type, llo:'ValidHttpHeader', G),
  rdf_retractall(O, llo:raw, _, G).
% HTTP version.
graph_to_widget(G, http_version(S, P, version(Major,Minor))) :-
  rdf(O, rdf:type, llo:'Version', G),
  pop_triple(S, P, O, G), !,
  pop_triple(O, llo:major, Major, G),
  pop_triple(O, llo:minor, Minor, G),
  rdf_retractall(O, rdf:type, llo:'Version', G).
% RDF tuples.
graph_to_widget(G, rdf_tuples(S,Quads,Triples,Duplicates)) :-
  pop_triple(S, llo:duplicate_tuples, Duplicates^^xsd:nonNegativeInteger, G), !,
  pop_triple(S, llo:processed_quads, Quads^^xsd:nonNegativeInteger, G),
  pop_triple(S, llo:processed_triples, Triples^^xsd:nonNegativeInteger, G),
  pop_triple(S, llo:processed_tuples, _, G),
  pop_triple(S, llo:unique_tuples, _^^xsd:nonNegativeInteger, G).
% Triple.
graph_to_widget(G, zh_triple(S, P, O)) :-
  pop_triple(S, P, O, G).



%! http_header_value(+Res, -Pl) is det.
% Prolog compound term representation Pl for RDF resource Res.

% RDF literal.
http_header_value(V, V, _) :-
  rdf_is_literal(V), !.
% List of RDF literals.
http_header_value(S, L, G) :-
  pop_list(S, L, G).
% HTTP cache directive.
http_header_value(S, O, G) :-
  pop_triple(S, rdf:type, llo:'CacheDirective', G), !,
  pop_triple(S, llo:key, O, G).
% Internet Media Type.
http_header_value(S, media(Type/Subtype,[Param]), G) :-
  pop_triple(S, rdf:type, llo:'MediaType', G), !,
  pop_triple(S, llo:parameters, O, G),
  http_parameter(O, Param, G),
  pop_triple(S, llo:subtype, Subtype, G),
  pop_triple(S, llo:type, Type, G).
% HTTP product.
http_header_value(S, product(Name,Version), G) :-
  pop_triple(S, rdf:type, llo:'Product', G), !,
  pop_triple(S, llo:name, Name, G),
  pop_triple(S, llo:version, Version, G).
% URI.
http_header_value(S, uri(Scheme,Host,Path), G) :-
  pop_triple(S, rdf:type, llo:'URI', G), !,
  pop_triple(S, llo:scheme, Scheme, G),
  pop_triple(S, llo:'hier-part', Hier0, G),
  pop_triple(Hier0, rdf:type, llo:'hier-part', G),
  pop_triple(Hier0, llo:authority, Auth0, G),
  pop_triple(Hier0, llo:'path-abempty', Path, G),
  pop_triple(Auth0, rdf:type, llo:authority, G),
  pop_triple(Auth0, llo:host, Host0, G),
  pop_triple(Host0, rdf:type, llo:host, G),
  pop_triple(Host0, llo:'reg-name', Host, G).


% http_parameter(+Res, -Param, +G) .

http_parameter(S, param(Key,Value), G) :-
  pop_triple(S, rdf:type, llo:'Parameter', G),
  pop_triple(S, llo:key, Key, G),
  pop_triple(S, llo:value, Value, G).





% HELPERS %

%! pop_list(+S, -L, +G) is det.

pop_list(S, [], _) :-
  rdf_equal(rdf:nil, S), !.
pop_list(S, [H|T], G) :-
  pop_triple(S, rdf:first, H, G),
  pop_triple(S, rdf:rest, O, G),
  pop_list(O, T, G).



%! pop_triple(+S, +P, +O, +G) is det.
%
% Consume the first triple instantiation of 〈S,P,O〉.

pop_triple(S, P, O, G) :-
  rdf_chk(S, P, O, G),
  rdf_retractall(S, P, O, G),
  rdf_statistics(triples_by_graph(G,N)),
  debug(rdf(grid), "~D triples left.", [N]).



%! pop_triples(?S, ?P, ?G, -Return) is det.
% Consume all triple instantiations of 〈S,P,O〉.
%
% Return is either of the following:
%   - o(-list)
%     The list of objects.
%   - po(-list)
%     The list of predicate/object pairs

pop_triples(S, P, O, G, Return) :-
  findall(rdf(S,P,O,G), rdf(S, P, O, G), Quads),
  Return =.. [Mode,L],
  maplist(quad_return(Mode), Quads, L),
  maplist(rdf_retractall, Quads).

quad_return(s,  rdf(S,_,_,_), S).
quad_return(p,  rdf(_,P,_,_), P).
quad_return(po, rdf(_,P,O,_), P-O).
quad_return(o,  rdf(_,_,O,_), O).
