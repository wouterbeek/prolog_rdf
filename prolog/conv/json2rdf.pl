:- module(
  json2rdf,
  [
    geojson2rdf_graph/4, % +M, +Source, +Alias, +Graph
    geojson2rdf_stmt/3,  % +Source, +Alias, -Triple
    json2rdf_graph/4,    % +M, +Source, +Alias, +Graph
    json2rdf_stmt/3,     % +Source, +Alias, -Triple
    ndjson2rdf_triple/3  % +Source, +Context, -Triple
  ]
).

/** <module> JSON-2-RDF

@author Wouter Beek
@see http://ndjson.org/
@version 2016/06
*/

:- use_module(library(atom_ext)).
:- use_module(library(debug)).
:- use_module(library(dict_ext)).
:- use_module(library(json_ext)).
:- use_module(library(jsonld/geold)).
:- use_module(library(jsonld/jsonld_read)).
:- use_module(library(os/open_any2)).
:- use_module(library(q/qb)).
:- use_module(library(q/q_term)).
:- use_module(library(readutil)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(yall)).

:- debug(json2rdf).




%! geojson2rdf_graph(+M, +File, +Alias, +G) is det.

geojson2rdf_graph(M, File, Alias, G) :-
  geojson2rdf_stmt(File, Alias, Triple),
  qb(M, Triple, G),
  fail.
geojson2rdf_graph(_, _, _, _) :-
  debug(geojsond2rdf, "GeoJSON → RDF done", []).



%! geojson2rdf_stmt(+File, +Alias, -Triple) is nondet.

geojson2rdf_stmt(File, Alias, Triple) :-
  geold_tuple(File, Alias, Triple).



%! json2rdf_graph(+M, +Source, +Alias, +G) is nondet.

json2rdf_graph(M, Source, Alias, G) :-
  json2rdf_stmt(Source, Alias, Triple),
  qb(M, Triple, G),
  fail.
json2rdf_graph(_, _, _, _) :-
  debug(jsond2rdf, "JSON → RDF done", []).



%! json2rdf_stmt(+Source, +Alias, -Triple) is nondet.

json2rdf_stmt(Source, Alias, Triple) :-
  call_on_stream(
    Source,
    {Alias,Triple}/[In,MIn,MIn]>>json2rdf_stmt0(In, Alias, Triple),
    [throw_when_empty(true)]
  ).

json2rdf_stmt0(In, Alias, Triple) :-
  repeat,
  read_line_to_string(In, Str),
  (   Str == end_of_file
  ->  !, fail
  ;   atom_string(A, Str),
      atom_json_dict(A, D),
      qb_bnode(S),
      get_dict_path(Keys1, D, Val),

      % P
      atomic_list_concat(Keys1, '_', Local1),
      atomic_list_concat(Keys2, ' ', Local1),
      atomic_list_concat(Keys2, '_', Local2),
      rdf_global_id(Alias:Local2, P),

      % O
      (is_list(Val) -> O = Val^^tcco:array ; O = Val^^xsd:string),
      
      rdf_global_term(rdf(S,P,O), Triple)
  ).



%! ndjson2rdf_triple(+Source, +Context, -Triple) is nondet.

ndjson2rdf_triple(Source, Context, Triple) :-
  call_on_stream(
    Source,
    {Context,Triple}/[In,MIn,MIn]>>ndjson2rdf0(In, Context, Triple)
  ).

ndjson2rdf0(In, Context, Triple) :-
  read_line_to_string(In, S),
  atom_json_dict(S, D),
  jsonld_tuple_with_context(Context, D, Triple).
