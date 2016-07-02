:- module(
  json2rdf,
  [
    geojson2rdf/4,      % +M, +Source, +Alias, +G
    geojson2rdf_stmt/3, % +Source, +Alias, -Triple
    json2rdf/4,         % +M, +Source, +Alias, +G
    json2rdf_stmt/3,    % +Source, +Alias, -Triple
    ndjson2rdf_triple/3 % +Source, +Context, -Triple
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
:- use_module(library(gen/gen_ntuples)).
:- use_module(library(hdt/hdt__io)).
:- use_module(library(json_ext)).
:- use_module(library(jsonld/geold)).
:- use_module(library(jsonld/jsonld_read)).
:- use_module(library(os/io)).
:- use_module(library(q/qb)).
:- use_module(library(q/q_term)).
:- use_module(library(readutil)).
:- use_module(library(semweb/rdf11)).

:- debug(json2rdf).




%! geojson2rdf(+M, +Source, +Alias, +G) is det.

geojson2rdf(hdt, Source, Alias, G) :- !,
  hdt__call(geojson2rdf_stream0(Source, Alias), G).


geojson2rdf_stream0(Source, Alias) :-
  geojson2rdf_stmt(Source, Alias, Triple),
  gen_ntriple(Triple),
  fail.
geojson2rdf_stream0(_, _) :-
  debug(geojsond2rdf, "GeoJSON → RDF done", []).



%! geojson2rdf_stmt(+Source, +Alias, -Triple) is nondet.

geojson2rdf_stmt(Source, Alias, Triple) :-
  geold_tuple(Source, Alias, Triple).



%! json2rdf(+M, +Source, +Alias, +G) is nondet.

json2rdf(hdt, Source, Alias, G) :- !,
  hdt__call(json2rdf_stream0(Source, Alias), G).


json2rdf_stream0(Source, Alias, Out) :-
  json2rdf_stmt(Source, Alias, Triple),
  with_output_to(Out, gen_ntriple(Triple)),
  fail.
json2rdf_stream0(_, _, _) :-
  debug(jsond2rdf, "JSON → RDF done", []).



%! json2rdf_stmt(+Source, +Alias, -Triple) is nondet.

json2rdf_stmt(Source, Alias, Triple) :-
  call_on_stream(Source, json2rdf_stmt_stream0(Alias, Triple, In)).


json2rdf_stmt_stream0(Alias, Triple, In, Meta, Meta) :-
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
  call_on_stream(Source, ndjson2rdf_triple_stream0(Context, Triple)).


ndjson2rdf_triple_stream0(Context, Triple, In, Meta, Meta) :-
  read_line_to_string(In, Str),
  atom_json_dict(Str, Json),
  jsonld_tuple_with_context(Context, Json, Triple).
