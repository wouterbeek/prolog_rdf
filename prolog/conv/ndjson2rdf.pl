:- module(
  ndjson2rdf,
  [
    ndjson2rdf_triple/3 % +Source, +Context, -Triple
  ]
).

/** <module> NDJSON-2-RDF

@author Wouter Beek
@see http://ndjson.org/
@version 2016/06
*/

:- use_module(library(json_ext)).
:- use_module(library(jsonld/jsonld_read)).
:- use_module(library(os/open_any2)).
:- use_module(library(readutil)).
:- use_module(library(yall)).





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
