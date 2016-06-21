:- module(
  json2rdf,
  [
%    json2rdf_record/3, % +Source, +Alias, -Triples
    json2rdf_stmt/3    % +Source, +Alias, -Triple
  ]
).

/** <module> JSON-2-RDF

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(atom_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(json_ext)).
:- use_module(library(os/open_any2)).
:- use_module(library(readutil)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(yall)).





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
      rdf_create_bnode(S),
      get_dict_path(Keys1, D, Val0),
      ensure_atom(Val0, Val),
      atomic_list_concat(Keys1, '_', Local1),
      atomic_list_concat(Keys2, ' ', Local1),
      atomic_list_concat(Keys2, '_', Local2),
      rdf_global_id(Alias:Local2, P),
      rdf_global_term(rdf(S,P,Val^^xsd:string), Triple)
  ).
