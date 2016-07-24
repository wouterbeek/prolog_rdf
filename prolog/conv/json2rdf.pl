:- module(
  json2rdf,
  [
    json2rdf/2,        % +Source, +Sink
    json2rdf/3,        % +Source, +Sink, +Opts
    json2rdf_stream/3, % +Source,        +State, +Out
    json2rdf_stream/4  % +Source, +Opts, +State, +Out
  ]
).

/** <module> JSON-2-RDF

@author Wouter Beek
@see http://ndjson.org/
@version 2016/06-2016/07
*/

:- use_module(library(atom_ext)).
:- use_module(library(debug_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(gen/gen_ntuples)).
:- use_module(library(json_ext)).
:- use_module(library(jsonld/jsonld_read)).
:- use_module(library(os/io)).
:- use_module(library(q/q_term)).
:- use_module(library(q/qb)).
:- use_module(library(readutil)).
:- use_module(library(semweb/rdf11)).





%! json2rdf(+Source, +Sink) is nondet.
%! json2rdf(+Source, +Sink, +Opts) is nondet.
%
% Convert JSON coming from Source into RDF that is stored in graph G
% using backend M.
%
% Options are passed to:
%
%   * call_on_stream/3
%
%   * call_to_ntriples/3

json2rdf(Source, Sink) :-
  json2rdf(Source, Sink, []).


json2rdf(Source, Sink, Opts) :-
  call_to_ntriples(Sink, json2rdf_stream(Source, Opts), Opts).



%! json2rdf_stream(+Source, +State, +Out) is nondet.
%! json2rdf_stream(+Source, +Opts, +State, +Out) is nondet.

json2rdf_stream(Source, State, Out) :-
  json2rdf_stream(Source, [], State, Out).


json2rdf_stream(Source, Opts1, State, Out) :-
  list_alias_options(Opts1, Opts2),
  indent_debug(conv(json2rdf), "> JSON → RDF"),
  call_on_stream(Source, json2rdf_stream0(State, Out, Opts2), Opts1),
  indent_debug(conv(json2rdf), "< JSON → RDF").


json2rdf_stream0(State, Out, Opts, In, Meta, Meta) :-
  repeat,
  read_line_to_string(In, Str),
  (   Str == end_of_file
  ->  !
  ;   string_json_dict(Str, Dict),
      dict_to_triple(Dict, Opts.tbox_alias, Triple),
      gen_ntuple(Triple, State, Out),
      fail
  ).





% HELPERS %

%! list_alias_options(+Opts1, -Opts2) is det.

list_alias_options(Opts1, Opts4) :-
  merge_options(Opts1, [alias(ex)], Opts2),
  select_option(alias(Alias), Opts2, Opts3),
  merge_options(Opts3, [abox_alias(Alias),tbox_alias(Alias)], Opts4).



%! dict_to_triple(+In, +Alias, -Triple) is nondet.

dict_to_triple(Dict, Alias, Triple) :-
  % S
  qb_iri(Alias, S),
  % P
  get_dict_path(Keys1, Dict, Val), % NONDET
  atomic_list_concat(Keys1, '_', Local1),
  atomic_list_concat(Keys2, ' ', Local1),
  atomic_list_concat(Keys2, '_', Local2),
  rdf_global_id(Alias:Local2, P),
  % O
  get_dict_path(Keys1, Dict, Val),
  (is_list(Val) -> O = Val^^tcco:array ; O = Val^^xsd:string),
  % 〈S,P,O〉
  rdf_global_term(rdf(S,P,O), Triple).
