:- module(
  json2rdf,
  [
    json2rdf/2,        % +Source, +Sink
    json2rdf/4,        % +Source, +Sink, +SourceOpts, +SinkOpts
    json2rdf_stream/3, % +Source, +State, +Out
    json2rdf_stream/4  % +Source, +Opts, +State, +Out
  ]
).

/** <module> JSON-2-RDF

@author Wouter Beek
@see http://ndjson.org/
@version 2016/06-2016/08, 2016/11
*/

:- use_module(library(atom_ext)).
:- use_module(library(conv/q_conv)).
:- use_module(library(debug_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(gen/gen_ntuples)).
:- use_module(library(json_ext)).
:- use_module(library(jsonld/jsonld_read)).
:- use_module(library(option)).
:- use_module(library(os/io)).
:- use_module(library(q/q_array), []). % tcco:array
:- use_module(library(q/q_iri)).
:- use_module(library(q/q_term)).
:- use_module(library(q/qb)).
:- use_module(library(readutil)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(uuid)).

:- multifile
    q_io:q_source2store_hook/5,
    q_io:q_source_format_hook/2.

q_io:q_source2store_hook(json, File1, File2, SourceOpts, SinkOpts) :-
  json2rdf(File1, File2, SourceOpts, SinkOpts).

q_io:q_source_format_hook(json, json).
q_io:q_source_format_hook(json, ndjson).





%! json2rdf(+Source, +Sink) is nondet.
%! json2rdf(+Source, +Sink, +SourceOpts, +SinkOpts) is nondet.
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
  json2rdf(Source, Sink, _{}, []).


json2rdf(Source, Sink, SourceOpts, SinkOpts) :-
  call_to_ntriples(Sink, json2rdf_stream(Source, SourceOpts), SinkOpts).



%! json2rdf_stream(+Source, +State, +Out) is nondet.
%! json2rdf_stream(+Source, +Opts, +State, +Out) is nondet.

json2rdf_stream(Source, State, Out) :-
  json2rdf_stream(Source, _{}, State, Out).


json2rdf_stream(Source, Opts1, State, Out) :-
  indent_debug(conv(json2rdf), "> JSON → RDF"),
  dict_options(Opts1, Opts2),
  call_on_stream(Source, json2rdf_stream0(State, Out, Opts1), Opts2),
  indent_debug(conv(json2rdf), "< JSON → RDF").


json2rdf_stream0(State, Out, Opts1, In, Meta, Meta) :-
  q_conv_options(Opts1, Opts2),
  repeat,
  read_line_to_string(In, Str),
  (   Str == end_of_file
  ->  !
  ;   string_json_dict(Str, Dict),
      dict_to_triple(Dict, Opts2, Triple),
      gen_ntuple(Triple, State, Out),
      fail
  ).


%! dict_to_triple(+In, +Opts, -Triple) is nondet.

dict_to_triple(Dict, Opts, Triple) :-
  % S
  q_abox_iri(Opts.scheme, Opts.host, Opts.concept, S),
  % P
  get_dict_path(Keys1, Dict, Val), % NONDET
  atomic_list_concat(Keys1, '_', Local1),
  atomic_list_concat(Keys2, ' ', Local1),
  atomic_list_concat(Keys2, '_', Local2),
  q_tbox_iri(Opts.scheme, Opts.host, Local2, P),
  % O
  get_dict_path(Keys1, Dict, Val),
  (is_list(Val) -> O = Val^^tcco:array ; O = Val^^xsd:string),
  % 〈S,P,O〉
  rdf_global_term(rdf(S,P,O), Triple0),
  % @bug This crashes the GUI debugger otherwise.
  Triple = Triple0.
