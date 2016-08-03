:- module(
  csv2rdf,
  [
    csv2rdf/2,        % +Source, +Sink
    csv2rdf/3,        % +Source, +Sink, +Opts
    csv2rdf_stream/3, % +Source,               +State, +Out
    csv2rdf_stream/4  % +Source,        +Opts, +State, +Out
  ]
).

/** <module> CSV to RDF

Automatic conversion from CSV to RDF.

The following debug flags are used:

  * conv(csv2rdf)

@author Wouter Beek
@version 2016/05-2016/08
*/

:- use_module(library(apply)).
:- use_module(library(csv)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_table)).
:- use_module(library(debug_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(gen/gen_ntuples)).
:- use_module(library(list_ext)).
:- use_module(library(option)).
:- use_module(library(os/file_ext)).
:- use_module(library(os/io)).
:- use_module(library(pure_input)).
:- use_module(library(q/qb)).
:- use_module(library(q/q_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(yall)).





%! csv2rdf(+Source, +Sink) is det.
%! csv2rdf(+Source, +Sink, +Opts) is det.
%
% Converts the given CSV input file into RDF that is asserted either
% into the given output file or into the given RDF graph.
%
% The following options are supported:
%
%   * concept(+atom)
%
%   * domain(+atom)
%
%   * header(+list) A list of RDF properties that represent the
%     columns.  This is used when there are no column labels in the
%     CSV file.

csv2rdf(Source, Sink) :-
  csv2rdf(Source, Sink, _{}).


csv2rdf(Source, Sink, Opts) :-
  call_to_ntriples(Sink, csv2rdf_stream(Source, Opts), Opts).



%! csv2rdf_stream(+Source, +State, +Out) is det.
%! csv2rdf_stream(+Source, +Opts, +State, +Out) is det.

csv2rdf_stream(Source, State, Out) :-
  csv2rdf_stream(Source, _{}, State, Out).


csv2rdf_stream(Source, Opts, State, Out) :-
  indent_debug(conv(csv2rdf), "> CSV → RDF"),
  call_on_stream(Source, csv2rdf_stream0(State, Out, Opts), Opts),
  indent_debug(conv(csv2rdf), "< CSV → RDF").


csv2rdf_stream0(State, Out, Opts1, In, Meta, Meta) :-
  default_csv2rdf_options(Opts1, Opts2),
  csv:make_csv_options([], CsvOpts, _),
  (   get_dict(header, Opts2, Ps)
  ->  true
  ;   once(csv:csv_read_stream_row(In, HeaderRow, _, CsvOpts)),
      list_row(HeaderNames, HeaderRow),
      maplist(qb_tbox_iri(Opts2.domain), HeaderNames, Ps)
  ),
  csv:csv_read_stream_row(In, DataRow, RowN, CsvOpts),
  debug(conv(csv2rdf), "~D", [RowN]),
  list_row(Vals, DataRow),
  first(Vals, Reference),
  qb_abox_iri(Opts2.domain, Opts2.concept, Reference, S),
  rdf_equal(xsd:string, D),
  maplist(
    {S,D,State,Out}/[P,Val]>>gen_ntuple(S, P, Val^^D, State, Out),
    Ps,
    Vals
  ),
  fail.
csv2rdf_stream0(_, _, _, _, Meta, Meta).





% HELPERS %

default_csv2rdf_options(Opts1, Opts2) :-
  q_alias_prefix(default, Domain),
  merge_dicts(_{concept: resource, domain: Domain}, Opts1, Opts2).
