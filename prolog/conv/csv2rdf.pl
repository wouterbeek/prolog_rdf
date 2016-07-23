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

@author Wouter Beek
@version 2016/05-2016/07
*/

:- use_module(library(apply)).
:- use_module(library(csv)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_table)).
:- use_module(library(debug)).
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

:- qb_alias(ex, 'http://example.org/').





%! csv2rdf(+Source, +Sink) is det.
%! csv2rdf(+Source, +Sink, +Opts) is det.
%
% Converts the given CSV input file into RDF that is asserted either
% into the given output file or into the given RDF graph.
%
% The following options are supported:
%
%   * abox_alias(+atom) Default is `ex`.
%
%   * alias(+atom) Sets both abox_alias/1 and tbox_alias/1.
%
%   * class(+compound)
%
%   * header(+list) A list of RDF properties that represent the
%   columns.  This is used when there are no column labels in the CSV
%   file.
%
%   * tbox_alias(+atom) Uses the header labels as specified in the
%   first row of the CSV file.  The header labels will be turned into
%   RDF properties within the given namespace.  Default is `ex`.

csv2rdf(Source, Sink) :-
  csv2rdf(Source, Sink, []).


csv2rdf(Source, Sink, Opts) :-
  call_to_ntriples(Sink, csv2rdf_stream(Source, Opts), Opts).



%! csv2rdf_stream(+Source, +State, +Out) is det.
%! csv2rdf_stream(+Source, +Opts, +State, +Out) is det.

csv2rdf_stream(Source, State, Out) :-
  csv2rdf_stream(Source, [], State, Out).


csv2rdf_stream(Source, Opts, State, Out) :-
  call_on_stream(Source, csv2rdf_stream0(State, Out, Opts), Opts).


csv2rdf_stream0(State, Out, Opts1, In, Meta, Meta) :-
  csv2rdf_options0(Opts1, Dict, Opts2),
  (   get_dict(header, Dict, Ps)
  ->  true
  ;   once(csv:csv_read_stream_row(In, HeaderRow, _, Opts2)),
      list_row(HeaderNames, HeaderRow),
      Alias = Dict.tbox_alias,
      maplist(
        {Alias}/[HeaderName,P]>>q_iri_alias_local(P, Alias, HeaderName),
        HeaderNames,
        Ps
      )
  ),
  csv:csv_read_stream_row(In, DataRow, RowN, Opts2),
  list_row(Vals, DataRow),
  atom_number(Name, RowN),
  rdf_global_id(Dict.abox_alias:Name, S),
  (option(class(C), Opts1) -> gen_ntuple(S, rdf:type, C, State, Out) ; true),
  rdf_equal(xsd:string, D),
  maplist(
    {S,D,State,Out}/[P,Val]>>gen_ntuple(S, P, Val^^D, State, Out),
    Ps,
    Vals
  ),
  debug(conv(csv2rdf), "Converted CSV row ~D to RDF.", [RowN]),
  fail.
csv2rdf_stream0(_, _, _, _, Meta, Meta).


csv2rdf_options0(Opts1, D, Opts4) :-
  select_option(alias(Box), Opts1, Opts2), !,
  merge_options(Opts2, [abox_alias(Box),tbox_alias(Box)], Opts3),
  csv2rdf_options0(Opts3, D, Opts4).
csv2rdf_options0(Opts1, D2, Opts2) :-
  option(abox_alias(ABox), Opts1, ex),
  csv:make_csv_options(Opts1, Opts2, _),
  D1 = _{abox_alias: ABox},
  (   option(header(Ps), Opts1)
  ->  D2 = D1.put(_{header: Ps})
  ;   option(tbox_alias(TBox), Opts1)
  ->  D2 = D1.put(_{tbox_alias: TBox})
  ;   D2 = D1.put(_{tbox_alias: ex})
  ).
