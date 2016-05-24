:- module(
  csv2rdf,
  [
    csv2rdf_file/2,  % +File1, +File2
    csv2rdf_file/3,  % +File1, +File2, +Opts
    csv2rdf_graph/2, % +File,  +G
    csv2rdf_graph/3  % +File,  +G,     +Opts
  ]
).

/** <module> CSV to RDF

Automatic conversion from CSV to RDF.

@author Wouter Beek
@version 2016/05
*/

:- use_module(library(apply)).
:- use_module(library(csv)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_table)).
:- use_module(library(debug)).
:- use_module(library(list_ext)).
:- use_module(library(option)).
:- use_module(library(os/file_ext)).
:- use_module(library(os/open_any2)).
:- use_module(library(pure_input)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_store)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(yall)).

:- rdf_register_prefix(ex, 'http://example.org/').





%! csv2rdf_file(+File1, +File2) is det.
%! csv2rdf_file(+File1, +File2, +Opts) is det.
%
% The following options are supported:
%   * abox_alias(+atom)
%     Default is `ex'.
%   * header(+list)
%     A list of RDF properties that represent the columns.
%     This is used when there are no column labels in the CSV file.
%   * tbox_alias(+atom)
%     Uses the header labels as specified in the first row of the CSV file.
%     The header labels will be turned into RDF properties within the given
%     namespace.
%     Default is `ex`.

csv2rdf_file(File1, File2) :-
  csv2rdf_file(File1, File2, []).


csv2rdf_file(File1, File2, Opts1) :-
  csv2rdf_options(Opts1, D, Opts2),
  call_onto_stream(
    File1,
    File2,
    {D,Opts2}/[In,MIn,MIn,Out,MOut,MOut]>>csv2rdf_1(In, stream(Out), D, Opts2),
    Opts1
  ).


csv2rdf_graph(File, G) :-
  csv2rdf_graph(File, G, []).


csv2rdf_graph(File, G, Opts1) :-
  csv2rdf_options(Opts1, D, Opts2),
  call_on_stream(
    File,
    {D,Opts2}/[In,M,M]>>csv2rdf_1(In, graph(G), D, Opts2),
    Opts1
  ).


csv2rdf_options(Opts1, D2, Opts2) :-
  option(abox_alias(ABox), Opts1, ex),
  csv:make_csv_options(Opts1, Opts2, _),
  D1 = _{abox_alias: ABox},
  (   option(header(Ps), Opts1)
  ->  D2 = D1.put(_{header: Ps})
  ;   option(tbox_alias(TBox), Opts1)
  ->  D2 = D1.put(_{tbox_alias: TBox})
  ;   D2 = D1.put(_{tbox_alias: ex})
  ).


csv2rdf_1(In, Sink, D, Opts) :-
  get_dict(header, D, Ps), !,
  csv2rdf_2(In, Sink, Ps, D, Opts).
csv2rdf_1(In, Sink, D, Opts) :-
  once(csv:csv_read_stream_row(In, Row, _, Opts)),
  list_row(Locals, Row),
  maplist(local_iri0(D.tbox_alias), Locals, Ps),
  csv2rdf_2(In, Sink, Ps, D, Opts).


csv2rdf_2(In, Sink, Ps, D, Opts) :-
  csv:csv_read_stream_row(In, Row, I, Opts),
  list_row(Atoms, Row),
  maplist(assert0(Sink, I, D), Ps, Atoms),
  fail.
csv2rdf_2(_, _, _, _, _).


local_iri0(Alias, Local, Iri) :-
  rdf_global_id(Alias:Local, Iri).


assert0(Sink, I1, D, P, Atom) :-
  atom_number(I2, I1),
  rdf_global_id(D.abox_alias:I2, S),
  (   Sink = stream(Out)
  ->  rdf_store(Out, S, P, Atom^^xsd:string)
  ;   Sink = graph(G)
  ->  rdf_assert(S, P, Atom^^xsd:string, G)
  ).
