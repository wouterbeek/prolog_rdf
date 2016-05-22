:- module(
  rdf_csv,
  [
    csv_to_rdf/2 % +Source, +Opts
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
:- use_module(library(os/open_any2)).
:- use_module(library(pure_input)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(yall)).

:- rdf_register_prefix(ex, 'http://example.org/').





%! csv_to_rdf(+Source, +Opts) is det.
%
% The following options are supported:
%   - abox_alias(+atom)
%     Default is `ex'.
%   - header(+list)
%     A list of RDF properties that represent the columns.
%     This is used when there are no column labels in the CSV file.
%   - tbox_alias(+atom)
%     Uses the header labels as specified in the first row of the CSV file.
%     The header labels will be turned into RDF properties within the given
%     namespace.

csv_to_rdf(Source, Opts1) :-
  option(abox_alias(ABox), Opts1, ex),
  rdf_default_graph(DefG),
  option(graph(G), Opts1, DefG),
  csv:make_csv_options(Opts1, Opts2, _),
  D1 = _{abox_alias: ABox, graph: G},
  (   option(header(Ps), Opts1)
  ->  D2 = D1.put(_{header: Ps})
  ;   option(tbox_alias(TBox), Opts1)
  ->  D2 = D1.put(_{tbox_alias: TBox})
  ;   D2 = D1.put(_{tbox_alias: ex})
  ),
  call_on_stream(
    Source,
    {D2,Opts2}/[In,M,M]>>csv_stream_to_rdf1(In, D2, Opts2)
  ).

csv_stream_to_rdf1(In, D, Opts) :-
  get_dict(header, D, Ps), !,
  csv_stream_to_rdf2(In, Ps, D, Opts).
csv_stream_to_rdf1(In, D, Opts) :-
  once(csv:csv_read_stream_row(In, Row, _, Opts)),
  list_row(Locals, Row),
  maplist(local_iri0(D.tbox_alias), Locals, Ps),
  csv_stream_to_rdf2(In, Ps, D, Opts).

csv_stream_to_rdf2(In, Ps, D, Opts) :-
  csv:csv_read_stream_row(In, Row, I, Opts),
  list_row(Atoms, Row),
  maplist(assert0(I, D), Ps, Atoms),
  fail.
csv_stream_to_rdf2(_, _, _, _).

local_iri0(Alias, Local, Iri) :-
  rdf_global_id(Alias:Local, Iri).

assert0(I1, D, P, Atom) :-
  atom_number(I2, I1),
  rdf_global_id(D.abox_alias:I2, S),
  rdf_assert(S, P, Atom^^xsd:string, D.graph).
