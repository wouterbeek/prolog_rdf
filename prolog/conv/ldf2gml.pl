:- module(
  ldf2gml,
  [
    ldf2gml/4, % ?S, ?P, ?O, +Endpoint
    ldf2gml/5  % ?S, ?P, ?O, +Endpoint, +Opts
  ]
).

/** <module> LDF-2-GML

@author Wouter Beek
@version 2016/07
*/

:- use_module(library(conv/rdf2gml)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(service/ldf)).

:- rdf_meta
   ldf2gml(r, r, o, +),
   ldf2gml(r, r, o, r, +).





%! ldf2gml(?S, ?P, ?O, +Endpoints) is det.
%! ldf2gml(?S, ?P, ?O, +Endpoints, +Opts) is det.
%
% Options are passed to rdf2gml/3.

ldf2gml(S, P, O, Endpoint) :-
  ldf2gml(S, P, O, Endpoint, []).


ldf2gml(S, P, O, Endpoint, SinkOpts) :-
  rdf2gml_start(SinkOpts, NFile, EFile, GFile, ExportOpts),
  call_to_streams(NFile, EFile, ldf2gml0(S, P, O, ExportOpts), SinkOpts),
  rdf2gml_end(NFile, EFile, GFile, SinkOpts).


ldf2gml0(S, P, O, ExportOpts, NOut, EOut) :-
  forall(
    ldf(S, P, O, Endpoint),
    rdf2gml_triple(NOut, EOut, S, P, O, ExportOpts)
  ).
