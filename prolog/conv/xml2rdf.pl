:- module(
  xml2rdf,
  [
    xml2rdf/3,        % +Source, +Sink, +RecordNames
    xml2rdf/5,        % +Source, +Sink, +RecordNames, +SourceOpts, +SinkOpts
    xml2rdf_stream/4, % +Source, +RecordNames, +State, +Out
    xml2rdf_stream/5  % +Source, +RecordNames, +Opts, +State, +Out
  ]
).

/** <module> XML-2-RDF

@author Wouter Beek
@tbd Currently runs out of memory for unclear reasons.
@version 2016/06-2016/08, 2016/11
*/

:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(conv/q_conv)).
:- use_module(library(debug)).
:- use_module(library(dict_ext)).
:- use_module(library(gen/gen_ntuples)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(q/q_iri)).
:- use_module(library(q/q_print)).
:- use_module(library(q/q_term)).
:- use_module(library(q/qb)).
:- use_module(library(xml/marcxml)).
:- use_module(library(xml/xml_stream)).
:- use_module(library(yall)).

:- multifile
    q_io:q_source2store_hook/5,
    q_io:q_source_format_hook/2.

q_io:q_source2store_hook(xml, Source, Sink, SourceOpts1, SinkOpts) :-
  del_dict(record_names, SourceOpts1, RecordNames, SourceOpts2),
  xml2rdf(Source, Sink, RecordNames, SourceOpts2, SinkOpts).

q_io:q_source_format_hook(xml, [marcxml]).
q_io:q_source_format_hook(xml, [xml]).





%! xml2rdf(+Source, +Sink, +RecordNames) is nondet.
%! xml2rdf(+Source, +Sink, +RecordNames, +SourceOpts, +SinkOpts) is nondet.
%
% The following options are supported:
%
%   * concept(+atom)
%
%     The local name of the IRI that is used for the subject term.
%
%   * entry_name(+atom)
%
%     The name of the entry inside the source archive, if any.
%
%   * host(+atom)
%
%     The host that is used for IRI terms.
%
%   * scheme(+atom)
%
%     The scheme that is used for IRI terms.

xml2rdf(Source, Sink, RecordNames) :-
  xml2rdf(Source, Sink, RecordNames, _{}, []).


xml2rdf(Source, Sink, RecordNames, SourceOpts, SinkOpts1) :-
  (   get_dict(entry_name, SourceOpts, Entry)
  ->  merge_options(SinkOpts1, [entry_name(Entry)], SinkOpts2)
  ;   SinkOpts2 = SinkOpts1
  ),
  call_to_ntriples(
    Sink,
    xml2rdf_stream(Source, RecordNames, SourceOpts),
    SinkOpts2
  ).



%! xml2rdf_stream(+Source, +RecordNames, +State, +Out) is det.
%! xml2rdf_stream(+Source, +RecordNames, +Opts, +State, +Out) is det.

xml2rdf_stream(Source, RecordNames, State, Out) :-
  xml2rdf_stream(Source, RecordNames, _{}, State, Out).


xml2rdf_stream(Source, RecordNames, Opts1, State, Out) :-
  q_conv_options(Opts1, Opts2),
  flag(xml2rdf, _, 0),
  xml_stream_record(
    Source,
    RecordNames,
    xml2rdf_stream0(State, Out, Opts2)
  ).


xml2rdf_stream0(State, Out, Opts, [element(_,_,Dom)]) :-
  q_abox_iri(Opts.scheme, Opts.host, Opts.concept, S),
  (get_dict(p_attrs, Opts, PAttrs) -> true ; PAttrs = []),
  xml2rdf_stream0(0, State, Out, Dom, S, PAttrs, Opts),
  flag(xml2rdf, N, N+1),
  debug(conv(xml2rdf), "~D", [N]).


xml2rdf_stream0(N, State, Out, [element(H,Attrs,Vals)|T], S, PAttrs, Opts) :-
  maplist(atomic, Vals), !,
  forall((
    member(Val, Vals),
    \+ is_empty_atom(Val)
  ), (
    q_literal(Lit, xsd:string, Val, _),
    xml_p(H, PAttrs, Attrs, P, Opts),
    gen_ntuple(S, P, Lit, State, Out)
  )),
  xml2rdf_stream0(N, State, Out, T, S, PAttrs, Opts).
xml2rdf_stream0(N1, State, Out, [element(H,Attrs,Content)|T], S, PAttrs, Opts) :- !,
  N2 is N1 + 1,
  xml_p(H, PAttrs, Attrs, P, Opts),
  q_abox_iri(Opts.scheme, Opts.host, Opts.concept, O),
  gen_ntuple(S, P, O, State, Out),
  xml2rdf_stream0(N2, State, Out, Content, O, PAttrs, Opts),
  xml2rdf_stream0(N1, State, Out, T, S, PAttrs, Opts).
xml2rdf_stream0(N, State, Out, [H|T], S, PAttrs, Opts) :-
  is_empty_atom(H), !,
  xml2rdf_stream0(N, State, Out, T, S, PAttrs, Opts).
xml2rdf_stream0(_, _, _, [], _, _, _) :- !.


xml_p(H, PAttrs, Attrs, P, Opts) :-
  xml_p_attrs(PAttrs, Attrs, T),
  atomic_list_concat([H|T], :, Term),
  q_tbox_iri(Opts.scheme, Opts.host, Term, P).


xml_p_attrs(_, [], []) :- !.
xml_p_attrs(PAttrs, [Attr|Attrs], [Val|Vals]) :-
  Attr = (Key=Val),
  memberchk(Key, PAttrs), !,
  xml_p_attrs(PAttrs, Attrs, Vals).
xml_p_attrs(PAttrs, [_|Attrs], Vals) :-
  xml_p_attrs(PAttrs, Attrs, Vals).
