:- module(
  xml2rdf,
  [
    xml2rdf/3,        % +Source, +Sink, +RecordNames
    xml2rdf/4,        % +Source, +Sink, +RecordNames, +Opts
    xml2rdf_stream/4, % +Source,        +RecordNames,        +State, +Out
    xml2rdf_stream/5  % +Source,        +RecordNames, +Opts, +State, +Out
  ]
).

/** <module> XML-2-RDF

@author Wouter Beek
@tbd Currently runs out of memory for unclear reasons.
@version 2016/06-2016/07
*/

:- set_prolog_stack(global, limit(7*10**9)).

:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(conv/q_conv)).
:- use_module(library(debug)).
:- use_module(library(dict_ext)).
:- use_module(library(gen/gen_ntuples)).
:- use_module(library(lists)).
:- use_module(library(q/q_print)).
:- use_module(library(q/q_term)).
:- use_module(library(q/qb)).
:- use_module(library(xml/marcxml)).
:- use_module(library(xml/xml_stream)).
:- use_module(library(yall)).





%! xml2rdf(+Source, +Sink, +RecordNames) is nondet.
%! xml2rdf(+Source, +Sink, +RecordNames, +Opts) is nondet.

xml2rdf(Source, Sink, RecordNames) :-
  xml2rdf(Source, Sink, RecordNames, []).


xml2rdf(Source, Sink, RecordNames, Opts) :-
  call_to_ntriples(Sink, xml2rdf_stream(Source, RecordNames, Opts)).



%! xml2rdf_stream(+Source, +RecordNames, +State, +Out) is det.
%! xml2rdf_stream(+Source, +RecordNames, +Opts, +State, +Out) is det.

xml2rdf_stream(Source, RecordNames, State, Out) :-
  xml2rdf_stream(Source, RecordNames, [], State, Out).


xml2rdf_stream(Source, RecordNames, Opts1, State, Out) :-
  conv_alias_options(Opts1, Opts2),
  xml_stream_record(
    Source,
    RecordNames,
    xml2rdf_stream0(State, Out, Opts2)
  ).


xml2rdf_stream0(State, Out, Opts, Dom) :-
  qb_bnode(S),
  get_dict(ltag_attr, Opts, LTagAttr),
  xml2rdf_stream0(0, Opts.tbox_alias, State, Out, Dom, S, LTagAttr).


xml2rdf_stream0(N, Alias, State, Out, [element(H,Attrs,Vals)|T], S, LTagAttr) :-
  maplist(atomic, Vals), !,
  forall((
    member(Val, Vals),
    \+ is_empty_atom(Val)
  ), (
    (   memberchk(LTagAttr=LTag0, Attrs)
    ->  % Language-tagged strings must adhere to the following
        % grammar: `[a-zA-Z]+ ('-' [a-zA-Z0-9]+)*`
        atomic_list_concat([x,LTag0], -, LTag),
        q_literal(Lit, rdf:langString, Val, LTag)
    ;   q_literal(Lit, xsd:string, Val, _)
    ),
    rdf_global_id(Alias:H, P),
    %%%%debug(conv(xml2rdf), "XML value ~w", [Lit]),
    gen_ntuple(S, P, Lit, State, Out)
  )),
  xml2rdf_stream0(N, Alias, State, Out, T, S, LTagAttr).
xml2rdf_stream0(N1, Alias, State, Out, [element(H,_,Content)|T], S, LTagAttr) :- !,
  N2 is N1 + 1,
  debug(conv(xml2rdf), "XML nesting level ~D", [N2]),
  rdf_global_id(Alias:H, P),
  qb_bnode(O),
  gen_ntuple(S, P, O, State, Out),
  xml2rdf_stream0(N2, Alias, State, Out, Content, O, LTagAttr),
  xml2rdf_stream0(N1, Alias, State, Out, T, S, LTagAttr).
xml2rdf_stream0(N, Alias, State, Out, [H|T], S, LTagAttr) :-
  is_empty_atom(H), !,
  xml2rdf_stream0(N, Alias, State, Out, T, S, LTagAttr).
xml2rdf_stream0(_, _, _, _, [], _, _) :- !.
xml2rdf_stream0(N, Alias, State, Out, Dom, S, LTagAttr) :-
  gtrace, %DEB
  xml2rdf_stream0(N, Alias, State, Out, Dom, S, LTagAttr).
