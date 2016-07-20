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
:- use_module(library(debug)).
:- use_module(library(dict_ext)).
:- use_module(library(gen/gen_ntuples)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(q/q_fs)).
:- use_module(library(q/q_print)).
:- use_module(library(q/q_term)).
:- use_module(library(q/qb)).
:- use_module(library(xml/marcxml)).
:- use_module(library(xml/xml_stream)).
:- use_module(library(yall)).





%! xml2rdf(+Source, +Sink, +RecordNames) is nondet.
%! xml2rdf(+Source, +Sink, +RecordNames, +Opts) is nondet.

xml2rdf(Source, Sink, RecordNames) :-
  xml2rdf(Source, Sink, RecordNames, _{}).


xml2rdf(Source, Sink, RecordNames, Opts1) :-
  (   get_dict(entry_name, Opts1, Entry)
  ->  Opts2 = [entry_name(Entry)]
  ;   Opts2 = []
  ),
  call_to_ntriples(Sink, xml2rdf_stream(Source, RecordNames, Opts1), Opts2).



%! xml2rdf_stream(+Source, +RecordNames, +State, +Out) is det.
%! xml2rdf_stream(+Source, +RecordNames, +Opts, +State, +Out) is det.

xml2rdf_stream(Source, RecordNames, State, Out) :-
  xml2rdf_stream(Source, RecordNames, _{}, State, Out).


xml2rdf_stream(Source, RecordNames, Opts1, State, Out) :-
  q_fs:q_alias_options0(Opts1, Opts2),
  xml_stream_record(
    Source,
    RecordNames,
    xml2rdf_stream0(State, Out, Opts2)
  ).


xml2rdf_stream0(State, Out, Opts, [element(_,_,Dom)]) :-
  (get_dict(s, Opts, S) -> true ; qb_iri(Opts.abox_alias, S)),
  (get_dict(p_attrs, Opts, PAttrs) -> true ; PAttrs = []),
  xml2rdf_stream0(0, Opts.tbox_alias, State, Out, Dom, S, PAttrs).


xml2rdf_stream0(N, Alias, State, Out, [element(H,Attrs,Vals)|T], S, PAttrs) :-
  maplist(atomic, Vals), !,
  forall((
    member(Val, Vals),
    \+ is_empty_atom(Val)
  ), (
    %%%%(   ground(LTagAttr),
    %%%%    memberchk(LTagAttr=LTag0, Attrs)
    %%%%->  % Language-tagged strings must adhere to the following
    %%%%    % grammar: `[a-zA-Z]+ ('-' [a-zA-Z0-9]+)*`
    %%%%    atomic_list_concat([x,LTag0], -, LTag),
    %%%%    q_literal(Lit, rdf:langString, Val, LTag)
    q_literal(Lit, xsd:string, Val, _),
    xml_p(Alias, H, PAttrs, Attrs, P),
    %%%%debug(conv(xml2rdf), "XML value ~w", [Lit]),
    gen_ntuple(S, P, Lit, State, Out)
  )),
  xml2rdf_stream0(N, Alias, State, Out, T, S, PAttrs).
xml2rdf_stream0(N1, Alias, State, Out, [element(H,Attrs,Content)|T], S, PAttrs) :- !,
  N2 is N1 + 1,
  debug(conv(xml2rdf), "XML nesting level ~D", [N2]),
  xml_p(Alias, H, PAttrs, Attrs, P),
  qb_iri(Alias, O),
  gen_ntuple(S, P, O, State, Out),
  xml2rdf_stream0(N2, Alias, State, Out, Content, O, PAttrs),
  xml2rdf_stream0(N1, Alias, State, Out, T, S, PAttrs).
xml2rdf_stream0(N, Alias, State, Out, [H|T], S, PAttrs) :-
  is_empty_atom(H), !,
  xml2rdf_stream0(N, Alias, State, Out, T, S, PAttrs).
xml2rdf_stream0(_, _, _, _, [], _, _) :- !.
xml2rdf_stream0(N, Alias, State, Out, Dom, S, PAttrs) :-
  gtrace, %DEB
  xml2rdf_stream0(N, Alias, State, Out, Dom, S, PAttrs).


xml_p(Alias, H, PAttrs, Attrs, P) :-
  xml_p_attrs(PAttrs, Attrs, T),
  atomic_list_concat([H|T], :, Local),
  rdf_global_id(Alias:Local, P).


xml_p_attrs(_, [], []) :- !.
xml_p_attrs(PAttrs, [Attr|Attrs], [Val|Vals]) :-
  Attr = (Key=Val),
  memberchk(Key, PAttrs), !,
  xml_p_attrs(PAttrs, Attrs, Vals).
xml_p_attrs(PAttrs, [_|Attrs], Vals) :-
  xml_p_attrs(PAttrs, Attrs, Vals).
