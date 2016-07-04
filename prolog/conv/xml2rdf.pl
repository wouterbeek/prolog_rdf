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
@version 2016/06
*/

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
  xml2rdf_stream0(Opts.tbox_alias, State, Out, Dom, [], S, LTagAttr).


xml2rdf_stream0(_, _, _, [], _, _, _) :- !.
xml2rdf_stream0(Alias, State, Out, [Empty|Dom], L, S, LTagAttr) :-
  is_empty_atom(Empty), !,
  xml2rdf_stream0(Alias, State, Out, Dom, L, S, LTagAttr).
xml2rdf_stream0(Alias, State, Out, [element(H,Attrs,Vals)|Dom], T, S, LTagAttr) :-
  (   maplist(atomic, Vals)
  ->  reverse([H|T], L),
      atomic_list_concat(L, '_', Name),
      rdf_global_id(Alias:Name, P),
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
        (   debugging(conv(xml2rdf))
        ->  with_output_to(user_output, q_print_triple(S, P, Lit))
        ;   true
        ),
        gen_ntuple(S, P, Lit, State, Out)
      ))
  ;   xml2rdf_stream0(Alias, State, Out, Vals, [H|T], S, LTagAttr)
  ),
  xml2rdf_stream0(Alias, State, Out, Dom, [], S, LTagAttr).
