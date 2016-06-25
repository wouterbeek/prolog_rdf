:- module(
  xml2rdf,
  [
    marcxml2rdf/4, % +Mode, +Source, +RecordNames, +Sink
    marcxml2rdf/5, % +Mode, +Source, +RecordNames, +Sink, +Opts
    xml2rdf/4,     % +Mode, +Source, +RecordNames, +Sink
    xml2rdf/5      % +Mode, +Source, +RecordNames, +Sink, +Opts
  ]
).

/** <module> XML-2-RDF

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(conv/rdf_conv)).
:- use_module(library(debug)).
:- use_module(library(dict_ext)).
:- use_module(library(lists)).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(xml/marcxml)).
:- use_module(library(xml/xml_stream)).
:- use_module(library(yall)).

:- rdf_meta
   marcxml2rdf(+, +, +, r),
   marcxml2rdf(+, +, +, r, +),
   xml2rdf(+, +, +, r),
   xml2rdf(+, +, +, r, +).

:- debug(xml2rdf).





%! marcxml2rdf(+Mode, +Source, +RecordNames, +Sink) is det.
%! marcxml2rdf(+Mode, +Source, +RecordNames, +Sink, +Opts) is det.

marcxml2rdf(Mode, Source, RecordNames, Sink) :-
  marcxml2rdf(Mode, Source, RecordNames, Sink, _{}).


marcxml2rdf(Mode, Source, RecordNames, Sink, Opts1) :-
  rdf_conv_alias_options(Opts1, Opts2),
  Alias = Opts2.alias,
  xml_stream_record(
    Source,
    RecordNames,
    {Mode,Alias,Sink}/[Dom]>>marcxml2rdf_assert_record0(Mode, Dom, Alias, Sink)
  ).


marcxml2rdf_assert_record0(Mode, Dom, Alias, Sink) :-
  rdf_create_bnode(S),
  forall(
    marcxml2rdf_stmt0(Dom, Alias, P, Val),
    rdf_assert_mode(Mode, S, P, Val^^xsd:string, Sink)
  ).


marcxml2rdf_stmt0(Dom, Alias, P, Val) :-
  marcxml_controlfield(Dom, Field, Val0),
  rdf_global_id(Alias:Field, P),
  atom_string(Val0, Val).
marcxml2rdf_stmt0(Dom, Alias, P, Val) :-
  marcxml_datafield(Dom, Field, Subfield, Val0),
  atom_concat(Field, Subfield, Local),
  rdf_global_id(Alias:Local, P),
  atom_string(Val0, Val).



%! xml2rdf(+Mode, +Source, +RecordNames, +Sink) is nondet.
%! xml2rdf(+Mode, +Source, +RecordNames, +Sink, +Opts) is nondet.

xml2rdf(Mode, Source, RecordNames, Sink) :-
  xml2rdf(Mode, Source, RecordNames, Sink, _{}).


xml2rdf(Mode, Source, RecordNames, Sink, Opts1) :-
  rdf_conv_alias_options(Opts1, Opts2),
  xml_stream_record(
    Source,
    RecordNames,
    {Mode}/[Dom]>>xml2rdf_assert_record0(Mode, Dom, Sink, Opts2)
  ).


xml2rdf_assert_record0(Mode, Dom, Sink, Opts) :-
  rdf_create_bnode(S),
  get_dict(ltag_attr, Opts, LTagAttr),
  xml2rdf_assert_record0(Mode, Dom, [], S, LTagAttr, Sink, Opts).


xml2rdf_assert_record0(_, [], _, _, _, _, _) :- !.
xml2rdf_assert_record0(Mode, [Empty|Dom], L, S, LTagAttr, Sink, Opts) :-
  is_empty_atom(Empty), !,
  xml2rdf_assert_record0(Mode, Dom, L, S, LTagAttr, Sink, Opts).
xml2rdf_assert_record0(Mode, [element(H,Attrs,Vals)|Dom], T, S, LTagAttr, Sink, Opts) :-
  (   maplist(atomic, Vals)
  ->  reverse([H|T], L),
      atomic_list_concat(L, '_', Name),
      rdf_global_id(Opts.tbox_alias:Name, P),
      forall((
        member(Val, Vals),
        \+ is_empty_atom(Val)
      ), (
        (   memberchk(LTagAttr=LTag, Attrs)
        ->  z_literal(Lit, rdf:langString, Val, LTag)
        ;   z_literal(Lit, xsd:string, Val, _)
        ),
        (debugging(xml2rdf) -> rdf_print_triple(S, P, Lit) ; true),
        rdf_assert_mode(Mode, S, P, Lit, Sink)
      ))
  ;   xml2rdf_assert_record0(Mode, Vals, [H|T], S, LTagAttr, Sink, Opts)
  ),
  xml2rdf_assert_record0(Mode, Dom, [], S, LTagAttr, Sink, Opts).
