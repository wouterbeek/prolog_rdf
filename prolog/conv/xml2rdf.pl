:- module(
  xml2rdf,
  [
    marcxml2rdf_graph/3, % +Source, +RecordNames, +G
    marcxml2rdf_graph/3, % +Source, +RecordNames, +G, +Opts
    xml2rdf_graph/3,     % +Source, +RecordNames, +G
    xml2rdf_graph/4      % +Source, +RecordNames, +G, +Opts
  ]
).

/** <module> XML-2-RDF

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(conv/rdf_conv)).
:- use_module(library(dict_ext)).
:- use_module(library(lists)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(xml/marcxml)).
:- use_module(library(xml/xml_stream)).
:- use_module(library(yall)).





%! marcxml2rdf_graph(+Source, +RecordNames, +G) is det.
%! marcxml2rdf_graph(+Source, +RecordNames, +G, +Opts) is det.

marcxml2rdf_graph(Source, RecordNames, G) :-
  marcxml2rdf_graph(Source, RecordNames, G, _{}).


marcxml2rdf_graph(Source, RecordNames, G, Opts1) :-
  rdf_conv_alias_options(Opts1, Opts2),
  Alias = Opts2.alias,
  xml_stream_record(
    Source,
    RecordNames,
    {Alias,G}/[Dom]>>marcxml2rdf_assert_record0(Dom, Alias, G)
  ).


marcxml2rdf_assert_record0(Dom, Alias, G) :-
  rdf_create_bnode(S),
  forall(
    marcxml2rdf_stmt0(Dom, Alias, P, Val),
    rdf_assert(S, P, Val^^xsd:string, G)
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



%! xml2rdf_graph(+Source, +RecordNames, +G) is nondet.
%! xml2rdf_graph(+Source, +RecordNames, +G, +Opts) is nondet.

xml2rdf_graph(Source, RecordNames, G) :-
  xml2rdf_graph(Source, RecordNames, G, _{}).


xml2rdf_graph(Source, RecordNames, G, Opts1) :-
  rdf_conv_alias_options(Opts1, Opts2),
  xml_stream_record(
    Source,
    RecordNames,
    [Dom]>>xml2rdf_assert_record0(Dom, G, Opts2)
  ).


xml2rdf_assert_record0(Dom, G, Opts) :-
  rdf_create_bnode(S),
  xml2rdf_assert_record0(Dom, [], S, G, Opts).


xml2rdf_assert_record0([], _, _, _, _) :- !.
xml2rdf_assert_record0([Empty|Dom], L, S, G, Opts) :-
  is_empty_atom(Empty), !,
  xml2rdf_assert_record0(Dom, L, S, G, Opts).
xml2rdf_assert_record0([element(H,Attrs,Vals)|Dom], T, S, G, Opts) :-
  (   maplist(atomic, Vals)
  ->  reverse([H|T], L),
      atomic_list_concat(L, '_', Name),
      rdf_global_id(Opts.tbox_alias:Name, P),
      forall((
        member(Val, Vals),
        \+ is_empty_atom(Val)
      ), (
        (   memberchk(language=LTag, Attrs)
        ->  rdf_literal(Lit, rdf:langString, Val, LTag)
        ;   rdf_literal(Lit, xsd:string, Val, _)
        ),
        rdf_print_quad(S, P, Lit, G),
        rdf_assert(S, P, Lit, G)
      ))
  ;   xml2rdf_assert_record0(Vals, [H|T], S, G, Opts)
  ),
  xml2rdf_assert_record0(Dom, [], S, G, Opts).
