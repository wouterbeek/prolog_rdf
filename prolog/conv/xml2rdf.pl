:- module(
  xml2rdf,
  [
    marcxml2rdf/3, % +Source, +RecordNames, +Opts
    xml2rdf/3      % +Source, +RecordNames, +Opts
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





%! marcxml2rdf(+Source, +RecordNames, +Opts) is det.

marcxml2rdf(Source, RecordNames, Opts1) :-
  conv_alias_options(Opts1, Opts2),
  Alias = Opts2.alias,
  xml_stream_record(
    Source,
    RecordNames,
    {Alias}/[Dom]>>marcxml2rdf_record0(Dom, Alias)
  ).


marcxml2rdf_record0(Dom, Alias) :-
  q_create_bnode(S),
  forall(
    marcxml2rdf_stmt0(Dom, Alias, P, Val),
    gen_ntriple(S, P, Val^^xsd:string)
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



%! xml2rdf(+Source, +RecordNames, +Opts) is nondet.

xml2rdf(Source, RecordNames, Opts1) :-
  conv_alias_options(Opts1, Opts2),
  xml_stream_record(Source, RecordNames, xml2rdf_record0(Opts2)).


xml2rdf_record0(Opts, Dom) :-
  qb_bnode(S),
  get_dict(ltag_attr, Opts, LTagAttr),
  xml2rdf_record0(Dom, [], S, LTagAttr, Opts).


xml2rdf_record0([], _, _, _, _) :- !.
xml2rdf_record0([Empty|Dom], L, S, LTagAttr, Opts) :-
  is_empty_atom(Empty), !,
  xml2rdf_record0(Dom, L, S, LTagAttr, Opts).
xml2rdf_record0([element(H,Attrs,Vals)|Dom], T, S, LTagAttr, Opts) :-
  (   maplist(atomic, Vals)
  ->  reverse([H|T], L),
      atomic_list_concat(L, '_', Name),
      rdf_global_id(Opts.tbox_alias:Name, P),
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
        gen_ntriple(S, P, Lit)
      ))
  ;   xml2rdf_record0(Vals, [H|T], S, LTagAttr, Opts)
  ),
  xml2rdf_record0(Dom, [], S, LTagAttr, Opts).
