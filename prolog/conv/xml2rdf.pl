:- module(
  xml2rdf,
  [
    xml2rdf_graph/3, % +Source, :Record_2, +G
    xml2rdf_graph/4  % +Source, :Record_2, +G, +Opts
  ]
).

/** <module> XML-2-RDF

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(apply)).
:- use_module(library(dict_ext)).
:- use_module(library(http/http_download)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf11)).

:- meta_predicate
    xml2rdf_graph(+, 2, +),
    xml2rdf_graph(+, 2, +, +).

:- rdf_register_prefix(ex, 'http://example.org/').





%! xml2rdf_graph(+Source, :Record_2, +G) is nondet.
%! xml2rdf_graph(+Source, :Record_2, +G, +Opts) is nondet.
%
% Fails where there are no records to convert.

xml2rdf_graph(Source, Record_2, G) :-
  xml2rdf_graph(Source, Record_2, G, _{}).


xml2rdf_graph(Source, Record_2, G, Opts1) :-
  (   del_dict(alias, Opts1, Alias, Opts2)
  ->  Opts3 = Opts2.put(_{abox_alias: Alias, tbox_alias: Alias})
  ;   dict_put_default(abox_alias, Opts1, ex, Opts2),
      dict_put_default(tbox_alias, Opts2, ex, Opts3)
  ),
  xml_download(Source, Dom, Opts3),
  findall(Record, call(Record_2, Dom, Record), Records),
  (   Records == []
  ->  true
  ;   forall(member(Record, Records), xml2rdf_record(Record, G, Opts3))
  ).


xml2rdf_record(Record, G, Opts) :-
  flag(xml_record, N, N + 1),
  atom_number(Name, N),
  rdf_global_id(Opts.abox_alias:Name, S),
  xml2rdf_record(Record, [], S, G, Opts).


xml2rdf_record([], _, _, _, _) :- !.
xml2rdf_record([element(H, _, Os)|Dom], T, S, G, Opts) :-
  (   maplist(atomic, Os)
  ->  reverse([H|T], L),
      atomic_list_concat(L, '_', Name),
      rdf_global_id(Opts.tbox_alias:Name, P),
      forall(member(O, Os), rdf_assert(S, P, O^^xsd:string, G))
  ;   xml2rdf_record(Os, [H|T], S, G, Opts)
  ),
  xml2rdf_record(Dom, [], S, G, Opts).
