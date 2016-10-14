:- module(
  lit_store_es,
  [
    create_lit_store/1 % +NumberOfThreads:positive_integer
  ]
).

/** <module> Literal store

@author Wouter Beek
@version 2015/06
*/

:- use_module(library(semweb/rdf_db), except([rdf_node/1])).
:- use_module(library(uuid)).





create_lit_store(N):-
  tmp_set_prolog_flag(cpu_count, N, create_lit_store).

create_lit_store:-
  rdf_triple_concurrent(_, _, _, store_lit).

store_lit(S, P, O, Md5):-
  ll_rdf(S, P, O, Md5),
  rdf_is_literal(O),
  lit_dict(O, ODict),
  md5_uri(Md5, G),
  uuid(Uuid0),
  atomic_list_concat(UuidComps, '-', Uuid0),
  atomic_list_concat(UuidComps, '', Uuid),
  D = _{subject: S, predicate: P, object: ODict, graph: G},
  es_post(_, lit, entry, Uuid, D),
  fail.
store_lit(_, _, _, _, _).

md5_uri(Md5, Uri):-
  atomic_concat('http://lodlaundromat.org/resource/', Md5, Uri).

lit_dict(literal(type(D,Lex)), _{datatype: D, lexical_form: Lex}):- !.
lit_dict(
  literal(lang(Lang,Lex)),
  _{datatype: D, lexical_form: Lex, language_tag:Lang}
):- !,
  rdf_equal(rdf:langString, D).
lit_dict(literal(Lex), _{datatype: D, lexical_form: Lex}):-
  rdf_equal(xsd:string, D).
