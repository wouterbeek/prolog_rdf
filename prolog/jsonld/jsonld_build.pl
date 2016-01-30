:- module(
  jsonld_build,
  [
    subject_to_jsonld/2, % +S, -Jsonld
    triples_to_jsonld/2  % +Triples, -Jsonld
  ]
).

/** <module> JSON-LD build

@author Wouter Beek
@compat JSON-LD 1.1
@see http://www.w3.org/TR/json-ld/
@version 2016/01
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(assoc_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(lists)).
:- use_module(library(pair_ext)).
:- use_module(library(rdf/rdf_api)).
:- use_module(library(rdf/rdf_datatype)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_statement)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(rdf11/rdf11_collections)).
:- use_module(library(typecheck)).

:- rdf_meta
   subject_to_jsonld(r, -),
   triples_to_jsonld(t, -).





%! subject_to_jsonld(+S, -Jsonld) is det.

subject_to_jsonld(S, Jsonld) :-
  rdf_subject_triples(S, Triples),
  triples_to_jsonld(Triples, Jsonld).



%! triples_to_jsonld(+Triples, -Jsonld) is det.

triples_to_jsonld(Triples, Jsonld) :-
  jsonld_triples_context(Triples, PDefs, DefLang, Context),
  jsonld_triples_tree(Triples, Tree),
  assoc_to_keys(Tree, Ss),
  maplist(jsonld_subject_tree(PDefs, DefLang, Tree), Ss, Ds),
  (   Ds = [D]
  ->  Jsonld = D.put('@context', Context)
  ;   dict_pairs(Jsonld, ['@context'-Context,'@graph'-Ds])
  ).
  


%! jsonld_subject_tree(+PDefintions:list(pair), ?DefLang, +Tree, +S, -Jsonld) is det.

jsonld_subject_tree(PDefs, DefLang, Tree, S1, D) :-
  get_assoc(S1, Tree, Subtree),

  % '@id'
  jsonld_abbreviate_iri0(S1, S2),
  (rdf_is_bnode(S1) -> Pairs1 = Pairs2 ; Pairs1 = ['@id'-S2|Pairs2]),

  % '@type'
  rdf_equal(rdf:type, RdfType),
  (   get_assoc(RdfType, Subtree, Cs1),
      maplist(jsonld_abbreviate_iri, Cs1, Cs2)
  ->  (Cs2 = [Cs3] -> true ; Cs3 = Cs2),
      Pairs2 = ['@type'-Cs3|Pairs3]
  ;   Pairs2 = Pairs3
  ),

  % Predicate-object pairs.
  assoc_to_keys(Subtree, Ps1),
  selectchk(RdfType, Ps1, Ps2),
  maplist(jsonld_ptree(PDefs, DefLang, Subtree), Ps2, Pairs3),

  % Pairs → dict.
  dict_pairs(D, Pairs1).



%! jsonld_triples_context(+Triples, -PDefs, -DefLang, -Context) is det.

jsonld_triples_context(Triples, PDefs, _LTag, Context) :-
  % Prefixes.
  rdf_triples_iris(Triples, Iris),
  aggregate_all(
    set(Alias-Prefix),
    (
      member(Iri, Iris),
      rdf_iri_alias_prefix_local(Iri, Alias, Prefix, _)
    ),
    Abbrs
  ),

  /*
  % Default language, if any.
  (   jsonld_default_language(Triples, LTag)
  ->  DefLang = ['@language'-LTag]
  ;   DefLang = []
  ),
  */

  % Predicate definitions.
  rdf_triples_predicates(Triples, Ps),
  p_defs(Triples, Ps, PDefs),

  % Pairs → dict.
  append([Abbrs,PDefs], Pairs),
  dict_pairs(Context, Pairs).



%! jsonld_default_language(+Triples, -LTag) is semidet.

jsonld_default_language(Triples, LTag) :-
  aggregate_all(set(LTag), member(rdf(_,_,_@LTag), Triples), LTags),
  maplist(language_occurrences(Triples), LTags, Ns),
  pairs_keys_values(Pairs, Ns, LTags),
  pairs_sorted_values(Pairs, @>=, [LTag|_]).



%! language_occurrences(+Triples, +LTag, -N) is det.

language_occurrences(Triples, LTag, N) :-
  aggregate_all(count, member(rdf(_,_,_@LTag), Triples), N).



%! jsonld_triples_tree(+Triples, -Trees) is det.

jsonld_triples_tree(Triples, Assoc) :-
  empty_assoc(Assoc0),
  jsonld_triples_tree(Triples, Assoc0, Assoc).


jsonld_triples_tree([rdf(S,P,O)|Triples], Assoc1, Assoc3) :- !,
  (get_assoc(S, Assoc1, SubAssoc1) -> true ; empty_assoc(SubAssoc1)),
  put_assoc_ord_member(P, SubAssoc1, O, SubAssoc2),
  put_assoc(S, Assoc1, SubAssoc2, Assoc2),
  jsonld_triples_tree(Triples, Assoc2, Assoc3).
jsonld_triples_tree([], Assoc, Assoc).



%! jsonld_ptree(+PDefs, ?DefLang, +Tree, +P, -POs) is det.

jsonld_ptree(PDefs, DefLang, Tree, P1, P2-O2) :-
  jsonld_abbreviate_iri(P1, P2),
  get_assoc(P1, Tree, Os1),
  maplist(jsonld_oterm(PDefs, DefLang, P2), Os1, Os2),
  (Os2 = [O2] -> true ; O2 = Os2).



%! jsonld_oterm(+PDefs, ?DefLang, +P, +O, -JsonldO) is det.

jsonld_oterm(PDefs, _, P2, O1, Elems2) :-
  rdf_list(O1),
  memberchk(P2-PDef, PDefs),
  '@list' == PDef.get('@container'), !,
  rdf_list(O1, Elems1),
  maplist(jsonld_abbreviate_iri, Elems1, Elems2).
jsonld_oterm(PDefs, _DefLang, P2, O1, O2) :-
  O1 = Lex@LTag, !,
  (   memberchk(P2-PDef, PDefs),
      _ = PDef.get('@language')
  ->  O2 = Lex
  %;   LTag == DefLang
  %->  O2 = Lex
  ;   O2 = literal{'@language':LTag}
  ).
jsonld_oterm(PDefs, _, P2, O1, O2) :-
  O1 = _^^D1, !,
  rdf_literal_lexical_form(O1, Lex),
  (   memberchk(P2-PDef, PDefs),
      _ = PDef.get('@type')
  ->  O2 = Lex
  ;   rdf_literal_datatype(O1, D1),
      jsonld_abbreviate_iri(D1, D2),
      O2 = literal{'@type':D2,'@value':Lex}
  ).
jsonld_oterm(_, _, _, O1, O2) :-
  jsonld_abbreviate_iri(O1, O2).



%! p_defs(+Triples, +Ps, -Pairs) is det.

p_defs(Triples, [P1|Ps1], [P2-odef{'@container': '@list'}|Ps2]) :-
  p_container(Triples, P1), !,
  jsonld_abbreviate_iri(P1, P2),
  p_defs(Triples, Ps1, Ps2).
p_defs(Triples, [P1|Ps1], [P2-odef{'@language':LTag}|Ps2]) :-
  p_ltag(Triples, P1, LTag), !,
  jsonld_abbreviate_iri(P1, P2),
  p_defs(Triples, Ps1, Ps2).
p_defs(Triples, [P1|Ps1], [P2-odef{'@type':'@id'}|Ps2]) :-
  p_iri(Triples, P1), !,
  jsonld_abbreviate_iri(P1, P2),
  p_defs(Triples, Ps1, Ps2).
p_defs(Triples, [P1|Ps1], [P2-odef{'@type':D2}|Ps2]) :-
  p_datatype(Triples, P1, D1), !,
  jsonld_abbreviate_iri(P1, P2),
  jsonld_abbreviate_iri(D1, D2),
  p_defs(Triples, Ps1, Ps2).
p_defs(Triples, [_|T1], T2) :- !,
  p_defs(Triples, T1, T2).
p_defs(_, [], []).



%! p_container(+Triples, +P) is semidet.
% Predicate P always has an RDF container as its object term.

p_container(Triples, P) :-
  memberchk(rdf(_,P,O), Triples),
  rdf_list(O),
  forall(member(rdf(_,P,O), Triples), rdf_list(O)).



%! p_datatype(+Triples, +P, -D) is semidet.
% 

p_datatype(Triples, P, Sup) :-
  memberchk(rdf(_,P,O), Triples),
  rdf_literal_datatype(O, D),
  findall(D, (member(rdf(_,P,O), Triples), rdf_literal_datatype(O, D)), Ds),
  rdf_datatype_supremum(Ds, Sup).



%! p_iri(+Triples, +P) is semidet.

p_iri(Triples, P) :-
  forall(member(rdf(_,P,O), Triples), is_iri(O)).



%! p_ltag(+Triples, +P, -LTag) is semidet.
% Predicate P always has a language tagged string as its object term.

p_ltag(Triples, P, LTag) :-
  memberchk(rdf(_,P,_@LTag), Triples),
  forall(member(rdf(_,P,O), Triples), O = _@LTag).



%! jsonld_abbreviate_iri0(+Iri, -Abbr) is det.

jsonld_abbreviate_iri0(Iri, Abbr) :-
  is_iri(Iri),
  rdf_iri_alias_prefix_local(Iri, Alias, _, Local), !,
  atomic_list_concat([Alias,Local], :, Abbr).
jsonld_abbreviate_iri0(Iri, Iri).
