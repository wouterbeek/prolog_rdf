:- module(
  jsonld,
  [
    jsonld_subject/2, % +S, -Json
    jsonld_triples/2 % +Triples, -Json
  ]
).

/** <module> JSON-LD

@author Wouter Beek
@compat JSON-LD 1.1
@see http://www.w3.org/TR/json-ld/
@version 2016/01
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(assoc_ext)).
:- use_module(library(lists)).
:- use_module(library(pair_ext)).
:- use_module(library(rdf/rdf_api)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_statement)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(rdf11/rdf11_collections)).

:- rdf_meta
   jsonld_subject(r, -).





%! jsonld_subject(+S, -Json) is det.

jsonld_subject(S, Json):-
  rdf_subject_triples(S, Trips),
  jsonld_triples(Trips, Json).



%! jsonld_triples(+Triples, -Json) is det.

jsonld_triples(Trips, Json):-
  jsonld_triples_context(Trips, PDefs, DefLang, Context),
  jsonld_triples_tree(Trips, Tree),
  assoc_to_keys(Tree, Ss),
  maplist(jsonld_stree(PDefs, DefLang, Tree), Ss, Ds),
  (   Ds = [D]
  ->  Json = D.put('@context', Context)
  ;   dict_pairs(Json, json, ['@context'-Context,'@graph'-Ds])
  ).



%! jsonld_stree(+PDefintions:list(pair), ?DefLang, +Tree, +S, -Json) is det.

jsonld_stree(PDefs, DefLang, Tree, S1, D):-
  get_assoc(S1, Tree, Subtree),

  % '@id'
  abbreviate(S1, S2),
  (rdf_is_bnode(S1) -> Pairs1 = Pairs2 ; Pairs1 = ['@id'-S2|Pairs2]),

  % '@type'
  rdf_equal(rdf:type, RdfType),
  (   get_assoc(RdfType, Subtree, Cs1),
      maplist(abbreviate, Cs1, Cs2)
  ->  (Cs2 = [Cs3] -> true ; Cs3 = Cs2),
      Pairs2 = ['@type'-Cs3|Pairs3]
  ;   Pairs2 = Pairs3
  ),

  % Predicate-object pairs.
  assoc_to_keys(Subtree, Ps1),
  selectchk(RdfType, Ps1, Ps2),
  maplist(jsonld_ptree(PDefs, DefLang, Subtree), Ps2, Pairs3),

  % Pairs → dict.
  dict_pairs(D, jsonld, Pairs1).



%! jsonld_triples_context(+Trips, -PDefs, -DefLang, -Context) is det.

jsonld_triples_context(Trips, PDefs, _LTag, Context):-
  % Prefixes.
  rdf_triples_iris(Trips, Iris),
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
  (   jsonld_default_language(Trips, LTag)
  ->  DefLang = ['@language'-LTag]
  ;   DefLang = []
  ),
  */

  % Predicate definitions.
  rdf_triples_predicates(Trips, Ps),
  p_defs(Trips, Ps, PDefs),

  % Pairs → dict.
  append([Abbrs,PDefs], Pairs),
  dict_pairs(Context, context, Pairs).



%! jsonld_default_language(+Trips, -LTag) is semidet.

jsonld_default_language(Trips, LTag):-
  aggregate_all(set(LTag), member(rdf(_,_,_@LTag), Trips), LTags),
  maplist(language_occurrences(Trips), LTags, Ns),
  pairs_keys_values(Pairs, Ns, LTags),
  pairs_sorted_values(Pairs, @>=, [LTag|_]).



%! language_occurrences(+Trips, +LTag, -N) is det.

language_occurrences(Trips, LTag, N):-
  aggregate_all(count, member(rdf(_,_,_@LTag), Trips), N).



%! jsonld_triples_tree(+Trips, -Trees) is det.

jsonld_triples_tree(Trips, Assoc):-
  empty_assoc(Assoc0),
  jsonld_triples_tree(Trips, Assoc0, Assoc).

jsonld_triples_tree([rdf(S,P,O)|Trips], Assoc1, Assoc3):- !,
  (   get_assoc(S, Assoc1, SubAssoc1)
  ->  true
  ;   empty_assoc(SubAssoc1)
  ),
  put_assoc_ord_member(P, SubAssoc1, O, SubAssoc2),
  put_assoc(S, Assoc1, SubAssoc2, Assoc2),
  jsonld_triples_tree(Trips, Assoc2, Assoc3).
jsonld_triples_tree([], Assoc, Assoc).



%! jsonld_ptree(+PDefs, ?DefLang, +Tree, +P, -POs) is det.

jsonld_ptree(PDefs, DefLang, Tree, P1, P2-O2):-
  abbreviate(P1, P2),
  get_assoc(P1, Tree, Os1),
  maplist(jsonld_oterm(PDefs, DefLang, P2), Os1, Os2),
  (Os2 = [O2] -> true ; O2 = Os2).



%! jsonld_oterm(+PDefs, ?DefLang, +P, +O, -JsonO) is det.

jsonld_oterm(PDefs, _, P2, O1, Elems2):-
  rdf_list(O1),
  memberchk(P2-PDef, PDefs),
  '@list' == PDef.get('@container'), !,
  rdf_list(O1, Elems1),
  maplist(abbreviate, Elems1, Elems2).
jsonld_oterm(PDefs, _DefLang, P2, O1, O2):-
  O1 = Lex@LTag, !,
  (   memberchk(P2-PDef, PDefs),
      _ = PDef.get('@language')
  ->  O2 = Lex
  %;   LTag == DefLang
  %->  O2 = Lex
  ;   O2 = literal{'@language':LTag}
  ).
jsonld_oterm(PDefs, _, P2, O1, O2):-
  O1 = _^^D1, !,
  rdf_literal_lexical_form(O1, Lex),
  (   memberchk(P2-PDef, PDefs),
      _ = PDef.get('@type')
  ->  O2 = Lex
  ;   rdf_literal_datatype(O1, D1),
      abbreviate(D1, D2),
      O2 = literal{'@type':D2,'@value':Lex}
  ).
jsonld_oterm(_, _, _, O1, O2):-
  abbreviate(O1, O2).



%! abbreviate(+Iri, -Abbr) is det.

abbreviate(Iri, Abbr):-
  rdf_is_iri(Iri),
  rdf_iri_alias_prefix_local(Iri, Alias, _, Local), !,
  atomic_list_concat([Alias,Local], :, Abbr).
abbreviate(Iri, Iri).



%! p_defs(+Trips, +Ps, -Pairs) is det.

p_defs(Trips, [P1|Ps1], [P2-odef{'@container': '@list'}|Ps2]):-
  p_container(Trips, P1), !,
  abbreviate(P1, P2),
  p_defs(Trips, Ps1, Ps2).
p_defs(Trips, [P1|Ps1], [P2-odef{'@language':LTag}|Ps2]):-
  p_ltag(Trips, P1, LTag), !,
  abbreviate(P1, P2),
  p_defs(Trips, Ps1, Ps2).
p_defs(Trips, [P1|Ps1], [P2-odef{'@type':D2}|Ps2]):-
  p_type(Trips, P1, D1), !,
  abbreviate(P1, P2),
  abbreviate(D1, D2),
  p_defs(Trips, Ps1, Ps2).
p_defs(Trips, [_|T1], T2):- !,
  p_defs(Trips, T1, T2).
p_defs(_, [], []).



%! p_container(+Trips, +P) is semidet.

p_container(Trips, P):-
  memberchk(rdf(_,P,O), Trips),
  rdf_list(O),
  forall(member(rdf(_,P,O), Trips), rdf_list(O)).



%! p_ltag(+Trips, +P, -LTag) is semidet.

p_ltag(Trips, P, LTag):-
  memberchk(rdf(_,P,_@LTag), Trips),
  forall(member(rdf(_, P, O), Trips), O = _@LTag).



%! p_type(+Trips, +P, -D) is semidet.

p_type(Trips, P, D):-
  memberchk(rdf(_,P,O1), Trips),
  o_type(O1, D),
  forall(member(rdf(_,P,O), Trips), o_type(O, D)).



%! o_type(+O, -D) is det.

o_type(O, '@id'):- rdf_is_iri(O), !.
o_type(O, D):- O = _^^D.
