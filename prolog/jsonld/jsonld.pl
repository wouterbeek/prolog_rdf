:- module(
  jsonld,
  [
    jsonld_deref/2 % +S, -Dict
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
:- use_module(library(rdf/id_store)).
:- use_module(library(rdf/rdf_literal)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_read)).
:- use_module(library(rdf/rdf_term)).

:- rdf_meta
   jsonld_deref(r, -).





%! jsonld_deref(+S, -Dict) is det.

jsonld_deref(S1, D):-
  % Prefixes
  rdf_deref_terms(S1, Ts),
  aggregate_all(
    set(Alias-Prefix),
    (
      member(T, Ts),
      rdf_iri_alias_prefix_local(T, Alias, Prefix, _)
    ),
    Abbrs
  ),

  % '@id'
  abbreviate(S1, S2),
  (rdf_is_bnode(S1) -> Pairs1 = Pairs2 ; Pairs1 = ['@id'-S2|Pairs2]),

  % '@type'
  aggregate_all(set(C2), (rdf_instance(S1, C1), abbreviate(C1, C2)), Cs1),
  (   Cs1 == []
  ->  Pairs2 = Pairs3
  ;   (Cs1 = [Cs2] -> true ; Cs2 = Cs1),
      Pairs2 = ['@type'-Cs2|Pairs3]
  ),

  % Predicate definitions.
  aggregate_all(set(P), (rdf(S1, P, _, _, _, _), \+ rdf_equal(rdf:type, P)), Ps),
  p_defs(Ps, PDefs),

  % Predicate-object pairs.
  aggregate_all(set(P-O), rdf(S1, P, O, _, _, _), Pairs4),
  maplist(jsonld_pair(PDefs), Pairs4, Pairs3),

  append(Abbrs, PDefs, Pairs5),
  dict_pairs(Context, context, Pairs5),

  % Pairs → dict.
  dict_pairs(D, json_ld, ['@context'-Context|Pairs1]).


%! jsonld_pair(+PredicateDefinitions, +Pair, -Pair) is det.

jsonld_pair(PDefs, P1-O1, P2-O2):-
  abbreviate(P1, P2),
  (   rdf_is_literal(O1)
  ->  rdf_literal_data(lexical_form, O1, Lex),
      (   memberchk(P2-_, PDefs)
      ->  O2 = Lex
      ;   rdf_literal_data(datatype, O1, D1),
          abbreviate(D1, D2),
          O2 = literal{'@type':D2,'@value':Lex}
      )
  ;   O2 = O1
  ).


%! abbreviate(+Iri, -Abbr) is det.

abbreviate(Iri, Abbr):-
  rdf_is_iri(Iri),
  rdf_iri_alias_prefix_local(Iri, Alias, _, Local), !,
  atomic_list_concat([Alias,Local], :, Abbr).
abbreviate(Iri, Iri).


%! p_defs(+Ps, -Pairs) is det.

p_defs([P1|Ps1], [P2-odef{'@type':D2}|Ps2]):-
  p_datatype(P1, D1), !,
  abbreviate(P1, P2),
  abbreviate(D1, D2),
  p_defs(Ps1, Ps2).
p_defs([_|T1], T2):- !,
  p_defs(T1, T2).
p_defs([], []).



%! rdf_deref_terms(+S, -Ts) is det.

rdf_deref_terms(S, Ts):-
  aggregate_all(
    set(T),
    (
      rdf(S, P, O, _, _, _),
      (T = S ; \+ rdf_equal(rdf:type, P), T = P ; T = O),
      rdf_is_iri(T)
    ),
    Ts
  ).


%! p_datatype(+P, -D) is semidet.

p_datatype(P, D):-
  aggregate_all(set(D), rdf(_, P, _^^D), Ds),
  Ds = [D].
