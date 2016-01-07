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
:- use_module(library(pairs)).
:- use_module(library(rdf/id_store)).
:- use_module(library(rdf/rdf_list)).
:- use_module(library(rdf/rdf_literal)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_read)).
:- use_module(library(rdf/rdf_statement)).
:- use_module(library(rdf/rdf_term)).

:- rdf_meta
   jsonld_subject(r, -).





%! jsonld_subject(+S, -Json) is det.

jsonld_subject(S, Json):-
  rdf_subject_triples(S, Trips),
  jsonld_triples(Trips, Json).



%! jsonld_triples(+Triples, -Json) is det.

jsonld_triples(Trips, Json):-
  jsonld_triples_context(Trips, PDefs, Context),
  jsonld_triples_tree(Trips, Tree),
  assoc_to_keys(Tree, Ss),
  maplist(jsonld_stree(PDefs, Context, Tree), Ss, Ds),
  (Ds = [D] -> Json = D ; Json = Ds).



%! jsonld_stree(
%!   +PredicateDefintions:list(pair),
%!   +Context:dict,
%!   +Tree,
%!   +S,
%!   -Json:dict
%! ) is det.

jsonld_stree(PDefs, Context, Tree, S1, D):-
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
  assoc_to_keys(Subtree, Ps),
  maplist(jsonld_ptree(PDefs, Subtree), Ps, Pairs3),

  % Pairs → dict.
  dict_pairs(D, jsonld, ['@context'-Context|Pairs1]).



%! jsonld_triples_context(
%!   +Trips,
%!   -PredicateDefinitions:list(pair),
%!   -Context:dict
%! ) is det.

jsonld_triples_context(Trips, PDefs, Context):-
  rdf_triples_iris(Trips, Iris),
  % Prefixes.
  aggregate_all(
    set(Alias-Prefix),
    (
      member(Iri, Iris),
      rdf_iri_alias_prefix_local(Iri, Alias, Prefix, _)
    ),
    Abbrs
  ),

  % Predicate definitions.
  rdf_triples_predicates(Trips, Ps),
  p_defs(Ps, PDefs),

  % Pairs → dict.
  append(Abbrs, PDefs, Pairs),
  dict_pairs(Context, context, Pairs).


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



%! jsonld_ptree(+PDefinitions, +Tree, +P, -POs) is det.

jsonld_ptree(PDefs, Tree, P1, P2-O2):-
  abbreviate(P1, P2),
  get_assoc(P1, Tree, Os1),
  maplist(jsonld_oterm(PDefs, P2), Os1, Os2),
  (Os2 = [O2] -> true ; O2 = Os2).


%! jsonld_oterm(+PDefinitons, +P, +O, -JsonO) is det.

jsonld_oterm(_, _, O1, Elems2):-
  rdf_list(O1), !,
  rdf_list(O1, Elems1),
  maplist(abbreviate, Elems1, Elems2).
jsonld_oterm(PDefs, P2, O1, O2):-
  rdf_is_literal(O1), !,
  rdf_literal_data(lexical_form, O1, Lex),
  (   memberchk(P2-_, PDefs)
  ->  O2 = Lex
  ;   rdf_literal_data(datatype, O1, D1),
      abbreviate(D1, D2),
      O2 = literal{'@type':D2,'@value':Lex}
  ).
jsonld_oterm(_, _, O1, O2):-
  abbreviate(O1, O2).


%! abbreviate(+Iri, -Abbr) is det.

abbreviate(Iri, Abbr):-
  rdf_is_iri(Iri),
  rdf_iri_alias_prefix_local(Iri, Alias, _, Local), !,
  atomic_list_concat([Alias,Local], :, Abbr).
abbreviate(Iri, Iri).


%! p_defs(+Ps, -Pairs) is det.

p_defs([P1|Ps1], [P2-odef{'@container': '@list'}|Ps2]):-
  p_container(P1), !,
  abbreviate(P1, P2),
  p_defs(Ps1, Ps2).
p_defs([P1|Ps1], [P2-odef{'@type':D2}|Ps2]):-
  p_type(P1, D1), !,
  abbreviate(P1, P2),
  abbreviate(D1, D2),
  p_defs(Ps1, Ps2).
p_defs([_|T1], T2):- !,
  p_defs(T1, T2).
p_defs([], []).



%! p_container(+P) is semidet.

p_container(P):-
  rdf(_, P, O), rdf_list(O),
  forall(rdf(_, P, O), rdf_list(O)).



%! p_type(+P, -D) is semidet.

p_type(P, D):-
  rdf(_, P, O1), o_type(O1, D),
  forall(rdf(_, P, O), o_type(O, D)).


%! o_type(+O, -D) is det.

o_type(O, '@id'):- rdf_is_iri(O), !.
o_type(O, D):- rdf_literal_data(datatype, O, D).


rdf_triple_spair(rdf(S,P,O), S-(P-O)).
