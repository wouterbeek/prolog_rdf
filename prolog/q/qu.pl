:- module(
  qu,
  [
    qu_add_ltag/5,          % +M1, +M2, +P, +LTag, +G
    qu_call/2,              % :Find_0, :Transform_0
    qu_change_datatype/5,   % +M1, +M2, +P, +G, +D
    qu_change_iri/8,        % +M1, +M2, ?S, ?P, ?O, +G, +Positions, :Dcg_0
    qu_change_lex/5,        % +M1, +M2, +P, +G, :Dcg_0
    qu_change_ltag/5,       % +M1, +M2, +LTag1, +G, +LTag2
    qu_change_p/5,          % +M1, +M2, +P, +G, +Q
    qu_change_prefix/9,     % +M1, +M2, ?S, ?P, ?O, +G, +Positions, +Alias1, +Alias2
    qu_cp/4,                % +M1, +M2, +G1, +G2
    qu_cp/7,                % +M1, +M2, +G1, ?S, ?P, ?O, +G2
    qu_comb_date/7,         % +M1, +M2, +YP, +MP, +DP, +G, +Q
    qu_comb_month_day/7,    % +M1, +M2, +YP, +MP, +DP, +G, +Q
    qu_comb_year_month/7,   % +M1, +M2, +YP, +MP, +DP, +G, +Q
    qu_inc/5,               % +M1, +M2, +S, +P, +G
    qu_flatten/4,           % +M1, +M2, +P, +G
    qu_lex_padding/5,       % +M1, +M2, +P, +G, +PaddingCode
    qu_lex_to_iri/6,        % +M1, +M2, ?P, +Alias, +G, :Lex2Local_0
    qu_mv/4,                % +M1, +M2, +G1, +G2
    qu_mv/7,                % +M1, +M2, +G1, ?S, ?P, ?O, +G2
    qu_process_string/5,    % +M1, +M2, +P, +G, :Dcg_3
    qu_replace_string/7,    % +M1, +M2, ?S, ?P, +Sub1, +G, +Sub2
    qu_replace_predicate/5, % +M1, +M2, +P, +G, +Q
    qu_rm/6,                % +M1, +M2, ?S, ?P, ?O, +G
    qu_rm_cell/6,           % +M1, +M2, +S, +P, +O, +G
    qu_rm_col/4,            % +M1, +M2, +P, +G
    qu_rm_error/6,          % +M1, +M2, ?S, ?P, ?O, +G
    qu_rm_null/5,           % +M1, +M2, ?P, +Null, +G
    qu_rm_tree/4,           % +M1, +M2, +S, +G
    qu_rm_triples/6,        % +M1, +M2, ?S, ?P, ?O, ?G
    qu_split_string/5       % +M1, +M2, +P, +G, ?SepChars
  ]
).

/** <module> Quine update API

Higher-level update operations performed on RDF data.

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(aggregate)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(list_ext)).
:- use_module(library(print_ext)).
:- use_module(library(q/q__io)).
:- use_module(library(q/q_datatype)).
:- use_module(library(q/q_shape)).
:- use_module(library(q/q_stmt)).
:- use_module(library(q/q_term)).
:- use_module(library(q/qb)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(string_ext)).

:- meta_predicate
    qu_call(0, 0),
    qu_change_iri(+, +, ?, ?, ?, ?, +, //),
    qu_change_iri0(+, +, //, -),
    qu_change_lex(+, +, +, ?, //),
    qu_lex_to_iri(+, +, ?, +, ?, //),
    qu_process_string(+, +, +, ?, 5).

:- rdf_meta
   qu_add_ltag(+, +, r, +, r),
   qu_call(t, t),
   qu_change_datatype(+, +, r, r, r),
   qu_change_iri(+, +, r, r, o, r, +, :),
   qu_change_lex(+, +, r, r, :),
   qu_change_ltag(+, +, +, r, +),
   qu_change_p(+, +, r, r, r),
   qu_change_prefix(+, +, r, r, o, r, +, +, +),
   qu_comb_date(+, +, r, r, r, r, r),
   qu_comb_month_day(+, +, r, r, r, r, r),
   qu_comb_year_month(+, +, r, r, r, r, r),
   qu_cp(+, +, r, r),
   qu_cp(+, +, r, r, r, o, r),
   qu_flatten(+, +, r, r),
   qu_inc(+, +, r, r),
   qu_inc(+, +, r, r, +),
   qu_lex_padding(+, +, r, r, +),
   qu_lex_to_iri(+, +, r, +, r, :),
   qu_mv(+, +, r, r),
   qu_mv(+, +, r, r, r, o, r),
   qu_process_string(+, +, r, r, :),
   qu_replace_string(+, +, r, r, +, r, +),
   qu_replace_predicate(+, +, r, r, r),
   qu_rm(+, +, r, r, o, r),
   qu_rm_cell(+, +, r, r, o, r),
   qu_rm_col(+, +, r, r),
   qu_rm_error(+, +, r, r, o, r),
   qu_rm_null(+, +, r, o, r),
   qu_rm_tree(+, +, r, r),
   qu_rm_triples(+, +, r, r, o, r),
   qu_split_string(+, +, r, r, +).





%! qu_add_ltag(+M1, +M2, +P, +LTag, +G) is det.

qu_add_ltag(M1, M2, P, LTag, G) :-
  qu_call(
    q(M1, S, P, Str^^xsd:string, G),
    qu(M1, M2, S, P, Str^^xsd:string, G, object(Str@LTag))
  ).



%! qu_call(:Find_0, Transform_0) is det.

qu_call(Find_0, Transform_0) :-
  q_transaction(qu:qu_call0(Find_0, Transform_0, _{count: 0})).


qu_call0(Find_0, Transform_0, State) :-
  % NONDET
  Find_0,
  (Transform_0 -> true ; gtrace), %DEB
  dict_inc(count, State),
  fail.
qu_call0(_, _, State) :-
  ansi_format(
    user_output,
    [fg(yellow)],
    "~D updates were made.~n",
    [State.count]
  ).



%! qu_change_datatype(+M1, +M2, +P, +G, +D) is semidet.
%
% Change the datatype IRI of all object terms that appear with
% predicate P.

qu_change_datatype(M1, M2, P, G, D2) :-
  qu_call((
    q(M1, S, P, Lit1, G),
    q_literal(Lit1, D1, Lex, LTag),
    D1 \== D2
  ), (
    q_literal(Lit2, D2, Lex, LTag),
    qu(M1, M2, S, P, Lit1, G, object(Lit2))
  )).



%! qu_change_iri(+M1, +M2, ?S, ?P, ?O, +G, +Positions:list(oneof([+,-])), :Dcg_0) is det.

qu_change_iri(M1, M2, S1, P1, O1, G, Pos, Dcg_0) :-
  qu_call((
    q(M1, S1, P1, O1, G),
    qu_change_iri0(rdf(S1,P1,O1), Pos, Dcg_0, rdf(S2,P2,O2)),
    rdf(S1,P1,O1) \== rdf(S2,P2,O2)
  ), (
    qb_rm(M1, S1, P1, O1, G),
    qb(M2, S2, P2, O2, G)
  )).


qu_change_iri0(rdf(S1,P1,O1), [PosS,PosP,PosO], Dcg_0, rdf(S2,P2,O2)) :-
  (PosS == +, q_is_iri(S1) -> atom_phrase(Dcg_0, S1, S2) ; S2 = S1),
  (PosP == +, q_is_iri(P1) -> atom_phrase(Dcg_0, P1, P2) ; P2 = P1),
  (PosO == +, q_is_iri(O1) -> atom_phrase(Dcg_0, O1, O2) ; O2 = O1).



%! qu_change_lex(+M1, +M2, +P, +G, :Dcg_0) is det.
%
% Change the lexical form of the literals that appear with predicate
% P.  Dcg_0 is used to parse the old lexical form and generate the new
% one.

qu_change_lex(M1, M2, P, G, Dcg_0) :-
  qu_call((
    q(M1, S, P, Lit1, G),
    q_literal(Lit1, D, Lex1, LTag),
    string_phrase(Dcg_0, Lex1, Lex2),
    Lex1 \== Lex2,
    q_datatype_compat(Lex2, D)
  ), (
    q_literal(Lit2, D, Lex2, LTag),
    qu(M1, M2, S, P, Lit1, G, object(Lit2))
  )).



%! qu_change_ltag(+M1, +M2, +LTag1, +G, +LTag2) is det.

qu_change_ltag(M1, M2, LTag1, G, LTag2) :-
  qu_call(
    q(M1, S, P, Lex@LTag1, G),
    qu(M1, M2, S, P, Lex@LTag1, G, object(Lex@LTag2))
  ).



%! qu_change_p(+M1, +M2, +P, +G, +Q) is det.

qu_change_p(M1, M2, P, G, Q) :-
  qu_call((
    q(M1, S, P, O, G),
    P \== Q
  ), (
    qu(M1, M2, S, P, O, G, predicate(Q))
  )).



%! qu_change_prefix(M1, M2, ?S, ?P, ?O, +G, +Positions, +Alias1, +Alias2) is det.
%
% Positions is a length-3 list of elements `+` (considered) or `-`
% (not considered).

qu_change_prefix(M1, M2, S, P, O, G, Positions, Alias1, Alias2) :-
  qu_change_iri(M1, M2, S, P, O, G, Positions, iri_change_prefix0(Alias1, Alias2)).


iri_change_prefix0(Alias1, Alias2), atom(Prefix2), Cs -->
  {q_alias_prefix(Alias1, Prefix1)},
  atom(Prefix1),
  {q_alias_prefix(Alias2, Prefix2)},
  rest(Cs).



%! qu_comb_date(+M1, +M2, +YP, +MP, +DP, +G, +Q) is det.

qu_comb_date(M1, M2, YP, MP, DP, G, Q) :-
  qu_call((
    q(M1, S, YP, Y^^xsd:gYear, G),
    q(M1, S, MP, M^^xsd:gMonth, G),
    q(M1, S, DP, D^^xsd:gDay, G)
  ), (
    qb(M1, S, Q, date(Y,M,D)^^xsd:date, G),
    qb_rm(M2, S, YP, Y^^xsd:gYear, G),
    qb_rm(M2, S, MP, M^^xsd:gMonth, G),
    qb_rm(M2, S, DP, D^^xsd:gDay, G)
  )).



%! qu_comb_month_day(+M1, +M2, +MY, +MP, +DP, +G, +Q) is det.

qu_comb_month_day(M1, M2, MY, MP, DP, G, Q) :-
  qu_call((
    q(M1, S, MP, M^^xsd:gMonth, G),
    q(M1, S, DP, D^^xsd:gDay, G),
    \+ q(M1, S, MY, _, G)
  ), (
    qb(M2, S, Q, month_day(M,D)^^xsd:gMonthDay, G),
    qb_rm(M1, S, MP, M^^xsd:gMonth, G),
    qb_rm(M1, S, DP, D^^xsd:gDay, G)
  )).



%! qu_comb_year_month(+M1, +M2, +YP, +MP, +DP, +G, +Q) is det.

qu_comb_year_month(M1, M2, YP, MP, DP, G, Q) :-
  qu_call((
    q(M1, S, YP, Y^^xsd:gYear, G),
    q(M1, S, MP, M^^xsd:gMonth, G),
    \+ q(M1, S, DP, _, G)
  ), (
    qb(M2, S, Q, year_month(Y,M)^^xsd:gYearMonth, G),
    qb_rm(M1, S, YP, Y^^xsd:gYear, G),
    qb_rm(M1, S, MP, M^^xsd:gMonth, G)
  )).




%! qu_cp(+M1, +M2, +G1, +G2) is det.
%! qu_cp(+M1, +M2, +G1, ?S, ?P, ?O, +G2) is det.
%
% Copy a full graph or copy the specified triples between graphs.
%
% This asumes that blank nodes are shared between graphs.

qu_cp(M1, M2, G1, G2) :-
  qu_cp(M1, M2, G1, _, _, _, G2).


qu_cp(_, _, G, _, _, _, G) :- !.
qu_cp(M1, M2, G1, S, P, O, G2) :-
  q_transaction(
    forall(
      q(M1, S, P, O, G1),
      qb(M2, S, P, O, G2)
    )
  ).



%! qu_flatten(+M1, +M2, +P, +G) is det.

qu_flatten(M1, M2, P, G) :-
  qu_call((
    q(M1, X, P, Y, G),
    q_is_bnode(Y),
    q(M1, Y, Q, Z, G)
  ), (
    qu(M1, M2, Y, Q, Z, G, subject(X)),
    qb_rm(M1, X, P, Y, G)
  )).



%! qu_inc(+M1, +M2, +S, +P, +G) is det.
%
% Increment values.

qu_inc(M1, M2, S, P, G) :-
  qu_call((
    q(M1, S, P, N1^^D, G),
    rdf11:xsd_numerical(D, TypeCheck, integer)
  ), (
    N2 is N1 + 1,
    must_be(TypeCheck, N2),
    qu(M1, M2, S, P, N1^^D, G, object(N2^^D))
  )).



%! qu_lex_padding(+M1, +M2, +P, +G, +PaddingCode) is det.

qu_lex_padding(M1, M2, P, G, C) :-
  aggregate_all(
    max(Len),
    (
      q(M1, _, P, Lit, G),
      q_literal_lex(Lit, Lex),
      atom_length(Lex, Len)
    ),
    Max
  ),
  qu_change_lex(M1, M2, P, G, qu_lex_padding0(C, Max)).


qu_lex_padding0(C, Len), Cs -->
  ...(Suffix),
  eos,
  {
    length(Suffix, SuffixLen),
    PrefixLen is Len - SuffixLen,
    repeating_list(C, PrefixLen, Prefix),
    append(Prefix, Suffix, Cs)
  }.



%! qu_lex_to_iri(M1, M2, ?P, +Alias, +G, :Lex2Local_0) is det.

qu_lex_to_iri(M1, M2, P, Alias, G, Lex2Local_0) :-
  qu_call((
    q(M1, S, P, Lit, G),
    q_is_literal(Lit)
  ), (
    q_literal_lex(Lit, Lex),
    string_codes(Lex, Cs1),
    phrase(Lex2Local_0, Cs1, Cs2),
    atom_codes(Local, Cs2),
    rdf_global_id(Alias:Local, O),
    qu(M1, M2, S, P, Lit, G, object(O))
  )).



%! qu_mv(+M1, +M2, +G1, +G2) is det.
%! qu_mv(+M1, +M2, +G1, ?S, ?P, ?O, +G2) is det.
%
% Rename a graph or move the specified triples between graphs.

qu_mv(M1, M2, G1, G2) :-
  qu_mv(M1, M2, G1, _, _, _, G2).


qu_mv(M1, M2, G1, S, P, O, G2) :-
  qu_call(
    q(M1, S, P, O, G1),
    qu(M1, M2, S, P, O, G1, graph(G2))
  ).



%! qu_process_string(+M1, +M2, +P, +G, :Dcg_3) is det.

qu_process_string(M1, M2, P, G, Dcg_3) :-
  qu_call((
    q(M1, S, P, Lex^^xsd:string, G)
  ), (
    string_phrase(dcg_call(Dcg_3, M2, S, G), Lex),
    qb_rm(M1, S, P, Lex^^xsd:string, G)
  )).



%! qu_replace_predicate(+M1, +M2, +P, +G, +Q) is det.

qu_replace_predicate(M1, M2, P, G, Q) :-
  qu_call(
    q(M1, S, P, O, G),
    qu(M1, M2, S, P, O, G, predicate(Q))
  ).



%! qu_replace_string(+M1, +M2, ?S, ?P, +Sub1, +G, +Sub2) is det.

qu_replace_string(M1, M2, S, P, Sub1, G, Sub2) :-
  qu_call((
    q(M1, S, P, Str1^^xsd:string, G),
    string_replace(Str1, Sub1, Sub2, Str2),
    Str1 \== Str2
  ), (
    qu(M1, M2, S, P, Str1^^xsd:string, G, object(Str2^^xsd:string))
  )).



%! qu_rm(+M1, +M2, ?S, ?P, ?O, +G) is det.
%
% Remove the specified triples or quadruples.

qu_rm(M1, M2, S, P, O, G) :-
  qu_call(
    q(M1, S, P, O, G),
    qb_rm(M2, S, P, O, G)
  ).



%! qu_rm_cell(+M1, +M2, +S, +P, +O, +G) is det.
%
% Remove a specific triple or quadruple.

qu_rm_cell(M1, M2, S, P, O, G) :-
  qu_rm(M1, M2, S, P, O, G).



%! qu_rm_col(+M1, +M2, +P, +G) is det.
%
% Remove all triples that contain predicate P.

qu_rm_col(M1, M2, P, G) :-
  qu_rm(M1, M2, _, P, _, G).



%! qu_rm_error(M1, M2, ?S, ?P, ?O, +G) is det.
%
% Wrapper around qu_rm/4 that indicates that an error or mistake is
% being removed.

qu_rm_error(M1, M2, S, P, O, G) :-
  qu_rm(M1, M2, S, P, O, G).



%! qu_rm_null(+M1, +M2, ?P, +Null, +G) is det.
%
% Wrapper around qu_rm/4 that indicates that an error or mistake is
% being removed.

qu_rm_null(M1, M2, P, Null, G) :-
  qu_rm(M1, M2, _, P, Null, G).



%! qu_rm_tree(+M1, +M2, +S, +G) is det.

qu_rm_tree(M1, M2, S, G) :-
  q_tree(M1, S, G, Tree),
  maplist({M2,G}/[Triple]>>qb_rm(M2, Triple, G), Tree).



%! qu_rm_triples(+M1, +M2, ?S, ?P, ?O, +G) is det.

qu_rm_triples(M1, M2, S, P, O, G) :-
  qu_call(
    q_triple(M1, S, P, O, G, Triple),
    qb_rm(M2, Triple, G)
  ).



%! qu_split_string(+M1, +M2, +P, +G, +SepChars) is det.

qu_split_string(M1, M2, P, G, SepChars) :-
  qu_call((
    q(M1, S, P, Str0^^xsd:string, G),
    split_string(Str0, SepChars, "", Strs)
  ), (
    forall(member(Str, Strs), qb(M2, S, P, Str^^xsd:string)),
    qb_rm(M2, S, P, Str0^^xsd:string, G)
  )).
