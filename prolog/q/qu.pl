:- module(
  qu,
  [
    q_transform/2,              % +G, :Goal_3
% TERM
    qu_replace_subject/5,       % +M1, +M2, +S1, ?G, +S2
    qu_subject_from_key/5,      % +M1, +M2, +Alias, +P, ?G
  % IRI
    qu_change_iri/8,            % +M1, +M2, ?S, ?P, ?O, ?G, +Pos, :Dcg_0
      qu_change_iri_prefix/9,   % +M1, +M2, ?S, ?P, ?O, ?G, +Pos, +Alias1, +Alias2
      qu_lowercase_predicate/4, % +M1, +M2, +Alias, ?G
      qu_replace_predicate/5,   % +M1, +M2, +P1, ?G, +P2

  % LITERAL
    qu_replace_string/5,        % +M1, +M2, ?P, ?G, :Dcg_3
    qu_replace_string/6,        % +M1, +M2, ?S, ?P, ?G, :Dcg_3
    qu_replace_substring/7,     % +M1, +M2, ?S, ?P, +SubStr1, ?G, +SubStr2
      % DATATYPE IRI
      qu_change_datatype/5,     % +M1, +M2, ?P, ?G, +D
      % LANGUAGE TAG
      qu_add_ltag/5,            % +M1, +M2, ?P, +LTag, ?G
      qu_change_ltag/6,         % +M1, +M2, ?P, +LTag1, ?G, +LTag2
      % LEXICAL FORM
      qu_change_lex/5,          % +M1, +M2, ?P, ?G, :Dcg_0
        qu_add_padding/5,       % +M1, +M2, +P, ?G, +PaddingCode
      % VALUE
      qu_change_val/7,          % +M1, +M2, ?S, ?P, ?DParent, ?G, :Goal_2
        qu_change_decimal/6,    % +M1, +M2, ?S, ?P, ?G, :Goal_2
          qu_dec/5,             % +M1, +M2, +S, +P, ?G
          qu_inc/5,             % +M1, +M2, +S, +P, ?G
        qu_change_double/6,     % +M1, +M2, ?S, ?P, ?G, :Goal_2
        qu_change_float/6,      % +M1, +M2, ?S, ?P, ?G, :Goal_2
% TERM → TERM
  qu_lex_to_iri/5,              % +M1, +M2, ?P, +Alias, ?G
  qu_lex_to_iri/6,              % +M1, +M2, ?P, +Alias, ?G, :Lex2Local_0
% STATEMENTS
  % COMBINE
    qu_comb_date/7,             % +M1, +M2, +P1, +P2, +P3, ?G, +Q
    qu_comb_month_day/7,        % +M1, +M2, +P1, +P2, +P3, ?G, +Q
    qu_comb_year_month/7,       % +M1, +M2, +P1, +P2, +P3, ?G, +Q
  % CP
    qu_cp/4,                    % +M1, +M2, +G1, +G2
    qu_cp/7,                    % +M1, +M2, +G1, ?S, ?P, ?O, +G2
  % FLATTEN
    qu_flatten_bnode/4,         % +M1, +M2, +P, ?G
  % MV
    qu_mv/4,                    % +M1, +M2, +G1, +G2
    qu_mv/7,                    % +M1, +M2, +G1, ?S, ?P, ?O, +G2
  % NEST
    qu_nest_bnode/5,            % +M1, +M2, ?G, +P, +Qs
  % RM
    qu_rm/6,                    % +M1, +M2, ?S, ?P, ?O, ?G
    qu_rm_cell/6,               % +M1, +M2, +S, +P, +O, ?G
    qu_rm_col/4,                % +M1, +M2, +P, ?G
    qu_rm_empty_string/3,       % +M1, +M2, ?G
    qu_rm_empty_string/4,       % +M1, +M2, ?P, ?G
    qu_rm_error/6,              % +M1, +M2, ?S, ?P, ?O, ?G
    qu_rm_null/5,               % +M1, +M2, ?P, +Null, ?G
    qu_rm_tree/4,               % +M1, +M2, +S, ?G
    qu_rm_triples/6,            % +M1, +M2, ?S, ?P, ?O, ?G
  % SPLIT
    qu_split_string/5,          % +M1, +M2, +P, ?G, +SepChars
% GEO
    qu_replace_flat_wgs84_point/3,    % +M1, +M2, ?G
    qu_replace_flat_wgs84_point/5,    % +M1, +M2, +Q, +R, ?G
    qu_replace_flat_wkt_geometry/3,   % +M1, +M2, +G
    qu_replace_flat_wkt_geometry/5,   % +M1, +M2, +Q, +R, +G
    qu_replace_nested_wgs84_point/3,  % +M1, +M2, ?G
    qu_replace_nested_wgs84_point/6,  % +M1, +M2, +P, +Q, +R, ?G
    qu_replace_nested_wkt_geometry/3, % +M1, +M2, +G
    qu_replace_nested_wkt_geometry/6  % +M1, +M2, +P, +Q, +R, +G
  ]
).

/** <module> Quine update API

Higher-level update operations performed on RDF data.

Most transformation predicates allow their applicability to be scoped
to predicate and/or graph.

@author Wouter Beek
@version 2016/06-2016/07
*/

:- use_module(library(aggregate)).
:- use_module(library(atom_ext)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(list_ext)).
:- use_module(library(print_ext)).
:- use_module(library(q/q_datatype)).
:- use_module(library(q/q_io)).
:- use_module(library(q/q_print)).
:- use_module(library(q/q_shape)).
:- use_module(library(q/q_stmt)).
:- use_module(library(q/q_term)).
:- use_module(library(q/q_wgs84)).
:- use_module(library(q/q_wkt)).
:- use_module(library(q/qb)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(string_ext)).

:- meta_predicate
    q_transform(+, 3),
    qu_call(0, 0),
    qu_call0(0, 0, +),
    qu_change_decimal(+, +, ?, ?, ?, 2),
    qu_change_double(+, +, ?, ?, ?, 2),
    qu_change_float(+, +, ?, ?, ?, 2),
    qu_change_iri(+, +, ?, ?, ?, ?, +, //),
    qu_change_iri0(+, +, //, -),
    qu_change_lex(+, +, ?, ?, //),
    qu_change_val(+, +, ?, ?, ?, ?, 2),
    qu_lex_to_iri(+, +, ?, +, ?, //),
    qu_replace_string(+, +, ?, ?, 5),
    qu_replace_string(+, +, ?, ?, ?, 5).

:- qb_alias(geold, 'http://geojsonld.com/vocab#').
:- qb_alias(wgs84, 'http://www.w3.org/2003/01/geo/wgs84_pos#').
:- qb_alias(wkt, 'http://geojsonld.com/wkt#').

:- rdf_meta
   q_transform(+, t),
   qu_add_ltag(+, +, r, +, r),
   qu_add_padding(+, +, r, r, +),
   qu_call(t, t),
   qu_change_datatype(+, +, r, r, r),
   qu_change_decimal(+, +, r, r, r, :),
   qu_change_double(+, +, r, r, r, :),
   qu_change_float(+, +, r, r, r, :),
   qu_change_iri(+, +, r, r, o, r, +, :),
   qu_change_iri_prefix(+, +, r, r, o, r, +, +, +),
   qu_change_lex(+, +, r, r, :),
   qu_change_ltag(+, +, r, +, r, +),
   qu_change_val(+, +, r, r, r, r, :),
   qu_comb_date(+, +, r, r, r, r, r),
   qu_comb_month_day(+, +, r, r, r, r, r),
   qu_comb_year_month(+, +, r, r, r, r, r),
   qu_cp(+, +, r, r),
   qu_cp(+, +, r, r, r, o, r),
   qu_dec(+, +, r, r, r),
   qu_flatten_bnode(+, +, r, r),
   qu_inc(+, +, r, r, r),
   qu_lex_to_iri(+, +, r, +, r),
   qu_lex_to_iri(+, +, r, +, r, :),
   qu_lowercase_predicate(+, +, +, r),
   qu_mv(+, +, r, r),
   qu_mv(+, +, r, r, r, o, r),
   qu_nest_bnode(+, +, r, r, t),
   qu_replace_flat_wgs84_point(+, +, r),
   qu_replace_flat_wgs84_point(+, +, r, r, r),
   qu_replace_flat_wkt_point(+, +, r),
   qu_replace_flat_wkt_point(+, +, r, r, r),
   qu_replace_nested_wgs84_point(+, +, r),
   qu_replace_nested_wgs84_point(+, +, r, r, r, r),
   qu_replace_nested_wkt_point(+, +, r),
   qu_replace_nested_wkt_point(+, +, r, r, r, r),
   qu_replace_predicate(+, +, r, r, r),
   qu_replace_string(+, +, r, r, :),
   qu_replace_string(+, +, r, r, r, :),
   qu_replace_subject(+, +, r, r, r),
   qu_replace_substring(+, +, r, r, +, r, +),
   qu_rm(+, +, r, r, o, r),
   qu_rm_cell(+, +, r, r, o, r),
   qu_rm_col(+, +, r, r),
   qu_rm_empty_string(+, +, r),
   qu_rm_empty_string(+, +, r, r),
   qu_rm_error(+, +, r, r, o, r),
   qu_rm_null(+, +, r, o, r),
   qu_rm_tree(+, +, r, r),
   qu_rm_triples(+, +, r, r, o, r),
   qu_split_string(+, +, r, r, +),
   qu_subject_from_key(+, +, +, r, r).





% GENERICS %

%! q_transform(+G, :Goal_3) is det.

q_transform(G, Goal_3) :-
  q_load(rdf, G),
  call(Goal_3, rdf, rdf, G),
  q_save(rdf, G),
  q_unload(rdf, G).





% TERM

%! qu_replace_subject(+M1, +M2, +S1, ?G, +S2) is det.

qu_replace_subject(M1, M2, S1, G, S2) :-
  qu_call(
    q(M1, S1, P, O, G),
    qu(M1, M2, S1, P, O, G, subject(S2))
  ).



%! qu_subject_from_key(+M1, +M2, +Alias, +P, ?G) is det.
%
% Use the string value of predicate P to generate the subject terms.

qu_subject_from_key(M1, M2, Alias, P, G) :-
  qu_call(
    q(M1, S1, P, Str^^xsd:string, G),
    (
      lowercase_string(Str, Local),
      rdf_global_id(Alias:Local, S2),
      qb_rm(M1, S1, P, Str^^xsd:string, G),
      forall(
        q(M1, S1, P, O, G),
        qu(M1, M2, S1, P, O, G, subject(S2))
      )
    )
  ).



% TERM > IRI

%! qu_change_iri(+M1, +M2, ?S, ?P, ?O, ?G, +Pos, :Dcg_0) is det.
%
% Change IRIs with Dcg_0.
%
% Pos is of type `list(oneof([+,-]))`:
%
%   - ‘+’ indicates that IRIs in the corresponding position must be
%     changed.
%
%   - ‘-’ indicates that IRIs in the corresponding position must not
%     be changed.

qu_change_iri(M1, M2, S1, P1, O1, G, [PosS,PosP,PosO], Dcg_0) :-
  qu_call(
    (
      q(M1, S1, P1, O1, G),
      qu_change_iri0(S1, PosS, Dcg_0, S2),
      qu_change_iri0(P1, PosP, Dcg_0, P2),
      qu_change_iri0(O1, PosO, Dcg_0, O2),
      rdf(S1,P1,O1) \== rdf(S2,P2,O2)
    ), (
      qb_rm(M1, S1, P1, O1, G),
      qb(M2, S2, P2, O2, G)
    )
  ).


qu_change_iri0(Term0, Pos, Dcg_0, Term) :-
  Pos == +,
  q_is_iri(Term0), !,
  atom_phrase(Dcg_0, Term0, Term).
qu_change_iri0(Term, _, _, Term).



%! qu_change_iri_prefix(M1, M2, ?S, ?P, ?O, ?G, +Pos, +Alias1, +Alias2) is det.
%
% Pos is a length-3 list of elements `+` (considered) or `-`
% (not considered).

qu_change_iri_prefix(M1, M2, S, P, O, G, Pos, Alias1, Alias2) :-
  qu_change_iri(M1, M2, S, P, O, G, Pos, qu_change_iri_prefix0(Alias1, Alias2)).


qu_change_iri_prefix0(Alias1, Alias2), atom(Prefix2), Cs -->
  {q_alias_prefix(Alias1, Prefix1)},
  atom(Prefix1),
  {q_alias_prefix(Alias2, Prefix2)},
  rest(Cs).



%! qu_lowercase_predicate(+M1, +M2, +Alias, ?G) is det.

qu_lowercase_predicate(M1, M2, Alias, G) :-
  qu_lowercase_predicate_deb(Alias),
  qu_change_iri(M1, M2, _, _, _, G, [-,+,-], qu_lowercase_iri0(Alias)).


qu_lowercase_iri0(Alias), Cs -->
  {q_alias_prefix(Alias, Prefix)},
  atom(Prefix),
  lowercase,
  {atom_codes(Prefix, Cs)}.



%! qu_replace_predicate(+M1, +M2, +P1, ?G, +P2) is det.

qu_replace_predicate(M1, M2, P1, G, P2) :-
  qu_replace_predicate_deb(P1, P2),
  qu_call(
    (
      q(M1, S, P1, O, G),
      P1 \== P2
    ),
    qu(M1, M2, S, P1, O, G, predicate(P2))
  ).



% TERM > LITERAL

%! qu_replace_string(+M1, +M2, ?P, ?G, :Dcg_3) is det.
%! qu_replace_string(+M1, +M2, ?S, ?P, ?G, :Dcg_3) is det.

qu_replace_string(M1, M2, P, G, Dcg_3) :-
  qu_replace_string(M1, M2, _, P, G, Dcg_3).


qu_replace_string(M1, M2, S, P, G, Dcg_3) :-
  qu_call(
    q(M1, S, P, Lex^^xsd:string, G),
    (
      string_phrase(dcg_call(Dcg_3, M2, S, G), Lex),
      qb_rm(M1, S, P, Lex^^xsd:string, G)
    )
  ).



%! qu_replace_substring(+M1, +M2, ?S, ?P, +SubStr1, ?G, +SubStr2) is det.

qu_replace_substring(M1, M2, S, P, SubStr1, G, SubStr2) :-
  qu_call(
    (
      q(M1, S, P, Str1^^xsd:string, G),
      string_replace(Str1, SubStr1, SubStr2, Str2),
      Str1 \== Str2
    ),
    qu(M1, M2, S, P, Str1^^xsd:string, G, object(Str2^^xsd:string))
  ).



% TERM > LITERAL > DATATYPE IRI

%! qu_change_datatype(+M1, +M2, ?P, ?G, +D) is semidet.
%
% Change the datatype IRI of literals to D.

qu_change_datatype(M1, M2, P, G, D) :-
  qu_change_datatype_deb(P, D),
  qu_call(
    (
      q(M1, S, P, Lit1, G),
      q_literal(Lit1, D0, Lex, LTag),
      D0 \== D
    ), (
      q_literal(Lit2, D, Lex, LTag),
      qu(M1, M2, S, P, Lit1, G, object(Lit2))
    )
  ).



% TERM > LITERAL > LANGUAGE TAG

%! qu_add_ltag(+M1, +M2, ?P, +LTag, ?G) is det.
%
% Changes strings (‘xsd:string’) to language-tagged strings
% (‘rdf:langString’).

qu_add_ltag(M1, M2, P, LTag, G) :-
  qu_add_ltag_deb(P, LTag),
  qu_call(
    q(M1, S, P, Str^^xsd:string, G),
    qu(M1, M2, S, P, Str^^xsd:string, G, object(Str@LTag))
  ).



%! qu_change_ltag(+M1, +M2, ?P, +LTag1, ?G, +LTag2) is det.
%
% Change languages tags LTag1 → LTag2.

qu_change_ltag(M1, M2, P, LTag1, G, LTag2) :-
  qu_call(
    q(M1, S, P, Lex@LTag1, G),
    qu(M1, M2, S, P, Lex@LTag1, G, object(Lex@LTag2))
  ).



% TERM > LITERAL > LEXICAL FORM

%! qu_change_lex(+M1, +M2, ?P, ?G, :Dcg_0) is det.
%
% Change lexical forms by calling Dcg_0.

qu_change_lex(M1, M2, P, G, Dcg_0) :-
  qu_change_lex_deb(P, Dcg_0),
  qu_call(
    (
      q(M1, S, P, Lit1, G),
      q_literal(Lit1, D, Lex1, LTag),
      string_phrase(Dcg_0, Lex1, Lex2),
      Lex1 \== Lex2,
      q_datatype_compat(Lex2, D)
    ), (
      q_literal(Lit2, D, Lex2, LTag),
      qu(M1, M2, S, P, Lit1, G, object(Lit2))
    )
  ).



%! qu_add_padding(+M1, +M2, +P, ?G, +PaddingCode) is det.

qu_add_padding(M1, M2, P, G, PaddingCode) :-
  qu_add_padding_deb(P, PaddingCode),
  aggregate_all(
    max(Len),
    (
      q(M1, _, P, Lit, G),
      q_literal_lex(Lit, Lex),
      atom_length(Lex, Len)
    ),
    Max
  ),
  qu_change_lex(M1, M2, P, G, qu_add_padding0(PaddingCode, Max)).


qu_add_padding0(PaddingCode, Len), Cs -->
  ...(Suffix),
  eos,
  {
    length(Suffix, SuffixLen),
    PrefixLen is Len - SuffixLen,
    repeating_list(PaddingCode, PrefixLen, Prefix),
    append(Prefix, Suffix, Cs)
  }.



% TERM > LITERAL > VALUE

%! qu_change_val(+M1, +M2, ?S, ?P, ?DParent, ?G, :Goal_2) is det.
%
% Change literal values.  The following call is made: `call(Goal_2,
% Val1, Val2)`.

qu_change_val(M1, M2, S, P, DParent, G, Goal_2) :-
  qu_call(
    (
      q(M1, S, P, Val1^^D, G),
      (var(DParent) -> true ; q_subdatatype_of(D, DParent))
    ),
    (
      call(Goal_2, Val1, Val2),
      rdf_typecheck(D, Val2),
      qu(M1, M2, S, P, Val1^^D, G, object(Val2^^D))
    )
  ).



% TERM > LITERAL > VALUE > DECIMAL

%! qu_change_decimal(+M1, +M2, ?S, ?P, ?G, :Goal_2) is det.

qu_change_decimal(M1, M2, S, P, G, Goal_2) :-
  qu_change_val(M1, M2, S, P, xsd:decimal, G, Goal_2).



% TERM > LITERAL > VALUE > DECIMAL > DECREMENT

%! qu_dec(+M1, +M2, ?S, ?P, ?G) is det.
%
% Decrement values.

qu_dec(M1, M2, S, P, G) :-
  qu_change_decimal(M1, M2, S, P, G, pred).



% TERM > LITERAL > VALUE > DECIMAL > INCREMENT

%! qu_inc(+M1, +M2, ?S, ?P, ?G) is det.
%
% Increment values.

qu_inc(M1, M2, S, P, G) :-
  qu_change_decimal(M1, M2, S, P, G, succ).



% TERM > LITERAL > VALUE > DOUBLE

%! qu_change_double(+M1, +M2, ?S, ?P, ?G, :Goal_2) is det.

qu_change_double(M1, M2, S, P, G, Goal_2) :-
  qu_change_val(M1, M2, S, P, xsd:double, G, Goal_2).



% TERM > LITERAL > VALUE > FLOAT

%! qu_change_float(+M1, +M2, ?S, ?P, ?G, :Goal_2) is det.

qu_change_float(M1, M2, S, P, G, Goal_2) :-
  qu_change_val(M1, M2, S, P, xsd:float, G, Goal_2).



% TERM → TERM

%! qu_lex_to_iri(M1, M2, ?P, +Alias, ?G) is det.
%! qu_lex_to_iri(M1, M2, ?P, +Alias, ?G, :Lex2Local_0) is det.

qu_lex_to_iri(M1, M2, P, Alias, G) :-
  qu_lex_to_iri(M1, M2, P, Alias, G, rest).


qu_lex_to_iri(M1, M2, P, Alias, G, Lex2Local_0) :-
  qu_lex_to_iri_deb(P, Alias, Lex2Local_0),
  qu_call(
    (
      q(M1, S, P, Lit, G),
      q_is_literal(Lit)
    ), (
      q_literal_lex(Lit, Lex),
      string_codes(Lex, Cs1),
      phrase(Lex2Local_0, Cs1, Cs2),
      atom_codes(Local, Cs2),
      rdf_global_id(Alias:Local, O),
      qu(M1, M2, S, P, Lit, G, object(O))
    )
  ).



% STATEMENTS

% STATEMENTS > COMBINE

%! qu_comb_date(+M1, +M2, +P1, +P2, +P3, ?G, +Q) is det.
%
% From:
%
% ```pseudo-rdf
% s p1 x^^xsd:gYear  g
%   p2 y^^xsd:gMonth g
%   p3 z^^xsd:gDay   g
% ```
%
% To:
%
% ```pseudo-rdf
% s q f(x,y,z)^^xsd:date g
% ```

qu_comb_date(M1, M2, P1, P2, P3, G, Q) :-
  qu_comb_date_deb,
  qu_call(
    (
      q(M1, S, P1, Y^^xsd:gYear, G),
      q(M1, S, P2, M^^xsd:gMonth, G),
      q(M1, S, P3, D^^xsd:gDay, G)
    ), (
      qb(M1, S, Q, date(Y,M,D)^^xsd:date, G),
      qb_rm(M2, S, P1, Y^^xsd:gYear, G),
      qb_rm(M2, S, P2, M^^xsd:gMonth, G),
      qb_rm(M2, S, P3, D^^xsd:gDay, G)
    )
  ).



%! qu_comb_month_day(+M1, +M2, +P1, +P2, +P3, ?G, +Q) is det.
%
% From:
%
% ```pseudo-rdf
% s  p2 y^^xsd:gMonth g
%    p3 z^^xsd:gDay   g
% \+ p1 x^^xsd:gYear  g
% ```
%
% To:
%
% ```pseudo-rdf
% s q f(y,z)^^xsd:date g
% ```

qu_comb_month_day(M1, M2, P1, P2, P3, G, Q) :-
  qu_comb_month_day_deb,
  qu_call(
    (
      q(M1, S, P2, M^^xsd:gMonth, G),
      q(M1, S, P3, D^^xsd:gDay, G),
      \+ q(M1, S, P1, _, G)
    ), (
      qb(M2, S, Q, month_day(M,D)^^xsd:gMonthDay, G),
      qb_rm(M1, S, P2, M^^xsd:gMonth, G),
      qb_rm(M1, S, P3, D^^xsd:gDay, G)
    )
  ).



%! qu_comb_year_month(+M1, +M2, +P1, +P2, +P3, ?G, +Q) is det.

qu_comb_year_month(M1, M2, P1, P2, P3, G, Q) :-
  qu_comb_year_month_deb,
  qu_call(
    (
      q(M1, S, P1, Y^^xsd:gYear, G),
      q(M1, S, P2, M^^xsd:gMonth, G),
      \+ q(M1, S, P3, _, G)
    ), (
      qb(M2, S, Q, year_month(Y,M)^^xsd:gYearMonth, G),
      qb_rm(M1, S, P1, Y^^xsd:gYear, G),
      qb_rm(M1, S, P2, M^^xsd:gMonth, G)
    )
  ).



% STATEMENTS > CP

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
  qu_call(
    q(M1, S, P, O, G1),
    qb(M2, S, P, O, G2)
  ).



% STATEMENTS > FLATTEN

%! qu_flatten_bnode(+M1, +M2, +P, ?G) is det.
%
% Remove triples 〈S,P,B〉 and 〈B,Q,O〉 from graph G and assert
% triples 〈S,Q,O〉 instead.

qu_flatten_bnode(M1, M2, P, G) :-
  qu_call(
    (
      q(M1, S, P, B, G),
      q_is_bnode(B),
      q(M1, B, Q, O, G)
    ), (
      qu(M1, M2, B, Q, O, G, subject(S)),
      qb_rm(M1, S, P, B, G)
    )
  ).



% STATEMENTS > MV

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



% STATEMENTS > NESTING

%! qu_nest_bnode(+M1, +M2, ?G, +P, +Qs) is det.
%
% Inserts a blank node in front of the relations Qs.
%
% From:
%
%   ```pseudo-rdf
%   s q1 o1
%   ...
%   s qn on
%   ```
%
% To:
%
%   ```pseudo-rdf
%   s p  b
%   b q1 o1
%   ...
%   b qn on
%   ```

qu_nest_bnode(M1, M2, G, P, [Q|Qs]) :-
  qu_call(
    (
      q(M1, S, Q, O, G),
      maplist({M1,S,G}/[Q0,O0]>>q(M1, S, Q0, O0, G), Qs, Os)
    ), (
      qb_bnode(B),
      qb(M2, S, P, B, G),
      maplist(
        {M1,M2,S,G,B}/[Q0,O0]>>qu(M1, M2, S, Q0, O0, G, subject(B)),
        [Q|Qs],
        [O|Os]
      )
    )
  ).



% STATEMENTS > RM

%! qu_rm(+M1, +M2, ?S, ?P, ?O, ?G) is det.
%
% Remove the specified triples or quadruples.

qu_rm(M1, M2, S, P, O, G) :-
  qu_call(
    q(M1, S, P, O, G),
    qb_rm(M2, S, P, O, G)
  ).



%! qu_rm_cell(+M1, +M2, +S, +P, +O, ?G) is det.
%
% Remove a specific triple or quadruple.

qu_rm_cell(M1, M2, S, P, O, G) :-
  qu_rm(M1, M2, S, P, O, G).



%! qu_rm_col(+M1, +M2, +P, ?G) is det.
%
% Remove all triples that contain predicate P.

qu_rm_col(M1, M2, P, G) :-
  qu_rm_col_deb(P),
  qu_rm(M1, M2, _, P, _, G).



%! qu_rm_empty_string(+M1, +M2, ?G) is det.
%! qu_rm_empty_string(+M1, +M2, ?P, ?G) is det.

qu_rm_empty_string(M1, M2, G) :-
  qu_rm_empty_string(M1, M2, _, G).


qu_rm_empty_string(M1, M2, P, G) :-
  qu_rm_empty_string_deb(P),
  qu_rm_null(M1, M2, P, ""^^xsd:string, G).



%! qu_rm_error(+M1, +M2, ?S, ?P, ?O, ?G) is det.
%
% Wrapper around qu_rm/4 that indicates that an error or mistake is
% being removed.

qu_rm_error(M1, M2, S, P, O, G) :-
  qu_rm_error_deb(S, P, O),
  qu_rm(M1, M2, S, P, O, G).



%! qu_rm_null(+M1, +M2, ?P, +Null, ?G) is det.
%
% Wrapper around qu_rm/4 that indicates that an error or mistake is
% being removed.

qu_rm_null(M1, M2, P, Null, G) :-
  qu_rm_null_deb(P, Null),
  qu_rm(M1, M2, _, P, Null, G).



%! qu_rm_tree(+M1, +M2, +S, ?G) is det.

qu_rm_tree(M1, M2, S, G) :-
  forall(
    q_tree_triple(M1, S, G, Triple),
    qb_rm(M2, Triple, G)
  ).



%! qu_rm_triples(+M1, +M2, ?S, ?P, ?O, ?G) is det.

qu_rm_triples(M1, M2, S, P, O, G) :-
  qu_call(
    q_triple(M1, S, P, O, G, Triple),
    qb_rm(M2, Triple, G)
  ).



% STATEMENTS > SPLIT

%! qu_split_string(+M1, +M2, +P, ?G, +SepChars) is det.
%
% From:
%
% ```pseudo-rdf
% s p lex^^xsd:string
% ```
%
% To:
%
% ```pseudo-rdf
% [lex1,...,lexn] = f(lex,seps)
%
% s p lex1^^xsd:string
%     ...
%     lexn^^xsd:string
% ```

qu_split_string(M1, M2, P, G, SepChars) :-
  qu_split_string_deb(P),
  qu_call(
    (
      q(M1, S, P, Str0^^xsd:string, G),
      split_string(Str0, SepChars, " ", Strs),
      Strs \= [_]
    ), (
      forall(
        member(Str, Strs),
        qb(M2, S, P, Str^^xsd:string, G)
      ),
      qb_rm(M2, S, P, Str0^^xsd:string, G)
    )
  ).



% GEO

%! qu_replace_flat_wgs84_point(+M1, +M2, ?G) is det.
%! qu_replace_flat_wgs84_point(+M1, +M2, +P, +Q, ?G) is det.
%
% From flat WGS84 point notation to Triply point notation.

qu_replace_flat_wgs84_point(M1, M2, G) :-
  qu_replace_flat_wgs84_point(M1, M2, wgs84:long, wgs84:lat, G).


qu_replace_flat_wgs84_point(M1, M2, P, Q, G) :-
  debug(qu(wgs84), "Triplyfy flat WGS84 point notation.", []),
  qu_call(
    (
      q(M1, S, P, Lng^^xsd:float, G),
      q(M1, S, Q, Lat^^xsd:float, G)
    ), (
      qb_wkt_point(M2, S, point(Lng,Lat), G),
      qb_rm(M1, S, P, Lng^^xsd:float, G),
      qb_rm(M1, S, Q, Lat^^xsd:float, G)
    )
  ).



%! qu_replace_flat_wkt_geometry(+M1, +M2, ?G) is det.
%! qu_replace_flat_wkt_geometry(+M1, +M2, +P, +Q, ?G) is det.

qu_replace_flat_wkt_geometry(M1, M2, G) :-
  qu_replace_flat_wkt_geometry(M1, M2, rdf:type, geold:coordinates, G).


qu_replace_flat_wkt_geometry(M1, M2, P, Q, G) :-
  debug(qu(wkt), "Triplyfy flat WKT geometry notation.", []),
  qu_call(
    (
      q(M1, S, P, C, G),
      q(M1, S, Q, Array^^D0, G)
    ), (
      q_iri_local(C, Local0),
      lowercase_atom(Local0, Local),
      rdf_global_id(wkt:Local, D),
      qb(M2, S, geold:geometry, Array^^D, G),
      qb_rm(M1, S, P, C, G),
      qb_rm(M1, S, Q, Array^^D0, G)
    )
  ).



%! qu_replace_nested_wkt_geometry(+M1, +M2, ?G) is det.
%! qu_replace_nested_wkt_geometry(+M1, +M2, +P, +Q, +R, ?G) is det.

qu_replace_nested_wkt_geometry(M1, M2, G) :-
  qu_replace_nested_wkt_geometry(M1, M2, geold:geometry, rdf:type, geold:coordinates, G).


qu_replace_nested_wkt_geometry(M1, M2, P, Q, R, G) :-
  qu_call((
    q(M1, S, P, B, G),
    % Without the blank node check an already converted literal may
    % appear in the subject position, resulting in an exception.
    q_is_bnode(B),
    q(M1, B, Q, C, G),
    q(M1, B, R, Array^^D0, G)
  ), (
    q_iri_local(C, Local0),
    lowercase_atom(Local0, Local),
    rdf_global_id(wkt:Local, D),
    qb(M2, S, geold:geometry, Array^^D, G),
    qb_rm(M1, S, P, B, G),
    qb_rm(M1, B, Q, C, G),
    qb_rm(M1, B, R, Array^^D0, G)
  )).



%! qu_replace_nested_wgs84_point(+M1, +M2, +P, +Q, +R, ?G) is det.
%
% From proper WGS84 point notation to Triply point notation.

qu_replace_nested_wgs84_point(M1, M2, G) :-
  qu_replace_nested_wgs84_point(M1, M2, wgs84:location, wgs84:long, wgs84:lat, G).


qu_replace_nested_wgs84_point(M1, M2, P, Q, R, G) :-
  debug(qu(wgs84), "Change WGS84 points to WKT points.", []),
  qu_call(
    (
      q(M1, S, P, B, G),
      % Without the blank node check an already converted literal may
      % appear in the subject position, resulting in an exception.
      q_is_bnode(B),
      q(M1, B, Q, Lng^^xsd:float, G),
      q(M1, B, R, Lat^^xsd:float, G)
    ), (
      qb_wkt_point(M2, S, point(Lng,Lat), G),
      qb_rm(M1, S, P, B, G),
      qb_rm(M1, B, R, Lng^^xsd:float, G),
      qb_rm(M1, B, Q, Lat^^xsd:float, G)
    )
  ).





% HELPERS %

%! qu_call(:Find_0, Transform_0) is det.
%
% Generic data transformation call:
%
%   - Find_0 matches a single candidate for transformation.
%
%   - Transform_0 acts on a single matched candidate to effectuate the
%     transformation.
%
% If Transform_0 fails the debugger is opened.

qu_call(Find_0, Transform_0) :-
  q_transaction(
    qu:qu_call0(Find_0, Transform_0, _{count: 0})
  ).


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





% DEBUG %

qu_add_ltag_deb(P, LTag) :-
  with_output_to(string(P0), q_print_predicate(P)),
  debug(qu(add_ltag), "Add language-tag ‘~a’ for ‘~s’.", [LTag,P0]).



qu_add_padding_deb(P, C) :-
  with_output_to(string(P0), q_print_predicate(P)),
  debug(qu(padding), "Add padding ‘~c’ to lexical forms of ‘~s’.", [C,P0]).



qu_change_datatype_deb(P, D) :-
  with_output_to(string(P0), q_print_predicate(P)),
  with_output_to(string(D0), q_print_datatype(D)),
  debug(qu(change_datatype), "Change datatype of ‘~s’ to ‘~s’.", [P0,D0]).



qu_change_lex_deb(P, _:Goal) :-
  with_output_to(string(P0), q_print_predicate(P)),
  Goal =.. [Pred|_],
  debug(qu(change_lex), "Change lexcal form for ‘~s’ using ‘~a’.", [P0,Pred]).



qu_comb_date_deb :-
  debug(qu(comb_data), "Combine ‘xsd:gYear’+‘xsd:gMonth’+‘xsd:gDay’ → ‘xsd:date’.", []).



qu_comb_month_day_deb :-
  debug(qu(comb_data), "Combine ‘xsd:gMonth’+‘xsd:gDay’ → ‘xsd:date’.", []).



qu_comb_year_month_deb :-
  debug(qu(comb_data), "Combine ‘xsd:gYear’+‘xsd:gMonth’ → ‘xsd:date’.", []).



qu_lex_to_iri_deb(P, Alias, _:Goal) :-
  with_output_to(string(P0), q_print_predicate(P)),
  Goal =.. [Pred|_],
  debug(qu(lex_to_iri), "Lexicals of ‘~s’ become IRIs with alias ‘~a’ using ‘~a’.", [P0,Alias,Pred]).



qu_lowercase_predicate_deb(Alias) :-
  debug(qu(lowercase_predicate), "Lowercase predicates using alias ‘~a’.", [Alias]).



qu_replace_predicate_deb(P, Q) :-
  with_output_to(string(P0), q_print_predicate(P)),
  with_output_to(string(Q0), q_print_predicate(Q)),
  debug(qu(replace_predicate), "Replace predicate ‘~s’ → ‘~s’.", [P0,Q0]).



qu_replace_string_deb(From, To, P) :-
  with_output_to(string(P0), q_print_predicate(P)),
  debug(qu(replace_string), "Replace string ‘~s’ → ‘~s’ for ‘~s’.", [From,To,P0]).



qu_rm_col_deb(P) :-
  with_output_to(string(P0), q_print_predicate(P)),
  debug(qu(rm_col), "Remove column ‘~s’.", [P0]).



qu_rm_empty_string_deb(P) :-
  with_output_to(string(P0), q_print_predicate(P)),
  debug(qu(rm_empty_string), "Remove ε for ‘~s’.", [P0]).



qu_rm_error_deb(S, P, O) :-
  with_output_to(string(Triple0), q_print_triple(S, P, O)),
  debug(qu(rm_error), "~s", [Triple0]).



qu_rm_null_deb(P, Null) :-
  with_output_to(string(P0), q_print_predicate(P)),
  with_output_to(string(Null0), q_print_object(Null)),
  debug(qu(rm_null), "Remove NULL value ‘~s’ for ‘~s’.", [Null0,P0]).



qu_split_string_deb(P) :-
  with_output_to(string(P0), q_print_predicate(P)),
  debug(qu(split_string), "Split string for ‘~s’.", [P0]).
