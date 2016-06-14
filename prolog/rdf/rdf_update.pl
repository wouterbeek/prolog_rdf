:- module(
  rdf_update,
  [
    rdf_call_update/1,     % :Goal_0
    rdf_change_datatype/2, % +P, +D
    rdf_change_datatype/3, % +P, ?G, +D
    rdf_change_iri/5,      % ?S, ?P, ?O, +Positions, :Dcg_0
    rdf_change_iri/6,      % ?S, ?P, ?O, +Positions, ?G, :Dcg_0
    rdf_change_lex/2,      % +P, :Dcg_0
    rdf_change_lex/3,      % +P, ?G, :Dcg_0
    rdf_change_p/2,        % +P, +Q
    rdf_change_p/2,        % +P, ?G, +Q
    rdf_cp/2,              % +G1, +G2
    rdf_cp/5,              % +G1, ?S, ?P, ?O, +G2
    rdf_comb_date/4,       % +YP, +MP, +DP, +Q
    rdf_comb_date/5,       % +YP, +MP, +DP, ?G, +Q
    rdf_comb_month_day/3,  % +MP, +DP, +Q
    rdf_comb_month_day/4,  % +MP, +DP, ?G, +Q
    rdf_comb_year_month/3, % +YP, +MP, +Q
    rdf_comb_year_month/4, % +YP, +MP, ?G, +Q
    rdf_inc/2,             % +S, +P
    rdf_inc/3,             % +S, +P, ?G
    rdf_flatten/1,         % +P
    rdf_flatten/2,         % +P, ?G
    rdf_mv/2,              % +G1, +G2
    rdf_mv/5,              % +G1, ?S, ?P, ?O, +G2
    rdf_rm/3,              % ?S, ?P, ?O
    rdf_rm/4,              % ?S, ?P, ?O, ?G
    rdf_padding/1,         % +P
    rdf_padding/2,         % +P, +PaddingCode
    rdf_padding/3,         % +P, +PaddingCode, ?G
    rdf_rm_cell/3,         % +S, +P, +O
    rdf_rm_cell/4,         % +S, +P, +O, ?G
    rdf_rm_col/1,          % +P
    rdf_rm_col/2,          % +P, ?G
    rdf_rm_error/3,        % ?S, ?P, ?O
    rdf_rm_error/4,        % ?S, ?P, ?O, ?G
    rdf_rm_null/1,         % +Null
    rdf_rm_null/2,         % +Null, ?G
    rdf_rm_tree/1,         % +S
    rdf_rm_tuples/1,       % +Tuples
    rdf_split_lex/2,       % +P, :Dcg_2
    rdf_split_lex/3        % +P, ?G, :Dcg_2
  ]
).

/** <module> RDF update

Higher-level update operations performed on RDF data.

@author Wouter Beek
@version 2015/07-2015/08, 2015/10-2016/01, 2016/03, 2016/05-2016/06
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(cli_ext)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug_ext)).
:- use_module(library(error)).
:- use_module(library(dict_ext)).
:- use_module(library(list_ext)).
:- use_module(library(print_ext)).
:- use_module(library(rdf/rdf_datatype)).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(semweb/rdf_db), []).
:- use_module(library(semweb/rdf11)).
:- use_module(library(solution_sequences)).

:- meta_predicate
    rdf_call_update(0),
    rdf_call_update(0, +),
    rdf_change_iri(?, ?, ?, +, //),
    rdf_change_iri(?, ?, ?, +, ?, //),
    rdf_change_iri0(+, +, //, -),
    rdf_change_lex(+, //),
    rdf_change_lex(+, ?, //),
    rdf_split_lex(+, 4),
    rdf_split_lex(+, ?, 4).

:- rdf_meta
   rdf_call_update(t),
   rdf_call_update(t, +),
   rdf_change_datatype(r, r),
   rdf_change_datatype(r, r, r),
   rdf_change_iri(r, r, o, +, :),
   rdf_change_iri(r, r, o, +, r, :),
   rdf_change_lex(r, :),
   rdf_change_lex(r, r, :),
   rdf_change_p(r, r),
   rdf_change_p(r, r, r),
   rdf_comb_date(r, r, r, r),
   rdf_comb_date(r, r, r, r, r),
   rdf_comb_month_day(r, r, r),
   rdf_comb_month_day(r, r, r, r),
   rdf_comb_year_month(r, r, r),
   rdf_comb_year_month(r, r, r, r),
   rdf_cp(r, r),
   rdf_cp(r, r, r, o, r),
   rdf_flatten(r),
   rdf_flatten(r, r),
   rdf_inc(r, r),
   rdf_inc(r, r, +),
   rdf_mv(r, r),
   rdf_mv(r, r, r, o, r),
   rdf_rm(r, r, o),
   rdf_rm(r, r, o, r),
   rdf_rm_cell(r, r, o),
   rdf_rm_cell(r, r, o, r),
   rdf_rm_col(r),
   rdf_rm_col(r, r),
   rdf_rm_error(r, r, o),
   rdf_rm_error(r, r, o, r),
   rdf_rm_null(o),
   rdf_rm_null(o, r),
   rdf_rm_tree(r),
   rdf_rm_tuples(t),
   rdf_split_lex(r, :),
   rdf_split_lex(r, r, :).





%! rdf_call_update(:Goal_0) is det.
%
% The following call is made: `call(

rdf_call_update(Goal_0) :-
  rdf_transaction(rdf_call_update(Goal_0, _{count: 0})).


rdf_call_update(Goal_0, State) :-
  call(Goal_0),
  dict_inc(count, State),
  fail.
rdf_call_update(_, State) :-
  ansi_format(
    user_output,
    [fg(yellow)],
    "~D updates were made.~n",
    [State.count]
  ).



%! rdf_change_datatype(+P, +D) is semidet.
%! rdf_change_datatype(+P, ?G, +D) is semidet.
%
% Change the datatype IRI of all object terms that appear with
% predicate P.

rdf_change_datatype(P, D) :-
  rdf_change_datatype(P, _, D).


rdf_change_datatype(P, G, D2) :-
  rdf_call_update((
    % Find instance.
    rdf(S, P, Lit1, G),
    rdf_literal(Lit1, D1, Lex, LTag),
    D1 \== D2,
    % Transform instance.
    rdf_literal(Lit2, D2, Lex, LTag),
    rdf_update(S, P, Lit1, G, object(Lit2))
  )).



%! rdf_change_iri(?S, ?P, ?O, +Positions:list(oneof([+,-])), :Dcg_0) is det.
%! rdf_change_iri(?S, ?P, ?O, +Positions:list(oneof([+,-])), ?G, :Dcg_0) is det.

rdf_change_iri(S1, P1, O1, Pos, Dcg_0) :-
  rdf_change_iri(S1, P1, O1, Pos, _, Dcg_0).


rdf_change_iri(S1, P1, O1, Pos, G, Dcg_0) :-
  rdf_call_update((
    % Find instance.
    rdf(S1, P1, O1, G),
    rdf_change_iri0(rdf(S1,P1,O1), Pos, Dcg_0, rdf(S2,P2,O2)),
    rdf(S1,P1,O1) \== rdf(S2,P2,O2),
    % Transform instance.
    rdf_retractall(S1, P1, O1, G),
    rdf_assert(S2, P2, O2, G)
  )).

rdf_change_iri0(rdf(S1,P1,O1), [PosS,PosP,PosO], Dcg_0, rdf(S2,P2,O2)) :-
  (PosS == +, rdf_is_iri(S1) -> atom_phrase(Dcg_0, S1, S2) ; S2 = S1),
  (PosP == +, rdf_is_iri(P1) -> atom_phrase(Dcg_0, P1, P2) ; P2 = P1),
  (PosO == +, rdf_is_iri(O1) -> atom_phrase(Dcg_0, O1, O2) ; O2 = O1).
  
  

%! rdf_change_lex(+P, :Dcg_0) is det.
%! rdf_change_lex(+P, ?G, :Dcg_0) is det.
%
% Change the lexical form of the literals that appear with predicate
% P.  Dcg_0 is used to parse the old lexical form and generate the new
% one.

rdf_change_lex(P, Dcg_0) :-
  rdf_change_lex(P, _, Dcg_0).


rdf_change_lex(P, G, Dcg_0) :-
  rdf_call_update((
    % Find instance.
    rdf(S, P, Lit1, G),
    rdf_literal(Lit1, D, Lex1, LTag),
    string_phrase(Dcg_0, Lex1, Lex2),
    Lex1 \== Lex2,
    rdf_datatype_compat(Lex2, D),
    % Transform instance.
    rdf_literal(Lit2, D, Lex2, LTag),
    rdf_update(S, P, Lit1, G, object(Lit2))
  )).



%! rdf_change_p(+P, +Q) is det.
%! rdf_change_p(+P, ?G, +Q) is det.

rdf_change_p(P, Q) :-
  rdf_change_p(P, _, Q).


rdf_change_p(P, G, Q) :-
  rdf_call_update((
    % Find instance.
    rdf(S, P, O, G),
    P \== Q,
    % Transform instance.
    rdf_update(S, P, O, G, predicate(Q))
  )).



%! rdf_comb_date(+YP, +MoP, +DaP, +Q) is det.
%! rdf_comb_date(+YP, +MoP, +DaP, ?G, +Q) is det.

rdf_comb_date(YP, MP, DP, Q) :-
  rdf_comb_date(YP, MP, DP, _, Q).

rdf_comb_date(YP, MP, DP, G, Q) :-
  rdf_call_update((
    % Find instance.
    rdf(S, YP, Y^^xsd:gYear, G),
    rdf(S, MP, M^^xsd:gMonth, G),
    rdf(S, DP, D^^xsd:gDay, G),
    % Transform instance.
    rdf_assert(S, Q, date(Y,M,D)^^xsd:date, G)%,
    %%%%rdf_retractall(S, YP, Y^^xsd:gYear, G),
    %%%%rdf_retractall(S, MP, M^^xsd:gMonth, G),
    %%%%rdf_retractall(S, DP, D^^xsd:gDay, G)
  )).



%! rdf_comb_month_day(+MP, +DP, +Q) is det.
%! rdf_comb_month_day(+MP, +DP, ?G, +Q) is det.

rdf_comb_month_day(MP, DP, Q) :-
  rdf_comb_month_day(MP, DP, _, Q).


rdf_comb_month_day(MP, DP, G, Q) :-
  rdf_call_update((
    % Find instance.
    rdf(S, MP, M^^xsd:gMonth, G),
    rdf(S, DP, D^^xsd:gDay, G),
    % Transform instance.
    rdf_assert(S, Q, month_day(M,D)^^xsd:gMonthDay, G)%,
    %%%%rdf_retractall(S, MoP, Mo^^xsd:gMonth, G),
    %%%%rdf_retractall(S, DaP, Da^^xsd:gDay, G)
  )).



%! rdf_comb_year_month(+YP, +MP, +Q) is det.
%! rdf_comb_year_month(+YP, +MP, ?G, +Q) is det.

rdf_comb_year_month(YP, MP, Q) :-
  rdf_comb_year_month(YP, MP, _, Q).


rdf_comb_year_month(YP, MP, G, Q) :-
  rdf_call_update((
    % Find instance.
    rdf(S, YP, Y^^xsd:gYear, G),
    rdf(S, MP, M^^xsd:gMonth, G),
    % Transform instance.
    rdf_assert(S, Q, year_month(Y,M)^^xsd:gYearMonth, G)%,
    %%%%rdf_retractall(S, YP, Y^^xsd:gYear, G),
    %%%%rdf_retractall(S, MoP, Mo^^xsd:gMonth, G)
  )).




%! rdf_cp(+G1, +G2) is det.
%! rdf_cp(+G1, ?S, ?P, ?O, +G2) is det.
%
% Copy a full graph or copy the specified triples between graphs.
%
% This asumes that blank nodes are shared between graphs.

rdf_cp(G1, G2) :-
  rdf_cp(G1, _, _, _, G2).


rdf_cp(G, _, _, _, G) :- !.
rdf_cp(G1, S, P, O, G2) :-
  rdf_transaction(
    forall(
      rdf(S, P, O, G1),
      rdf_assert(S, P, O, G2)
    )
  ).



%! rdf_flatten(+P) is det.
%! rdf_flatten(+P, ?G) is det.

rdf_flatten(P) :-
  rdf_flatten(P, _).


rdf_flatten(P, G) :-
  rdf_call_update((
    % Find instance.
    rdf(X, P, Y, G),
    rdf_is_bnode(Y),
    rdf(Y, Q, Z, G),
    % Transform instance.
    rdf_update(Y, Q, Z, G, subject(X)),
    rdf_retractall(X, P, Y, G)
  )).



%! rdf_inc(+S, +P) is det.
%! rdf_inc(+S, +P, ?G) is det.
%
% Increment values.

rdf_inc(S, P) :-
  rdf_inc(S, P, _).


rdf_inc(S, P, G) :-
  rdf_transaction((
    rdf(S, P, N1^^D, G),
    rdf11:xsd_numerical(D, TypeCheck, integer),
    N2 is N1 + 1,
    must_be(TypeCheck, N2),
    rdf_update(S, P, N1^^D, G, object(N2^^D))
  )).



%! rdf_mv(+G1, +G2) is det.
%! rdf_mv(+G1, ?S, ?P, ?O, +G2) is det.
%
% Rename a graph or move the specified triples between graphs.

rdf_mv(G1, G2) :-
  rdf_mv(G1, _, _, _, G2),
  rdf_unload_graph(G1).


rdf_mv(G1, S, P, O, G2) :-
  rdf_call_update((
    % Find instance.
    rdf(S, P, O, G1),
    % Transform instance.
    rdf_update(S, P, O, G1, graph(G2))
  )).



%! rdf_padding(+P) is det.
%! rdf_padding(+P, +PaddingCode) is det.
%! rdf_padding(+P, +PaddingCode, ?G) is det.

rdf_padding(P) :-
  rdf_padding(P, 0'0).


rdf_padding(P, C) :-
  rdf_padding(P, C, _).


rdf_padding(P, C, G) :-
  aggregate_all(max(Len), (
    rdf(_, P, Lit, G),
    rdf_literal_lex(Lit, Lex),
    atom_length(Lex, Len)
  ), Max),
  rdf_change_lex(P, G, rdf_padding_lex(C, Max)).

rdf_padding_lex(C, Len), Cs -->
  ...(Suffix),
  eos,
  {
    length(Suffix, SuffixLen),
    PrefixLen is Len - SuffixLen,
    repeating_list(C, PrefixLen, Prefix),
    append(Prefix, Suffix, Cs)
  }.



%! rdf_rm(?S, ?P, ?O) is det.
%! rdf_rm(?S, ?P, ?O, ?G) is det.
%
% Remove the specified triples or quadruples.

rdf_rm(S, P, O) :-
  rdf_rm(S, P, O, _).


rdf_rm(S, P, O, G) :-
  rdf_call_update((
    % Find instance.
    rdf(S, P, O, G),
    % Transform instance.
    rdf_retractall(S, P, O, G)
  )).



%! rdf_rm_cell(+S, +P, +O) is det.
%! rdf_rm_cell(+S, +P, +O, ?G) is det.
%
% Remove a specific triple or quadruple.

rdf_rm_cell(S, P, O) :-
  rdf_rm_cell(S, P, O, _).


rdf_rm_cell(S, P, O, G) :-
  rdf_rm(S, P, O, G).



%! rdf_rm_col(+P) is det.
%! rdf_rm_col(+P, ?G) is det.
%
% Remove all triples that contain predicate P.

rdf_rm_col(P) :-
  rdf_rm_col(P, _).


rdf_rm_col(P, G) :-
  rdf_rm(_, P, _, G).



%! rdf_rm_errpr(?S, ?P, ?O) is det.
%! rdf_rm_error(?S, ?P, ?O, ?G) is det.
%
% Wrapper around rdf_rm/4 that indicates that an error or mistake is
% being removed.

rdf_rm_error(S, P, O) :-
  rdf_rm_error(S, P, O, _).


rdf_rm_error(S, P, O, G) :-
  rdf_rm(S, P, O, G).



%! rdf_rm_null(+Null) is det.
%! rdf_rm_null(+Null, ?G) is det.
%
% Wrapper around rdf_rm/4 that indicates that an error or mistake is
% being removed.

rdf_rm_null(Null) :-
  rdf_rm_null(Null, _).


rdf_rm_null(Null, G) :-
  rdf_rm(_, _, Null, G).



%! rdf_rm_tree(+S) is det.

rdf_rm_tree(S) :-
  rdf_tree(S, Tree),
  rdf_rm_tuples(Tree).



%! rdf_rm_tuples(+Tuples) is det.

rdf_rm_tuples(Tuples) :-
  rdf_call_update((
    % Find instance.
    member(Tuple, Tuples),
    rdf_tuple(Tuple),
    % Transform instance.
    rdf_retractall(Tuple)
  )).



%! rdf_split_lex(+P, :Dcg_2) is det.
%! rdf_split_lex(+P, ?G, :Dcg_2) is det.

rdf_split_lex(P, Dcg_2) :-
  rdf_split_lex(P, _, Dcg_2).


rdf_split_lex(P, G, Dcg_2) :-
  rdf_call_update((
    rdf(S, P, Lex^^xsd:string, G),
    string_phrase(dcg_call(Dcg_2, S, G), Lex),
    rdf_retractall(S, P, Lex^^xsd:string, G)
  )).
