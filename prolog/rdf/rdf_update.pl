:- module(
  rdf_update,
  [
    rdf_change/4,           % +S, +P, +O, +Action
    rdf_change_datatype/2,  % +P, +D
    rdf_cp/2,               % +FromG, +ToG
    rdf_cp/5,               % +FromG, ?S, ?P, ?O, +ToG
    rdf_inc/2,              % +S, +P
    rdf_inc/3,              % +S, +P, +G
    rdf_mv/2,               % +FromG, +ToG
    rdf_mv/5,               % +FromG, ?S, ?P, ?O, +ToG
    rdf_parse_col/2,        % +P, Dcg_0
    rdf_rm/3,               % ?S, ?P, ?O
    rdf_rm/4,               % ?S, ?P, ?O, ?G
    rdf_rm_cell/3,          % +S, +P, +O
    rdf_rm_cell/4,          % +S, +P, +O, ?G
    rdf_rm_col/1,           % +P
    rdf_rm_col/2,           % +P, +G
    rdf_rm_error/3,         % ?S, ?P, ?O
    rdf_rm_error/4,         % ?S, ?P, ?O, ?G
    rdf_rm_null/1,          % +Null
    rdf_rm_null/2           % +Null, +G
  ]
).

/** <module> RDF update

Higher-level update operations performed on RDF data.

@author Wouter Beek
@version 2015/07-2015/08, 2015/10-2016/01, 2016/03, 2016/05
*/

:- use_module(library(cli_ext)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug_ext)).
:- use_module(library(error)).
:- use_module(library(dict_ext)).
:- use_module(library(print_ext)).
:- use_module(library(rdf/rdf_datatype)).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(semweb/rdf_db), []).
:- use_module(library(semweb/rdf11)).

:- meta_predicate
    rdf_parse_col(+, //).

:- rdf_meta
   rdf_change(r, r, o, t),
   rdf_change_datatype(r, r),
   rdf_cp(r, r),
   rdf_cp(r, r, r, o, r),
   rdf_inc(r, r),
   rdf_inc(r, r, +),
   rdf_mv(r, r),
   rdf_mv(r, r, r, o, r),
   rdf_parse_col(r, :),
   rdf_rm(r, r, o),
   rdf_rm(r, r, o, r),
   rdf_rm_cell(r, r, o),
   rdf_rm_cell(r, r, o, r),
   rdf_rm_col(r),
   rdf_rm_col(r, r),
   rdf_rm_error(r, r, o),
   rdf_rm_error(r, r, o, r),
   rdf_rm_null(o),
   rdf_rm_null(o, r).





%! rdf_change(?S, ?P, ?O, +Action) is det.

rdf_change(S, P, O, Action) :-
  must_be(ground, O),
  rdf11:pre_ground_object(O, O0),
  (   Action = object(NewO)
  ->  rdf11:pre_ground_object(NewO, NewO0),
      Action0 = object(NewO0)
  ;   Action0 = Action
  ),
  rdf_db:rdf_update(S, P, O0, Action0).



%! rdf_change_datatype(+P, +D) is semidet.

rdf_change_datatype(P, D2) :-
  rdf_datatypes_compat(P, Ds),
  memberchk(D2, Ds),
  State = _{count:0},
  forall((
    rdf(S, P, Lit1),
    rdf_literal(Lit1, D1, Lex, LTag),
    D1 \== D2
  ), (
    rdf_literal(Lit2, D2, Lex, LTag),
    rdf_change(S, P, Lit1, object(Lit2)),
    dict_inc(count, State)
  )),
  rdf_msg(State.count, "changed datatype").



%! rdf_cp(+FromG, +ToG) is det.
%! rdf_cp(+FromG, ?S, ?P, ?O, +ToG) is det.
%
% Copy graph or copy triples between graphs.
%
% @tbd Perform blank node renaming.
% @tbd Use rdf_update/5.

rdf_cp(FromG, ToG) :-
  FromG == ToG, !.
rdf_cp(FromG, ToG) :-
  rdf_cp(FromG, _, _, _, ToG).


rdf_cp(FromG, S, P, O, ToG) :-
  rdf_transaction(forall(rdf(S, P, O, FromG), rdf_assert(S, P, O, ToG))).



%! rdf_inc(+S, +P) is det.
%! rdf_inc(+S, +P, +G) is det.
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
    rdf_change(S, P, N1^^D, G, object(N2^^D))
  )).



%! rdf_mv(+FromG, ?S, ?P, ?O, +ToG) is det.
%! rdf_mv(+FromG, +ToG) is det.
%
% Rename graphs or move triples between graphs.

rdf_mv(FromG, ToG) :-
  rdf_mv(FromG, _, _, _, ToG),
  rdf_unload_graph(FromG).


rdf_mv(FromG, S, P, O, ToG) :-
  State = _{count:0},
  forall(rdf(S, P, O, FromG), (
    rdf_change(S, P, O, FromG, graph(ToG)),
    dict_inc(count, State)
  )),
  rdf_msg(State.count, "moved").



%! rdf_parse_col(+P, :Dcg_0) is det.
%
% @tbd This does not work with a graph argument.
% @tbd Explain what rdf_update/4 does in terms of graphs.

rdf_parse_col(P, Dcg_0) :-
  forall(rdf(_, P, Lit), (
    rdf_literal(Lit, D, Lex1, _),
    string_phrase(Dcg_0, Lex1, Lex2),
    rdf_datatype_compat(Lex2, D)
  )),
  State = _{count:0},
  forall(rdf(S, P, Lit1), (
    rdf_literal(Lit1, D, Lex1, LTag),
    string_phrase(Dcg_0, Lex1, Lex2),
    rdf_literal(Lit2, D, Lex2, LTag),
    rdf_change(S, P, Lit1, object(Lit2)),
    dict_inc(count, State)
  )),
  rdf_msg(State.count, "changed lexical form").



%! rdf_rm(?S, ?P, ?O) is det.
%! rdf_rm(?S, ?P, ?O, ?G) is det.

rdf_rm(S, P, O) :-
  rdf_rm(S, P, O, _).


rdf_rm(S, P, O, G) :-
  State = _{count:0},
  forall(rdf(S, P, O, G), (
    rdf_retractall(S, P, O, G),
    dict_inc(count, State)
  )),
  rdf_msg(State.count, "removed").



%! rdf_rm_cell(+S, +P, +O) is det.
%! rdf_rm_cell(+S, +P, +O, +G) is det.

rdf_rm_cell(S, P, O) :-
  rdf_rm_cell(S, P, O, _).


rdf_rm_cell(S, P, O, G) :-
  rdf_rm(S, P, O, G).



%! rdf_rm_col(+P) is det.
%! rdf_rm_col(+P, +G) is det.

rdf_rm_col(P) :-
  rdf_rm_col(P, _).


rdf_rm_col(P, G) :-
  rdf_rm(_, P, _, G).



%! rdf_rm_errpr(?S, ?P, ?O) is det.
%! rdf_rm_error(?S, ?P, ?O, ?G) is det.

rdf_rm_error(S, P, O) :-
  rdf_rm_error(S, P, O, _).


rdf_rm_error(S, P, O, G) :-
  rdf_rm(S, P, O, G).



%! rdf_rm_null(+Null) is det.
%! rdf_rm_null(+Null, +G) is det.

rdf_rm_null(Null) :-
  rdf_rm_null(Null, _).


rdf_rm_null(Null, G) :-
  rdf_rm(_, _, Null, G).





% HELPERS %

%! rdf_msg(+N, +Action) is det.

rdf_msg(N, Action) :-
  ansi_format(
    user_output,
    [fg(yellow)],
    "~D statements were ~s.~n",
    [N,Action]
  ).
