:- module(
  rdf_update,
  [
    rdf_cp/2,      % +FromG, +ToG
    rdf_cp/5,      % +FromG, ?S, ?P, ?O, +ToG
    rdf_inc/2,     % +S, +P
    rdf_inc/3,     % +S, +P, +G
    rdf_mv/2,      % +FromG, +ToG
    rdf_mv/5,      % +FromG, ?S, ?P, ?O, +ToG
    rdf_rm/3,      % ?S, ?P, ?O
    rdf_rm/4,      % ?S, ?P, ?O, ?G
    rdf_rm_cell/3, % +S, +P, +O
    rdf_rm_cell/4, % +S, +P, +O, +G
    rdf_rm_col/1,  % +P
    rdf_rm_col/2,  % +P, +G
    rdf_rm_null/1, % +Null
    rdf_rm_null/2  % +Null, +G
  ]
).

/** <module> RDF update

Higher-level update operations performed on RDF data.

@author Wouter Beek
@version 2015/07-2015/08, 2015/10-2016/01, 2016/03, 2016/05
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(print_ext)).
:- use_module(library(semweb/rdf11)).

:- rdf_meta
   rdf_cp(r, r),
   rdf_cp(r, r, r, o, r),
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
   rdf_rm_null(o),
   rdf_rm_null(o, r).





%! rdf_cp(+FromG, +ToG) is det.
%! rdf_cp(+FromG, ?S, ?P, ?O, +ToG) is det.
%
% Copy graph or copy triples between graphs.
%
% @tbd Perform blank node renaming.

rdf_cp(FromG, ToG) :-
  FromG == ToG, !.
rdf_cp(FromG, ToG) :-
  rdf_cp(FromG, _, _, _, ToG).


rdf_cp(FromG, S, P, O, ToG) :-
  rdf_transaction(rdf_cp0(copied, FromG, S, P, O, ToG)).

rdf_cp0(Action, FromG, S, P, O, ToG) :-
  forall(rdf(S, P, O, FromG), (
    rdf_assert(S, P, O, ToG),
    (   debugging(rdf(update))
    ->  format("[~a] ", [Action]),
        rdf_print_triples(S, P, O, FromG),
        write(" → "),
        rdf_print_triples(S, P, O, ToG)
    ;   true
    )
  )).



%! rdf_inc(+S, +P) is det.
%! rdf_inc(+S, +P, +G) is det.
%
% Increment values.

rdf_inc(S, P) :-
  rdf_inc(S, P, _).


rdf_inc(S, P, G) :-
  rdf_transaction((
    rdf(S, P, Old^^D, G),

    % Any integer datatype can be incremented.
    rdf11:xsd_numerical(D, TypeCheck, integer),
    New is Old + 1,
    % Make sure the new value belongs to the datatype's value space.
    must_be(TypeCheck, New),

    rdf_retractall(S, P, Old^^Old, G),
    rdf_assert(S, P, New^^D, G)
  )).



%! rdf_mv(+FromG, ?S, ?P, ?O, +ToG) is det.
%! rdf_mv(+FromG, +ToG) is det.
%
% Rename graphs or move triples between graphs.

rdf_mv(FromG, ToG) :-
  rdf_mv(FromG, _, _, _, ToG),
  rdf_unload_graph(FromG).


rdf_mv(FromG, S, P, O, ToG) :-
  rdf_transaction((
    rdf_cp0(moved, FromG, S, P, O, ToG),
    rdf_retractall(S, P, O, FromG)
  )).



%! rdf_rm(?S, ?P, ?O) is det.
%! rdf_rm(?S, ?P, ?O, ?G) is det.

rdf_rm(S, P, O) :-
  rdf_rm(S, P, O, _).


rdf_rm(S, P, O, G) :-
  rdf_transaction(rdf_rm(S, P, O, G, _{count:0})).


rdf_rm(S, P, O, G, State) :-
  rdf(S, P, O, G),
  rdf_retractall(S, P, O, G),
  dict_inc(count, State),
  fail.
rdf_rm(_, _, _, _, State) :-
  ansi_format(
    user_output,
    [fg(yellow)],
    "~D statements were removed.~n",
    [State.count]
  ).



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



%! rdf_rm_null(+Null) is det.
%! rdf_rm_null(+Null, +G) is det.

rdf_rm_null(Null) :-
  rdf_rm_null(Null, _).


rdf_rm_null(Null, G) :-
  rdf_rm(_, _, Null, G).
