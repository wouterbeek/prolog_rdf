:- module(
  rdf_update,
  [
    rdf_cp/5,        % +FromG, ?S, ?P, ?O, +ToG
    rdf_cp_graph/2,  % +FromG, +ToG
    rdf_increment/2, % +S, +P
    rdf_increment/3, % +S, +P, +G
    rdf_mv/5,        % +FromG, ?S, ?P, ?O, +ToG
    rdf_mv_graph/2   % +FromG, +ToG
  ]
).

/** <module> RDF update

Higher-level update operations performed on RDF data.

@author Wouter Beek
@version 2015/07-2015/08, 2015/10-2016/01, 2016/03
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug_ext)).
:- use_module(library(rdf/rdf_ext)).

:- rdf_meta
   rdf_cp(r, r, r, o, r),
   rdf_cp_graph(r, r),
   rdf_increment(r, r),
   rdf_increment(r, r, +),
   rdf_mv(r, r, r, o, r),
   rdf_mv_graph(r, r).





%! rdf_cp(
%!   +FromGraph:atom,
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   +ToGraph:atom
%! ) is det.
% Copies triples between graphs.
%
% @tbd Perform blank node renaming.

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



%! rdf_cp_graph(+FromG, +ToG) is det.

rdf_cp_graph(FromG, ToG) :-
  FromG == ToG, !.
rdf_cp_graph(FromG, ToG) :-
  rdf_cp(FromG, _, _, _, ToG).



%! rdf_increment(+Subject:or([bnode,iri]), +Predicate:iri) is det.
% Wrapper around rdf_increment/5.

rdf_increment(S, P) :-
  rdf_increment(S, P, _).


%! rdf_increment(+Subject:or([bnode,iri]), +Predicate:iri, +Graph:atom) is det.

rdf_increment(S, P, G) :-
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



%! rdf_mv(
%!   +FromGraph:atom,
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   +ToGraph:atom
%! ) is det.
% Move triples between graphs.

rdf_mv(FromG, S, P, O, ToG) :-
  rdf_transaction((
    rdf_cp0(moved, FromG, S, P, O, ToG),
    rdf_retractall(S, P, O, FromG)
  )).



%! rdf_mv_graph(+FromG, +ToG) is det.

rdf_mv_graph(FromG, ToG) :-
  rdf_mv(FromG, _, _, _, ToG),
  rdf_unload_graph(FromG).
