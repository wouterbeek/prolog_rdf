:- module(
  rdf_update,
  [
    rdf_cp/5, % +FromGraph:atom
              % ?Subject:or([bnode,iri])
              % ?Predicate:iri
              % ?Object:rdf_term
              % +ToGraph:atom
    rdf_increment/2, % +Subject, +Predicate
    rdf_increment/3, % +Subject:or([bnode,iri])
                     % +Predicate:iri
                     % +Graph:atom
    rdf_mv/5, % +FromGraph:atom
              % ?Subject:or([bnode,iri])
              % ?Predicate:iri
              % ?Object:rdf_term
              % +ToGraph:atom
    rdf_update/5 % +Subject:rdf_term
                 % +Predicate:iri
                 % +Object:rdf_term
                 % +Graph:rdf_graph
                 % +Action:compound
  ]
).

/** <module> RDF update

Higher-level update operations performed on RDF data.

@author Wouter Beek
@version 2015/07-2015/08, 2015/10-2016/01
*/

:- use_module(library(dcg/dcg_bracket)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_debug)).
:- use_module(library(debug_ext)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_database)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_print_stmt)).
:- use_module(library(rdf/rdf_read)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(rdf11/rdf11), []).
:- use_module(library(xsd/xsd)).

:- rdf_meta
   rdf_cp(+,r,r,o,+),
   rdf_increment(r,r),
   rdf_increment(r,r,+),
   rdf_mv(+,r,r,o,+),
   rdf_update(o,r,o,r,+).





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
    dcg_debug(rdf(update), (
      bracketed(square, atom(Action)),
      " ",
      rdf_print_statement(S, P, O, FromG, []),
      " → ",
      rdf_print_statement(S, P, O, ToG, [])
    ))
  )).



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



%! rdf_update(
%!   +Subject:rdf_term,
%!   +Predicate:iri,
%!   +Object:rdf_term,
%!   +Graph:rdf_graph,
%!   +Action:compound
%! ) is det.

rdf_update(S1, P1, O1, G1, Act) :-
  rdf_update_action(S1, P1, O1, G1, Act, S2, P2, O2, G2),
  rdf_transaction(
    rdf_retractall(S1, P1, O1, G1),
    rdf_assert(S2, P2, O2, G2)
  ).

rdf_update_action(S, P, O, _, graph(G2),     S,  P,  O,  G2).
rdf_update_action(S, P, _, G, object(O2),    S,  P,  O2, G ).
rdf_update_action(S, _, O, G, predicate(P2), S,  P2, O,  G ).
rdf_update_action(_, P, O, G, subject(S2),   S2, P,  O,  G ).
