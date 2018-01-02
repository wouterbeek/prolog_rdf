:- module(
  rdf_update,
  [
    rdf_update/4, % ?S, ?P, ?O, +Action
    rdf_update/5  % ?S, ?P, ?O, ?G, +Action
  ]
).

/** <module> RDF update

@author Wouter Beek
@version 2017/08-2018/01
*/

:- use_module(library(error)).
:- use_module(library(semweb/rdf_api)).
:- use_module(library(semweb/rdf11), []).
:- use_module(library(xml/xsd)).

:- rdf_meta
   rdf_update(r, r, o, t),
   rdf_update(r, r, o, r, t).





%! rdf_update(?S, ?P, ?O, +Action:compound) is det.
%! rdf_update(?S, ?P, ?O, ?G, +Action:compound) is det.

rdf_update(S, P, O, Action) :-
  rdf_update(S, P, O, _, Action).


rdf_update(S, P, O, G, Action) :-
  must_be(ground, Action),
  (   update_column(Action, S,P,O,G, On)
  ->  must_be(ground, On),
      arg(1, Action, Old),
      (   On == Old
      ->  true
      ;   rdf_transaction(rdf_update_(S, P, O, G, Action), update)
      )
  ;   domain_error(rdf_update_action, Action)
  ).

update_column(datatype(_),  _,_,O,_, O).
update_column(graph(_),     _,_,_,G, G).
update_column(ltag(_),      _,_,O,_, O).
update_column(object(_),    _,_,O,_, O).
update_column(predicate(_), _,P,_,_, P).
update_column(subject(_),   S,_,_,_, S).

rdf_update_(S, P, O, G, datatype(D)) :- !,
  forall(
    rdf2(S, P, O, G),
    (
      update_value(O, Value^^D),
      rdf_retractall(S, P, O, G),
      rdf_assert(S, P, Value^^D, G)
    )
  ).
rdf_update_(S, P, O, G1, graph(G2)) :- !,
  forall(
    rdf2(S, P, O, G1),
    (
      rdf_retractall(S, P, O, G1),
      rdf_assert(S, P, O, G2)
    )
  ).
rdf_update_(S, P, O, G, ltag(LTag)) :- !,
  forall(
    rdf2(S, P, O, G),
    (
      rdf_literal_lexical_form(O, Lex),
      rdf_retractall(S, P, O, G),
      rdf_assert(S, P, Lex@LTag, G)
    )
  ).
rdf_update_(S, P, O1, G, object(O2)) :- !,
  forall(
    rdf2(S, P, O1, G),
    (
      rdf_retractall(S, P, O1, G),
      rdf_assert(S, P, O2, G)
    )
  ).
rdf_update_(S, P1, O, G, predicate(P2)) :- !,
  forall(
    rdf2(S, P1, O, G),
    (
      rdf_retractall(S, P1, O, G),
      rdf_assert(S, P2, O, G)
    )
  ).
rdf_update_(S1, P, O, G, subject(S2)) :- !,
  forall(
    rdf2(S1, P, O, G),
    (
      rdf_retractall(S1, P, O, G),
      rdf_assert(S2, P, O, G)
    )
  ).

update_value(Literal, N^^D) :-
  rdf_literal_value(Literal, Value),
  xsd_subtype_of(D, xsd:integer), !,
  value_to_number(Value, M),
  number_to_integer(M, N).

value_to_number(N, N) :-
  number(N), !.
value_to_number(Value, N) :-
  atom(Value), !,
  atom_number(Value, N).
value_to_number(Value, N) :-
  string(Value), !,
  string_number(Value, N).

number_to_integer(N, N) :-
  integer(N), !.
number_to_integer(M, N) :-
  number(M), !,
  N is round(M).
