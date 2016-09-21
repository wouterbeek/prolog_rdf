:- module(
  q_deref,
  [
    q_deref/2,  % +Iri, -Quad
    q_derefs/2, % +Iri, -Quads
    q_derefs/3, % +Iri, -POs, -SPs
    q_derefs/4  % +Iri, -POs, -SPs, -Rest
  ]
).

/** <module> Quine: Dereference

@author Wouter Beek
@version 2016/09
*/

:- use_module(library(aggregate)).
:- use_module(library(http/http_io)).
:- use_module(library(nb_set)).
:- use_module(library(rdf/rdf__io)).
:- use_module(library(semweb/rdf11)).

:- rdf_meta
   q_deref(r, -),
   q_derefs(r, -),
   q_derefs(r, -, -),
   q_derefs(r, -, -, -).




%! q_deref(+Iri, -Quad) is nondet.

q_deref(Iri1, Quad) :-
  empty_nb_set(Set),
  q_deref0(Set, Iri1, Quad).


q_deref0(Set, Iri1, Quad) :-
  rdf_equal(owl:sameAs, P0),
  http_fail_on_exception(rdf_load_quads(Iri1, Quads, [timeout(2)])),
  add_nb_set(Iri1, Set),
  member(Quad0, Quads),
  (   Quad = Quad0
  ;   (Quad0 = rdf(Iri1,P0,Iri2,_) ; Quad0 = rdf(Iri2,P0,Iri1,_)),
      add_nb_set(Iri2, Set, true),
      q_deref0(Set, Iri2, Quad)
  ).



%! q_derefs(+Iri, -Quads) is det.
%! q_derefs(+Iri, -POs, -SPs) is det.
%! q_derefs(+Iri, -POs, -SPs, -Rest) is det.

q_derefs(Iri, Quads) :-
  q_derefs(Iri, POs, SPs),
  append(POs, SPs, Quads).


q_derefs(Iri, POs, SPs) :-
  q_derefs(Iri, POs, SPs, _).


q_derefs(Iri, POs, SPs, Rest) :-
  q_derefs0(Iri, Quads0, Iris),
  partition(q_deref_category0(Iris), Quads0, POs, Rest, SPs).


q_derefs0(Iri, Quads, Iris) :-
  empty_nb_set(Set),
  aggregate_all(set(Quad), q_deref0(Set, Iri, Quad), Quads),
  nb_set_to_list(Set, Iris).


q_deref_category0(Iris, rdf(S,_,_,_), <) :-
  memberchk(S, Iris), !.
q_deref_category0(Iris, rdf(_,_,O,_), >) :-
  memberchk(O, Iris), !.
q_deref_category0(_, _, =).
