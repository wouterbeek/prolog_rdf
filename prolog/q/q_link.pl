:- module(
  q_link,
  [
    q_link/2,         % +Term, -Iri
    q_link_literal/2, % +Lit, -Iri
    q_link_objects/2, % +P, +G
    q_link_subjects/2 % +P, +G
  ]
).

/** <module> Quine: IRI linking

@author Wouter Beek
@version 2016/10
*/

:- use_module(library(dcg/dcg_cli)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(print_ext)).
:- use_module(library(q/q_print)).
:- use_module(library(q/q_rdf)).
:- use_module(library(q/q_term)).
:- use_module(library(q/qu)).
:- use_module(library(service/ll_api)).
:- use_module(library(service/lotus_api)).
:- use_module(library(semweb/rdf11)).

:- rdf_meta
   q_link(o, -),
   q_link_literal(o, -),
   q_link_objects(r, r),
   q_link_subjects(r, r).





%! q_link(+Term, -Iri) is nondet.

q_link(Lit, Iri) :-
  q_is_literal(Lit), !,
  q_link_literal(Lit, Iri),
  forall(
    q_link_class(Iri, C),
    (write("  - "), q_print_object(C), nl)
  ),
  dcg_with_output_to(string(Msg), (
    str("Do you want to link ‘"),
    dcg_q_print_literal(Lit),
    str(" to ‘"),
    dcg_q_print_iri(Iri),
    str("’?")
  )),
  (user_input(Msg) -> !, true ; fail).

q_link_class(I, C) :-
  % @bug RDF prefix expansion does not work inside distinct/2.
  rdf_equal(rdf:type, P),
  distinct(C, ll_ldf(I, P, C)).



%! q_link_literal(+Lit, -Iri) is nondet.

q_link_literal(Str@LTag, Iri) :- !,
  lotus(Str, Iri, [ltag(LTag)]).
q_link_literal(Lit, Iri) :-
  q_literal_lex(Lit, Lex),
  lotus(Lex, Iri).

/*
  format(
    atom(Query),
    '\c
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n\c
SELECT ?s\n\c
WHERE {\n\c
  ?s rdfs:label "~s"@en .\n\c
}\n',
    [AreaStr]
  ),
  (   sparql_select('http://dbpedia.org/sparql', Query, Result),
      Result = [[AreaIri]|_]
  ->  debug(drones, "[LINK] ~a", [AreaIri]),
      forall(
        q(trp, S, P, AreaStr@en, CbdG),
        qu(S, P, AreaStr@en, CbdG, object(AreaIri))
      )
  ;   debug(drones, "[NOTHING] ~a", [AreaStr])
  ).
*/



%! q_link_objects(+P, +G) is det.

q_link_objects(P, G) :-
  forall(
    q(trp, S, P, O1, G),
    (q_link(O1, O2) -> qu(S, P, O1, G, object(O2)) ; true)
  ).



%! q_link_subjects(+P, +G) is det.

q_link_subjects(P, G) :-
  forall(
    q(trp, S1, P, O, G),
    (q_link(S1, S2) -> qu(S1, P, O, G, subject(S2)) ; true)
  ).
