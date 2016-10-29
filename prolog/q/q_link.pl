:- module(
  q_link,
  [
    q_link/4,        % +Backend, +Term, -Score, -Iri
    q_link_objects/3 % +Backend, +P, +G
  ]
).

/** <module> Quine: Term linking

Literals are substituted by IRIs.

IRIs are linked using ‘owl:sameAs’.

@author Wouter Beek
@version 2016/10
*/

:- use_module(library(aggregate)).
:- use_module(library(cli_ext)).
:- use_module(library(dcg/dcg_cli)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(print_ext)).
:- use_module(library(q/q_deref)).
:- use_module(library(q/q_io)).
:- use_module(library(q/q_print)).
:- use_module(library(q/q_rdf)).
:- use_module(library(q/q_term)).
:- use_module(library(q/qu)).
:- use_module(library(service/fct)).
:- use_module(library(service/ll_api)).
:- use_module(library(service/lotus_api)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(solution_sequences)).

:- rdf_meta
   q_link(+, o, -, -),
   q_link_objects(+, r, r).





%! q_link(+Backend, +Term, -Score, -Iri) is nondet.

q_link(Backend, Lit, Score, Iri) :-
  q_literal(Lit, D, Lex, LTag),
  q_link(Backend, D, Lex, LTag, Score, Iri).


q_link(fct, _, Lex, _, Score, Iri) :- !,
  fct_label(Lex, Score, Iri).
q_link(lotus, _, Lex, LTag, 1, Iri) :-
  (nonvar(LTag) -> lotus(Lex, Iri, [ltag(LTag)]) ; lotus(Lex, Iri)).



%! q_link_objects(+Backend, +P, +G) is det.

q_link_objects(Backend, P, G) :-
  aggregate_all(
    set(From),
    (
      q(trp, _, P, From, G),
      q_is_literal(From)
    ),
    Froms
  ),
  length(Froms, NumFroms),
  msg_notification("We're going to link ~D terms…~n", [NumFroms]),
  catch(q_link_objects_loop(Backend, 1, NumFroms, Froms, P, G), E, true),
  (E == user_quits -> !, true ; true),
  q_view2store(trp, G),
  q_store2view(hdt, G).


q_link_objects_loop(_, _, _, [], _, _) :- !,
  msg_success("We're done linking!~n").
q_link_objects_loop(Backend, M1, N, [H|T], P, G) :-
  dcg_with_output_to(user_output, (
    str("("),
    thousands(M1),
    str("/"),
    thousands(N),
    str(") Let's link "),
    sq(dcg_q_print_literal(H)),
    str(":"),
    nl
  )),
  q_link_object_loop(Backend, H, P, G),
  M2 is M1 + 1,
  q_link_objects_loop(Backend, M2, N, T, P, G).


q_link_object_loop(Backend, From, P, G) :-
  q_link(Backend, From, Score, To), % NONDET
  q_user_chooses_iri(Backend, From, Score, To), !,
  qu_replace_object(From, P, G, To),
  msg_linked(From, To).
q_link_object_loop(_, From, _, _) :-
  dcg_with_output_to(user_output, (
    str("Could not link "),
    sq(dcg_q_print_literal(From)),
    str("."),
    nl
  )).





% HEPERS %

%! msg_linked(+Term, +Iri) is det.

msg_linked(Term, Iri) :-
  dcg_with_output_to(string(Msg), (
    str("Linked "),
    sq(dcg_q_print_term(Term)),
    str(" to "),
    sq(dcg_q_print_iri(Iri)),
    str(".")
  )),
  dcg_with_output_to(user_output, (
    ansi_str([1,37,43], Msg),
    nl,
    nl
  )).



%! print_info(+Backend, +Iri) is det.
%
% Enumerate classes C for instance I.

print_info(fct, S) :- !,
  aggregate_all(set(Triple), q_deref_triple(S, Triple), Triples),
  q_print_triples(Triples).
print_info(lotus, I) :-
  % @bug RDF prefix expansion does not work inside distinct/2.
  rdf_equal(rdf:type, P),
  forall(
    distinct(C, ll_ldf(I, P, C)),
    (write("  - "), q_print_object(C), nl)
  ).



%! q_user_chooses_iri(+Backend, +Lit, +Score, +Iri) is semidet.
%
% User accedes that IRI is the correct term WRT a given linking task.

q_user_chooses_iri(Backend, Lit, Score, Iri) :-
  dcg_with_output_to(user_output, (
    str("Evidence for linking "),
    sq(dcg_q_print_literal(Lit)),
    str(" to "),
    sq(dcg_q_print_iri(Iri)),
    str(":"),
    nl
  )),
  print_info(Backend, Iri),
  dcg_with_output_to(string(Msg), (
    str("["),
    number(Score),
    str("] Do you want to replace literal "),
    sq(dcg_q_print_literal(Lit)),
    str(" with IRI "),
    sq(dcg_q_print_iri(Iri)),
    str(" (Y/N)?")
  )),
  (user_input(Msg) -> !, true ; fail).
