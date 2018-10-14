%! q_link(+Backend, +P, +G) is det.

q_link(Backend, P, G) :-
  q_link_objects(Backend, P, G).
  
:- module(
  rdf_link,
  [
    rdf_link/4,        % +Backend, +Term, -Score, -Iri
    rdf_link_objects/3 % +Backend, +P, +G
  ]
).

/** <module> Quine: Term linking

Literals are substituted by IRIs.

IRIs are linked using ‘owl:sameAs’.

@author Wouter Beek
@version 2016/10-2017/01
*/

:- use_module(library(aggregate)).
:- use_module(library(dcg_cli)).
:- use_module(library(dcg)).
:- use_module(library(print_ext)).
:- use_module(library(rdf/rdf_api)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(service/fct)).
:- use_module(library(service/ll_api)).
:- use_module(library(service/lotus_api)).
:- use_module(library(solution_sequences)).
:- use_module(library(trp/trp_update)).

:- rdf_meta
   rdf_link(+, o, -, -),
   rdf_link_objects(+, r, r).





%! rdf_link(+Backend, +Term, -Score, -Iri) is nondet.

rdf_link(Backend, Lit, Score, Iri) :-
  rdf_literal(Lit, D, Lex, LTag),
  rdf_link(Backend, D, Lex, LTag, Score, Iri).


rdf_link(fct, _, Lex, _, Score, Iri) :- !,
  fct_label(Lex, Score, Iri).
rdf_link(lotus, _, Lex, LTag, 1, Iri) :-
  (nonvar(LTag) -> lotus(Lex, Iri, [ltag(LTag)]) ; lotus(Lex, Iri)).



%! rdf_link_objects(+Backend, +P, +G) is det.

rdf_link_objects(Backend, P, G) :-
  aggregate_all(
    set(From),
    (
      rdf(trp, _, P, From, G),
      rdf_is_literal(From)
    ),
    Froms
  ),
  length(Froms, NumFroms),
  msg_notification("We're going to link ~D terms…~n", [NumFroms]),
  catch(rdf_link_objects_loop(Backend, 1, NumFroms, Froms, P, G), E, true),
  (E == user_quits -> ! ; true),
  rdf_view2store(trp, G),
  rdf_store2view(hdt, G).


rdf_link_objects_loop(_, _, _, [], _, _) :- !,
  msg_success("We're done linking!~n").
rdf_link_objects_loop(Backend, M1, N, [H|T], P, G) :-
  dcg_with_output_to(user_output, (
    atom("("),
    thousands(M1),
    atom("/"),
    thousands(N),
    atom(") Let's link "),
    sq(dcg_rdf_print_literal(H)),
    atom(":"),
    nl
  )),
  rdf_link_object_loop(Backend, H, P, G),
  M2 is M1 + 1,
  rdf_link_objects_loop(Backend, M2, N, T, P, G).


rdf_link_object_loop(Backend, From, P, G) :-
  rdf_link(Backend, From, Score, To), % NONDET
  rdf_user_chooses_iri(Backend, From, Score, To), !,
  qu_replace_object(From, P, G, To),
  msg_linked(From, To).
rdf_link_object_loop(_, From, _, _) :-
  dcg_with_output_to(user_output, (
    atom("Could not link "),
    sq(dcg_rdf_print_literal(From)),
    atom("."),
    nl
  )).





% HEPERS %

%! msg_linked(+Term, +Iri) is det.

msg_linked(Term, Iri) :-
  dcg_with_output_to(string(Msg), (
    atom("Linked "),
    sq(dcg_rdf_print_term(Term)),
    atom(" to "),
    sq(dcg_rdf_print_iri(Iri)),
    atom(".")
  )),
  dcg_with_output_to(user_output, (
    ansi_string([1,37,43], Msg),
    nl,
    nl
  )).



%! print_info(+Backend, +Iri) is det.
%
% Enumerate classes C for instance I.

print_info(fct, S) :- !,
  aggregate_all(set(Triple), rdf_deref_triple(S, Triple), Triples),
  rdf_print_triples(Triples).
print_info(lotus, I) :-
  % @bug RDF prefix expansion does not work inside distinct/2.
  rdf_equal(rdf:type, P),
  forall(
    distinct(C, ll_ldf(I, P, C)),
    (write("  - "), rdf_print_object(C), nl)
  ).



%! rdf_user_chooses_iri(+Backend, +Lit, +Score, +Iri) is semidet.
%
% User accedes that IRI is the correct term WRT a given linking task.

rdf_user_chooses_iri(Backend, Lit, Score, Iri) :-
  dcg_with_output_to(user_output, (
    atom("Evidence for linking "),
    sq(dcg_rdf_print_literal(Lit)),
    atom(" to "),
    sq(dcg_rdf_print_iri(Iri)),
    atom(":"),
    nl
  )),
  print_info(Backend, Iri),
  dcg_with_output_to(string(Msg), (
    atom("["),
    number(Score),
    atom("] Do you want to replace literal "),
    sq(dcg_rdf_print_literal(Lit)),
    atom(" with IRI "),
    sq(dcg_rdf_print_iri(Iri)),
    atom(" (Y/N)?")
  )),
  (user_input(Msg) -> ! ; fail).
