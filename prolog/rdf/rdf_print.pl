:- module(
  rdf_print,
  [
    rdf_print_graph/1,      % +G
    rdf_print_graph_term/1, % +G
    rdf_print_object//1,    % +G
    rdf_print_predicate//1, % +G
    rdf_print_quad/4,       % +S, +P, +O, +G
    rdf_print_quads/3,      % ?S, ?P, ?O
    rdf_print_quads/4,      % ?S, ?P, ?O, ?G
    rdf_print_term/1,       % +T
    rdf_print_term//1,      % +T
    rdf_print_triple/3,     % +S, +P, +O
    rdf_print_triple//3,    % +S, +P, +O
    rdf_print_triples/3,    % ?S, ?P, ?O
    rdf_print_triples/4,    % ?S, ?P, ?O, ?G
    rdf_print_tuples/1      % +Tuples
  ]
).

/** <module> RDF print

Print RDF statements.

@author Wouter Beek
@version 2016/03
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(semweb/rdf11)).

:- rdf_meta
   rdf_print_graph(r),
   rdf_print_graph_term(r),
   rdf_print_object(o, ?, ?),
   rdf_print_predicate(r, ?, ?),
   rdf_print_quad(r, r, o, r),
   rdf_print_quads(r, r, o),
   rdf_print_quads(r, r, o, r),
   rdf_print_term(o),
   rdf_print_term(o, ?, ?),
   rdf_print_triple(r, r, o),
   rdf_print_triple(r, r, o, ?, ?),
   rdf_print_triples(r, r, o),
   rdf_print_triples(r, r, o, r),
   rdf_print_tuples(r, r, o),
   rdf_print_tuples(r, r, o, r).





rdf_print_graph(G) :-
  rdf_print_triples(_, _, _, G).



rdf_print_quad(S, P, O, G) :-
  rdf_print_tuples([rdf(S,P,O,G)]).



rdf_print_quads(S, P, O) :-
  rdf_print_quads(S, P, O, _).


rdf_print_quads(S, P, O, G) :-
  aggregate_all(set(G-rdf(S,P,O)), rdf(S, P, O, G), SortedPairs),
  rdf_print_sorted_pairs0(SortedPairs).



rdf_print_triple(S, P, O) :-
  rdf_print_tuples([rdf(S,P,O)]).


rdf_print_triple(S, P, O) -->
  dcg_goal(rdf_print_triple(S, P, O)).



rdf_print_triples(S, P, O) :-
  rdf_print_triples(S, P, O, _).


rdf_print_triples(S, P, O, G) :-
  aggregate_all(set(rdf(S,P,O)), rdf(S, P, O, G), Triples),
  rdf_default_graph(G),
  rdf_print_groups0([G-Triples]).



rdf_print_tuples(Tuples) :-
  maplist(graph_triple_pair0, Tuples, Pairs),
  keysort(Pairs, SortedPairs),
  rdf_print_sorted_pairs0(SortedPairs).

graph_triple_pair0(rdf(S,P,O), G-rdf(S,P,O)) :-
  rdf_default_graph(G).
graph_triple_pair0(rdf(S,P,O,G), G-rdf(S,P,O)).

rdf_print_sorted_pairs0(SortedPairs) :-
  group_pairs_by_key(SortedPairs, Groups),
  rdf_print_groups0(Groups).

rdf_print_groups0(Groups) :-
  forall(member(G-Triples, Groups), rdf_print_graph(Triples, G)).



rdf_print_graph(Triples, G) :-
  (   rdf_default_graph(G)
  ->  I = 0
  ;   rdf_print_graph_term(G),
      write(" {\n"),
      I = 1
  ),
  rdf_print_triples(I, Triples),
  (rdf_default_graph(G) -> true ; write("}\n")).



rdf_print_triples(I, Triples) :-
  aggregate_all(set(S-po(P,O)), member(rdf(S,P,O), Triples), SortedPairs),
  group_pairs_by_key(SortedPairs, Groups),
  forall(member(S-POs, Groups), rdf_print_subject(I, S, POs)).



rdf_print_subject(I1, S, POs) :-
  tab0(I1),
  rdf_print_subject(S),
  aggregate_all(set(P-O), member(po(P,O), POs), SortedPairs),
  group_pairs_by_key(SortedPairs, Groups),
  (   Groups = [P-Os]
  ->  write(" "),
      rdf_print_predicate(P, Os),
      writeln(" .")
  ;   nl,
      I2 is I1 + 1,
      rdf_print_predicates(I2, Groups)
  ).



rdf_print_predicate(P, Os) :-
  rdf_print_predicate(P),
  write(" "),
  rdf_print_objects(Os).



rdf_print_predicates(I, [P-Os]) :-
  tab0(I),
  rdf_print_predicate(P, Os),
  writeln(" .").
rdf_print_predicates(I, [P-Os|T]) :-
  tab0(I),
  rdf_print_predicate(P, Os),
  writeln(" ;"),
  rdf_print_predicates(I, T).



rdf_print_objects([H]) :- !,
  rdf_print_object(H).
rdf_print_objects([H|T]) :-
  rdf_print_object(H),
  write(", "),
  rdf_print_objects(T).





% TERMS BY POSITION

rdf_print_graph_term(G) :-
  rdf_print_iri(G).



rdf_print_object(O) :-
  rdf_is_literal(O), !,
  rdf_print_literal(O).
rdf_print_object(O) :-
  rdf_print_subject(O).


rdf_print_object(O) -->
  dcg_goal(rdf_print_object(O)).



rdf_print_predicate(P) :-
  rdf_print_iri(P).


rdf_print_predicate(P) -->
  dcg_goal(rdf_print_predicate(P)).



rdf_print_subject(S) :-
  rdf_is_bnode(S), !,
  rdf_print_bnode(S).
rdf_print_subject(S) :-
  rdf_print_iri(S).



rdf_print_term(T) :-
  rdf_print_object(T).


rdf_print_term(T) -->
  dcg_goal(rdf_print_term(T)).





% TERMS BY KIND

rdf_print_bnode(B) :-
  write(B).



rdf_print_iri(Full) :-
  rdf_global_id(Alias:Local, Full), !,
  write(Alias),
  write(":"),
  write(Local).
rdf_print_iri(Full) :-
  write("<"),
  write(Full),
  write(">").



rdf_print_literal(Lex^^D) :-
  rdf_equal(xsd:boolean, D), !,
  turtle:turtle_write_quoted_string(current_output, Lex).
rdf_print_literal(V^^D) :-
  rdf_equal(xsd:string, D), !,
  atom_string(Lex, V),
  turtle:turtle_write_quoted_string(current_output, Lex).
rdf_print_literal(V^^D) :-
  (   rdf_equal(xsd:integer, D)
  ;   rdf_equal(xsd:decimal, D)
  ;   rdf_equal(xsd:double, D)
  ), !,
  atom_number(Lex, V),
  turtle:turtle_write_quoted_string(current_output, Lex).
rdf_print_literal(V^^D) :- !,
  rdf_literal_lexical_form(V^^D, Lex),
  turtle:turtle_write_quoted_string(current_output, Lex),
  write("^^"),
  rdf_print_iri(D).
rdf_print_literal(V@LTag) :- !,
  atom_string(Lex, V),
  turtle:turtle_write_quoted_string(current_output, Lex),
  write("@"),
  write(LTag).
rdf_print_literal(V) :-
  rdf_print_literal(V^^xsd:string).





% HELPERS %

tab0(N1) :- N2 is N1 * 4, tab(N2).
