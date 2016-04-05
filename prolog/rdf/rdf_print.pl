:- module(
  rdf_print,
  [
    dcg_print_describe//1,   % ?S
    dcg_print_describe//2,   % ?S,             +Opts
    dcg_print_graph//1,      % +G
    dcg_print_graph//2,      % +G,             +Opts
    dcg_print_graph_term//1, % +G
    dcg_print_graph_term//2, % +G,             +Opts
    dcg_print_object//1,     % +O
    dcg_print_object//2,     % +O,             +Opts
    dcg_print_predicate//1,  % +P
    dcg_print_predicate//2,  % +P,             +Opts
    dcg_print_quad//4,       % +S, +P, +O, +G
    dcg_print_quad//5,       % +S, +P, +O, +G, +Opts
    dcg_print_quads//1,      % +Tuples
    dcg_print_quads//2,      % +Tuples,        +Opts
    dcg_print_quads//4,      % ?S, ?P, ?O, ?G
    dcg_print_quads//5,      % ?S, ?P, ?O, ?G, +Opts
    dcg_print_term//1,       % +T
    dcg_print_term//2,       % +T,             +Opts
    dcg_print_triple//3,     % +S, +P, +O
    dcg_print_triple//4,     % +S, +P, +O,     +Opts
    dcg_print_triples//1,    % +Triples
    dcg_print_triples//2,    % +Triples,       +Opts
    dcg_print_triples//3,    % ?S, ?P, ?O
    dcg_print_triples//4,    % ?S, ?P, ?O, ?G
    dcg_print_triples//5,    % ?S, ?P, ?O, ?G, +Opts
    rdf_print_describe/1,    % ?S
    rdf_print_describe/2,    % ?S,             +Opts
    rdf_print_graph/1,       % +G
    rdf_print_graph/2,       % +G,             +Opts
    rdf_print_graph_term/1,  % +G
    rdf_print_graph_term/2,  % +G,             +Opts
    rdf_print_object/1,      % +O
    rdf_print_object/2,      % +O,             +Opts
    rdf_print_predicate/1,   % +P
    rdf_print_predicate/2,   % +P,             +Opts
    rdf_print_quad/4,        % +S, +P, +O, +G
    rdf_print_quad/5,        % +S, +P, +O, +G, +Opts
    rdf_print_quads/1,       % +Tuples
    rdf_print_quads/2,       % +Tuples,        +Opts
    rdf_print_quads/4,       % ?S, ?P, ?O, ?G
    rdf_print_quads/5,       % ?S, ?P, ?O, ?G, +Opts
    rdf_print_term/1,        % +T
    rdf_print_term/2,        % +T,             +Opts
    rdf_print_triple/3,      % +S, +P, +O
    rdf_print_triple/4,      % +S, +P, +O,     +Opts
    rdf_print_triples/1,     % +Triples
    rdf_print_triples/2,     % +Triples,       +Opts
    rdf_print_triples/3,     % ?S, ?P, ?O
    rdf_print_triples/4,     % ?S, ?P, ?O, ?G
    rdf_print_triples/5      % ?S, ?P, ?O, ?G, +Opts
  ]
).

/** <module> RDF print

Print RDF statements.

| **Key**      | **Value** |
| `indent`     | nonneg    |
| `max_length` | nonneg    |

@author Wouter Beek
@tbd Turtle container abbreviation.
@tbd Turtle collection abbreviation.
@version 2016/03-2016/04
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(semweb/rdf11)).

:- rdf_meta
   dcg_print_describe(r, ?, ?),
   dcg_print_describe(r, +, ?, ?),
   dcg_print_graph(r, ?, ?),
   dcg_print_graph(r, +, ?, ?),
   dcg_print_graph_term(r, ?, ?),
   dcg_print_graph_term(r, +, ?, ?),
   dcg_print_object(o, ?, ?),
   dcg_print_object(o, +, ?, ?),
   dcg_print_predicate(r, ?, ?),
   dcg_print_predicate(r, +, ?, ?),
   dcg_print_quad(r, r, o, r, ?, ?),
   dcg_print_quad(r, r, o, r, +, ?, ?),
   dcg_print_quads(r, r, o, r, ?, ?),
   dcg_print_quads(r, r, o, r, +, ?, ?),
   dcg_print_term(o, ?, ?),
   dcg_print_term(o, +, ?, ?),
   dcg_print_triple(r, r, o, ?, ?),
   dcg_print_triple(r, r, o, +, ?, ?),
   dcg_print_triples(r, r, o, ?, ?),
   dcg_print_triples(r, r, o, r, ?, ?),
   dcg_print_triples(r, r, o, r, +, ?, ?),
   rdf_print_describe(r),
   rdf_print_describe(r, +),
   rdf_print_graph(r),
   rdf_print_graph(r, +),
   rdf_print_graph_term(r),
   rdf_print_graph_term(r, +),
   rdf_print_object(o),
   rdf_print_object(o, +),
   rdf_print_predicate(r),
   rdf_print_predicate(r, +),
   rdf_print_quad(r, r, o, r),
   rdf_print_quad(r, r, o, r, +),
   rdf_print_quads(r, r, o, r),
   rdf_print_quads(r, r, o, r, +),
   rdf_print_term(o),
   rdf_print_term(o, +),
   rdf_print_triple(r, r, o),
   rdf_print_triple(r, r, o, +),
   rdf_print_triples(r, r, o),
   rdf_print_triples(r, r, o, r),
   rdf_print_triples(r, r, o, r, +).





% NON-DCG INVOCATIONS %

rdf_print_describe(G) :-
  dcg_with_output_to(current_output, dcg_print_describe(G)).

rdf_print_describe(G, Opts) :-
  dcg_with_output_to(current_output, dcg_print_describe(G, Opts)).

rdf_print_graph(G) :-
  dcg_with_output_to(current_output, dcg_print_graph(G)).

rdf_print_graph(G, Opts1) :-
  mod_dict(out, Opts1, current_output, Out, Opts2),
  dcg_with_output_to(Out, dcg_print_graph(G, Opts2)).

rdf_print_graph_term(G) :-
  dcg_with_output_to(current_output, dcg_print_graph_term(G)).

rdf_print_graph_term(G, Opts1) :-
  mod_dict(out, Opts1, current_output, Out, Opts2),
  dcg_with_output_to(Out, dcg_print_graph_term(G, Opts2)).

rdf_print_object(O) :-
  dcg_with_output_to(current_output, dcg_print_object(O)).

rdf_print_object(O, Opts1) :-
  mod_dict(out, Opts1, current_output, Out, Opts2),
  dcg_with_output_to(Out, dcg_print_object(O, Opts2)).

rdf_print_predicate(P) :-
  dcg_with_output_to(current_output, dcg_print_predicate(P)).

rdf_print_predicate(P, Opts1) :-
  mod_dict(out, Opts1, current_output, Out, Opts2),
  dcg_with_output_to(Out, dcg_print_predicate(P, Opts2)).

rdf_print_quad(S, P, O, G) :-
  dcg_with_output_to(current_output, dcg_print_quad(S, P, O, G)).

rdf_print_quad(S, P, O, G, Opts1) :-
  mod_dict(out, Opts1, current_output, Out, Opts2),
  dcg_with_output_to(Out, dcg_print_quad(S, P, O, G, Opts2)).

rdf_print_quads(Tuples) :-
  dcg_with_output_to(current_output, dcg_print_quads(Tuples)).

rdf_print_quads(Tuples, Opts1) :-
  mod_dict(out, Opts1, current_output, Out, Opts2),
  dcg_with_output_to(Out, dcg_print_quads(Tuples, Opts2)).

rdf_print_quads(S, P, O, G) :-
  dcg_with_output_to(current_output, dcg_print_quads(S, P, O, G)).

rdf_print_quads(S, P, O, G, Opts1) :-
  mod_dict(out, Opts1, current_output, Out, Opts2),
  dcg_with_output_to(Out, dcg_print_quads(S, P, O, G, Opts2)).

rdf_print_term(T) :-
  dcg_with_output_to(current_output, dcg_print_term(T)).

rdf_print_term(T, Opts1) :-
  mod_dict(out, Opts1, current_output, Out, Opts2),
  dcg_with_output_to(Out, dcg_print_term(T, Opts2)).

rdf_print_triple(S, P, O) :-
  dcg_with_output_to(current_output, dcg_print_triple(S, P, O)).

rdf_print_triple(S, P, O, Opts1) :-
  mod_dict(out, Opts1, current_output, Out, Opts2),
  dcg_with_output_to(Out, dcg_print_triple(S, P, O, Opts2)).

rdf_print_triples(Triples) :-
  dcg_with_output_to(current_output, dcg_print_triples(Triples)).

rdf_print_triples(Triples, Opts1) :-
  mod_dict(out, Opts1, current_output, Out, Opts2),
  dcg_with_output_to(Out, dcg_print_triples(Triples, Opts2)).

rdf_print_triples(S, P, O) :-
  dcg_with_output_to(current_output, dcg_print_triples(S, P, O)).

rdf_print_triples(S, P, O, G) :-
  dcg_with_output_to(current_output, dcg_print_triples(S, P, O, G)).

rdf_print_triples(S, P, O, G, Opts1) :-
  mod_dict(out, Opts1, current_output, Out, Opts2),
  dcg_with_output_to(Out, dcg_print_triples(S, P, O, G, Opts2)).





% PRINT MULTIPLE TUPLES %

dcg_print_describe(S) -->
  dcg_print_describe(S, _{}).


dcg_print_describe(S, Opts) -->
  dcg_print_triples(S, _, _, _, Opts).



dcg_print_graph(G) -->
  dcg_print_graph(G, _{}).


dcg_print_graph(G, Opts) -->
  dcg_print_triples(_, _, _, G, Opts).



dcg_print_quads(Tuples) -->
  dcg_print_quads(Tuples, _{}).


dcg_print_quads(Tuples, Opts) -->
  {
    maplist(graph_triple_pair0, Tuples, Pairs),
    keysort(Pairs, SortedPairs)
  },
  dcg_print_sorted_pairs0(SortedPairs, Opts).

graph_triple_pair0(rdf(S,P,O), G-rdf(S,P,O)) :-
  rdf_default_graph(G).
graph_triple_pair0(rdf(S,P,O,G), G-rdf(S,P,O)).



dcg_print_quads(S, P, O, G) -->
  dcg_print_quads(S, P, O, G, _{}).


dcg_print_quads(S, P, O, G, Opts) -->
  {aggregate_all(set(G-rdf(S,P,O)), rdf(S, P, O, G), SortedPairs)},
  dcg_print_sorted_pairs0(SortedPairs, Opts).



dcg_print_triples(Triples) -->
  dcg_print_triples(Triples, _{}).


dcg_print_triples(Triples, Opts) -->
  {rdf_default_graph(G)},
  dcg_print_groups0([G-Triples], Opts).


dcg_print_triples(S, P, O) -->
  dcg_print_triples(S, P, O, _).


dcg_print_triples(S, P, O, G) -->
  dcg_print_triples(S, P, O, G, _{}).


dcg_print_triples(S, P, O, G, Opts) -->
  {aggregate_all(set(rdf(S,P,O)), rdf(S, P, O, G), Triples)},
  dcg_print_triples(Triples, Opts).


dcg_print_sorted_pairs0(SortedPairs, Opts) -->
  {group_pairs_by_key(SortedPairs, Groups)},
  dcg_print_groups0(Groups, Opts).


dcg_print_groups0([], _) --> !, [].
dcg_print_groups0([G-Triples|Groups], Opts) -->
  {get_dict(indent, Opts, I1, 0)},
  (   {rdf_default_graph(G)}
  ->  {I2 = I1}
  ;   tab(I1),
      dcg_print_graph_term(G, Opts),
      " {\n",
      {I2 = I1 + 1}
  ),
  dcg_print_triples0(I2, Triples, Opts),
  ({rdf_default_graph(G)} -> "" ; "}\n"),
  dcg_print_groups0(Groups, Opts).


dcg_print_triples0(I, Triples, Opts) -->
  {
    aggregate_all(set(S-po(P,O)), member(rdf(S,P,O), Triples), SortedPairs),
    group_pairs_by_key(SortedPairs, Groups)
  },
  dcg_print_subjects0(I, Groups, Opts).


dcg_print_subjects0(_, [], _) --> !, [].
dcg_print_subjects0(I1, [S-POs|Groups1], Opts) -->
  tab(I1),
  dcg_print_subject(S, Opts),
  {
    aggregate_all(set(P-O), member(po(P,O), POs), SortedPairs),
    group_pairs_by_key(SortedPairs, Groups2)
  },
  ({Groups2 = [_]} -> " ", {I2 = 0} ; nl, {I2 is I1 + 1}),
  dcg_print_predicates0(I2, Groups2, Opts),
  dcg_print_subjects0(I1, Groups1, Opts).


dcg_print_predicates0(_, [], _) --> !, [].
dcg_print_predicates0(I1, [P-Os|T], Opts) -->
  tab(I1),
  dcg_print_predicate(P, Opts),
  " ",
  (   {Os = [O]}
  ->  dcg_print_object(O, Opts)
  ;   {
        I2 is I1 + 1,
        Os = [O|Os0]
      },
      dcg_print_object(O, Opts),
      nl,
      dcg_print_objects0(I2, Os0, Opts)
  ),
  " ",
  ({T == []} -> "." ; ";"),
  nl,
  dcg_print_predicates0(I1, T, Opts).

dcg_print_objects0(_, [], _) --> !, [].
dcg_print_objects0(I, [O|T], Opts) -->
  tab(I),
  dcg_print_object(O, Opts),
  ({T == []} -> "" ; " ,", nl),
  dcg_print_objects0(I, T, Opts).





% PRINT A SINGLE TUPLE %

dcg_print_quad(S, P, O, G) -->
  dcg_print_quad(S, P, O, G, _{}).


dcg_print_quad(S, P, O, G, Opts) -->
  dcg_print_quads([rdf(S,P,O,G)], Opts).



dcg_print_triple(S, P, O) -->
  dcg_print_triple(S, P, O, _{}).


dcg_print_triple(S, P, O, Opts) -->
  dcg_print_triples([rdf(S,P,O)], Opts).





% PRINT A TERM BY IT POSITIONALITY %

dcg_print_graph_term(G) -->
  dcg_print_graph_term(G, _{}).


dcg_print_graph_term(G, Opts) -->
  dcg_print_iri(G, Opts).



dcg_print_object(O) -->
  dcg_print_object(O, _{}).


dcg_print_object(O, Opts) -->
  {rdf_is_literal(O)}, !,
  dcg_print_literal(O, Opts).
dcg_print_object(O, Opts) -->
  dcg_print_subject(O, Opts).



dcg_print_predicate(P) -->
  dcg_print_predicate(P, _{}).


dcg_print_predicate(P, Opts) -->
  dcg_print_iri(P, Opts).



dcg_print_subject(S, Opts) -->
  {rdf_is_bnode(S)}, !,
  dcg_print_bnode(S, Opts).
dcg_print_subject(S, Opts) -->
  dcg_print_iri(S, Opts).



dcg_print_term(T) -->
  dcg_print_term(T, _{}).


dcg_print_term(T, Opts) -->
  dcg_print_object(T, Opts).





% PRINT A TERM BY ITS KIND %

dcg_print_bnode(B, Opts) -->
  dcg_print_truncated_atom(B, Opts).



dcg_print_datatype_iri(D, Opts) -->
  dcg_print_iri(D, Opts).



dcg_print_iri(Full, Opts) -->
  {rdf_global_id(Alias:Local1, Full)}, !,
  {
    get_dict(max_length, Opts, Max1, inf),
    atom_length(Alias, AliasLen),
    Minus is AliasLen + 1,
    inf_minus(Max1, Minus, Max2),
    atom_truncate(Local1, Max2, Local2)
  },
  atom(Alias),
  ":",
  atom(Local2).
dcg_print_iri(Full, Opts) -->
  "<",
  dcg_print_truncated_atom(Full, Opts),
  ">".



dcg_print_language_tag(LTag, Opts) -->
  dcg_print_truncated_atom(LTag, Opts).



dcg_print_lexical_form(Lex, Opts) -->
  "\"",
  dcg_print_truncated_atom(Lex, Opts),
  "\"".


% Abbreviate XSD Boolean.
dcg_print_literal(Lex^^D, Opts) -->
  {rdf_equal(xsd:boolean, D)}, !,
  dcg_print_lexical_form(Lex, Opts).
% Abbreviate XSD string.
dcg_print_literal(V^^D, Opts) -->
  {rdf_equal(xsd:string, D)}, !,
  {atom_string(Lex, V)},
  dcg_print_lexical_form(Lex, Opts).
% Abbreviate XSD decimal, double and integer.
dcg_print_literal(V^^D, Opts) -->
  {(  rdf_equal(xsd:integer, D)
  ;   rdf_equal(xsd:decimal, D)
  ;   rdf_equal(xsd:double, D)
  )}, !,
  {atom_number(Lex, V)},
  dcg_print_lexical_form(Lex, Opts).
% Unabbreviated datatype IRI that is not `rdf:langString`.
dcg_print_literal(V^^D, Opts) --> !,
  {rdf_literal_lexical_form(V^^D, Lex)},
  dcg_print_lexical_form(Lex, Opts),
  "^^",
  dcg_print_datatype_iri(D, Opts).
% Language-tagged string datatype IRI.
dcg_print_literal(V@LTag, Opts) --> !,
  {atom_string(Lex, V)},
  dcg_print_lexical_form(Lex, Opts),
  "@",
  dcg_print_language_tag(LTag, Opts).





% HELPERS %

dcg_print_truncated_atom(A1, Opts) -->
  {
    get_dict(max_length, Opts, Max, inf),
    atom_truncate(A1, Max, A2)
  },
  atom(A2).



inf_minus(inf, _, inf) :- !.
inf_minus(X, Y, X)     :- X =< Y, !.
inf_minus(X, Y, Z)     :- Z is X - Y.
