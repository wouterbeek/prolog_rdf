:- module(
  rdf_print,
  [
    dcg_print_graph//1,      % +G
    dcg_print_graph//2,      % +G,             +Opts
    dcg_print_graph_term//1, % +G
    dcg_print_graph_term//2, % +G,             +Opts
    dcg_print_literal//1,    % +Lit
    dcg_print_literal//2,    % +Lit,           +Opts
    dcg_print_object//1,     % +O
    dcg_print_object//2,     % +O,             +Opts
    dcg_print_predicate//1,  % +P
    dcg_print_predicate//2,  % +P,             +Opts
    dcg_print_quad//1,       % +Tuple
    dcg_print_quad//2,       % +Tuple,         +Opts
    dcg_print_quad//4,       % +S, +P, +O, +G
    dcg_print_quad//5,       % +S, +P, +O, +G, +Opts
    dcg_print_quads//1,      % +Tuples
    dcg_print_quads//2,      % +Tuples,        +Opts
    dcg_print_quads//4,      % ?S, ?P, ?O, ?G
    dcg_print_quads//5,      % ?S, ?P, ?O, ?G, +Opts
    dcg_print_term//1,       % +T
    dcg_print_term//2,       % +T,             +Opts
    dcg_print_triple//1,     % +Tuple
    dcg_print_triple//2,     % +Tuple,         +Opts
    dcg_print_triple//3,     % +S, +P, +O
    dcg_print_triple//4,     % +S, +P, +O,     +Opts
    dcg_print_triples//1,    % +Triples
    dcg_print_triples//2,    % +Triples,       +Opts
    dcg_print_triples//3,    % ?S, ?P, ?O
    dcg_print_triples//4,    % ?S, ?P, ?O, ?G
    dcg_print_triples//5,    % ?S, ?P, ?O, ?G, +Opts
    rdf_print_graph/1,       % +G
    rdf_print_graph/2,       % +G,             +Opts
    rdf_print_graph_term/1,  % +G
    rdf_print_graph_term/2,  % +G,             +Opts
    rdf_print_object/1,      % +O
    rdf_print_object/2,      % +O,             +Opts
    rdf_print_predicate/1,   % +P
    rdf_print_predicate/2,   % +P,             +Opts
    rdf_print_quad/1,        % +Tuple
    rdf_print_quad/2,        % +Tuple,         +Opts
    rdf_print_quad/4,        % +S, +P, +O, +G
    rdf_print_quad/5,        % +S, +P, +O, +G, +Opts
    rdf_print_quads/1,       % +Tuples
    rdf_print_quads/2,       % +Tuples,        +Opts
    rdf_print_quads/4,       % ?S, ?P, ?O, ?G
    rdf_print_quads/5,       % ?S, ?P, ?O, ?G, +Opts
    rdf_print_table/1,       % +Rows,
    rdf_print_table/2,       % +Rows,          +Opts
    rdf_print_term/1,        % +T
    rdf_print_term/2,        % +T,             +Opts
    rdf_print_triple/1,      % +Tuple
    rdf_print_triple/2,      % +Tuple,         +Opts
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

| **Key**         | **Value** | **Default** | **Description**                 |
|:----------------|:----------|:-----------:|:--------------------------------|
| `bnode_map`     | boolean   | `true`      | Whether or not blank node names |
|                 |           |             | are replaced by small integers. |
| `indent`        | nonneg    | 0           |                                 |
| `iri_lbl`       | boolean   | `false`     | Whether or not the prefered     |
|                 |           |             | label is used i.o. the IRI.     |
| `max_length`    | nonneg    | `inf`       |                                 |

@author Wouter Beek
@tbd Turtle container abbreviation.
@tbd Turtle collection abbreviation.
@version 2016/03-2016/06
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_pl)).
:- use_module(library(dcg/dcg_table)).
:- use_module(library(dict_ext)).
:- use_module(library(lists)).
:- use_module(library(pair_ext)).
:- use_module(library(print_ext)).
:- use_module(library(rdf/rdf_bnode_map)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(rdfs/rdfs_ext)).
:- use_module(library(semweb/rdf11)).

:- rdf_meta
   dcg_print_graph(r, ?, ?),
   dcg_print_graph(r, +, ?, ?),
   dcg_print_graph_term(r, ?, ?),
   dcg_print_graph_term(r, +, ?, ?),
   dcg_print_literal(o, ?, ?),
   dcg_print_literal(o, +, ?, ?),
   dcg_print_object(o, ?, ?),
   dcg_print_object(o, +, ?, ?),
   dcg_print_predicate(r, ?, ?),
   dcg_print_predicate(r, +, ?, ?),
   dcg_print_quad(t, ?, ?),
   dcg_print_quad(t, +, ?, ?),
   dcg_print_quad(r, r, o, r, ?, ?),
   dcg_print_quad(r, r, o, r, +, ?, ?),
   dcg_print_quads(r, r, o, r, ?, ?),
   dcg_print_quads(r, r, o, r, +, ?, ?),
   dcg_print_term(o, ?, ?),
   dcg_print_term(o, +, ?, ?),
   dcg_print_triple(t, ?, ?),
   dcg_print_triple(t, +, ?, ?),
   dcg_print_triple(r, r, o, ?, ?),
   dcg_print_triple(r, r, o, +, ?, ?),
   dcg_print_triples(r, r, o, ?, ?),
   dcg_print_triples(r, r, o, r, ?, ?),
   dcg_print_triples(r, r, o, r, +, ?, ?),
   rdf_print_graph(r),
   rdf_print_graph(r, +),
   rdf_print_graph_term(r),
   rdf_print_graph_term(r, +),
   rdf_print_object(o),
   rdf_print_object(o, +),
   rdf_print_predicate(r),
   rdf_print_predicate(r, +),
   rdf_print_quad(t),
   rdf_print_quad(t, +),
   rdf_print_quad(r, r, o, r),
   rdf_print_quad(r, r, o, r, +),
   rdf_print_quads(r, r, o, r),
   rdf_print_quads(r, r, o, r, +),
   rdf_print_term(o),
   rdf_print_term(o, +),
   rdf_print_triple(t),
   rdf_print_triple(t, +),
   rdf_print_triple(r, r, o),
   rdf_print_triple(r, r, o, +),
   rdf_print_triples(r, r, o),
   rdf_print_triples(r, r, o, r),
   rdf_print_triples(r, r, o, r, +).

:- dynamic
    var_map/2.





% NON-DCG INVOCATIONS %

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

rdf_print_quad(Tuple) :-
  dcg_with_output_to(current_output, dcg_print_quad(Tuple)).

rdf_print_quad(Tuple, Opts1) :-
  mod_dict(out, Opts1, current_output, Out, Opts2),
  dcg_with_output_to(Out, dcg_print_quad(Tuple, Opts2)).

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

rdf_print_table(Rows) :-
  rdf_print_table(Rows, []).

rdf_print_table(Rows, Opts1) :-
  merge_options([cell(print_cell0)], Opts1, Opts2),
  dcg_with_output_to(current_output, dcg_table(Rows, Opts2)).

print_cell0(Term) --> dcg_print_term(Term), !.
print_cell0(Term) --> term(Term).

rdf_print_term(T) :-
  dcg_with_output_to(current_output, dcg_print_term(T)).

rdf_print_term(T, Opts1) :-
  mod_dict(out, Opts1, current_output, Out, Opts2),
  dcg_with_output_to(Out, dcg_print_term(T, Opts2)).

rdf_print_triple(Tuple) :-
  dcg_with_output_to(current_output, dcg_print_triple(Tuple)).

rdf_print_triple(Tuple, Opts1) :-
  mod_dict(out, Opts1, current_output, Out, Opts2),
  dcg_with_output_to(Out, dcg_print_triple(Tuple, Opts2)).

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

dcg_print_graph(G) -->
  dcg_print_graph(G, _{}).


dcg_print_graph(G, Opts) -->
  dcg_print_quads(_, _, _, G, Opts).



dcg_print_quads(Tuples) -->
  dcg_print_quads(Tuples, _{}).


dcg_print_quads([rdf(S,P,O)], Opts) --> !,
  dcg_print_triples0(0, [rdf(S,P,O,default)], Opts).
dcg_print_quads([rdf(S,P,O,G)], Opts) --> !,
  dcg_print_subject(S, Opts),
  " ",
  dcg_print_predicate(P, Opts),
  " ",
  dcg_print_object(O, Opts),
  " ",
  dcg_print_graph_term(G, Opts),
  " .",
  nl.
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
    group_pairs_by_key(SortedPairs, Groups2),
    I2 is I1 + 1
  },
  dcg_print_predicates1(I2, Groups2, Opts),
  nl,
  dcg_print_subjects0(I1, Groups1, Opts).

% There is exactly one predicate.  Emit it on the same line.
dcg_print_predicates1(I, [P-Os], Opts) --> !,
  " ",
  dcg_print_predicate(P, Opts),
  dcg_print_objects1(I, Os, Opts),
  " .".
dcg_print_predicates1(I, Groups, Opts) -->
  dcg_print_predicates2(I, Groups, Opts).

dcg_print_predicates2(_, [], _) --> !, [].
dcg_print_predicates2(I1, [P-Os|Groups], Opts) -->
  nl,
  tab(I1),
  dcg_print_predicate(P, Opts),
  {I2 is I1 + 1},
  dcg_print_objects1(I2, Os, Opts),
  " ",
  ({Groups == []} -> "." ; ";"),
  dcg_print_predicates2(I1, Groups, Opts).


% There is exactly one object.  Emit it on the same line.
dcg_print_objects1(_, [O], Opts) --> !,
  " ",
  dcg_print_object(O, Opts).
dcg_print_objects1(I, Os, Opts) -->
  dcg_print_objects2(I, Os, Opts).


dcg_print_objects2(_, [], _) --> !, [].
dcg_print_objects2(I, [O|Os], Opts) -->
  nl,
  tab(I),
  dcg_print_object(O, Opts),
  ({Os == []} -> "" ; " ,"),
  dcg_print_objects2(I, Os, Opts).





% PRINT A SINGLE TUPLE %

dcg_print_quad(Tuple) -->
  dcg_print_quad(Tuple, _{}).


dcg_print_quad(rdf(S,P,O), Opts) --> !,
  {rdf_default_graph(G)},
  dcg_print_quad(rdf(S,P,O,G), Opts).
dcg_print_quad(rdf(S,P,O,G), Opts) -->
  dcg_print_quad(S, P, O, G, Opts).


dcg_print_quad(S, P, O, G) -->
  dcg_print_quad(S, P, O, G, _{}).


dcg_print_quad(S, P, O, G, Opts) -->
  dcg_print_quads([rdf(S,P,O,G)], Opts).



dcg_print_triple(Tuple) -->
  dcg_print_triple(Tuple, _{}).


dcg_print_triple(rdf(S,P,O), Opts) --> !,
  dcg_print_triple(S, P, O, Opts).
dcg_print_triple(rdf(S,P,O,_), Opts) -->
  dcg_print_triple(rdf(S,P,O), Opts).


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


dcg_print_predicate(P, _) -->
  {rdf_equal(rdf:type, P)}, !,
  "a".
dcg_print_predicate(P, _) -->
  {var(P)}, !,
  dcg_print_var(P).
dcg_print_predicate(P, Opts) -->
  dcg_print_iri(P, Opts).



dcg_print_subject(S, Opts) -->
  {rdf_is_bnode(S)}, !,
  dcg_print_bnode(S, Opts).
dcg_print_subject(S, _) -->
  {var(S)}, !,
  dcg_print_var(S).
dcg_print_subject(S, Opts) -->
  {rdf_is_iri(S)}, !,
  dcg_print_iri(S, Opts).



dcg_print_term(T) -->
  dcg_print_term(T, _{}).


dcg_print_term(T, Opts) -->
  dcg_print_object(T, Opts).





% PRINT A TERM BY ITS KIND %

dcg_print_bnode(B, Opts) -->
  get_dict(bnode_map, Opts, false), !,
  dcg_print_truncated_atom(B, Opts).
dcg_print_bnode(B, _) -->
  {rdf_bnode_map(B, Name)},
  "_:", integer(Name).



dcg_print_datatype_iri(D, Opts) -->
  dcg_print_iri(D, Opts).



dcg_print_iri(Full, Opts) -->
  {
    get_dict(iri_lbl, Opts, true),
    rdfs_pref_label(Full, Lit)
  }, !,
  {rdf_literal_lex(Lit, Lex)},
  "“",
  atom(Lex),
  "”".
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



%! dcg_print_literal(+Lit)// is det.
%! dcg_print_literal(+Lit, +Opts)// is det.

dcg_print_literal(Lit) -->
  dcg_print_literal(Lit, _{}).


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
  {rdf_literal_lex(V^^D, Lex)},
  dcg_print_lexical_form(Lex, Opts),
  "^^",
  dcg_print_datatype_iri(D, Opts).
% Language-tagged string datatype IRI.
dcg_print_literal(V@LTag, Opts) --> !,
  {atom_string(Lex, V)},
  dcg_print_lexical_form(Lex, Opts),
  "@",
  dcg_print_language_tag(LTag, Opts).
% Unsupported literal.
dcg_print_literal(Lit, _) -->
  {gtrace}, %DEB
  {format(user_output, "~w~n", [Lit])}.


dcg_print_var(Var) -->
  {var_number(Var, N)},
  "?q",
  integer(N).


var_number(Var, N) :-
  var_map(Var0, N),
  Var == Var0, !.
var_number(Var, N) :-
  flag(var_counter, N0, N0 + 1),
  N is N0 + 1,
  assert(var_map(Var, N)).





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
