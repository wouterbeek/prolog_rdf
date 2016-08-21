:- module(
  q_print,
  [
    dcg_q_print_dataset_term//1, %     +D
    dcg_q_print_dataset_term//2, %     +D,             +Opts
    dcg_q_print_datatype//1,     %     +D
    dcg_q_print_datatype//2,     %     +D,             +Opts
    dcg_q_print_graph//2,        % ?M,             +G
    dcg_q_print_graph//3,        % ?M,             +G, +Opts
    dcg_q_print_graph_term//1,   %                 +G
    dcg_q_print_graph_term//2,   %                 +G, +Opts
    dcg_q_print_iri//1,          %     +Iri
    dcg_q_print_iri//2,          %     +Iri,           +Opts
    dcg_q_print_literal//1,      %     +Lit
    dcg_q_print_literal//2,      %     +Lit,           +Opts
    dcg_q_print_node//1,         %     +Node
    dcg_q_print_node//2,         %     +Node,          +Opts
    dcg_q_print_object//1,       %     +O
    dcg_q_print_object//2,       %     +O,             +Opts
    dcg_q_print_predicate//1,    %     +P
    dcg_q_print_predicate//2,    %     +P,             +Opts
    dcg_q_print_quad//1,         %     +Tuple
    dcg_q_print_quad//2,         %     +Tuple,         +Opts
    dcg_q_print_quad//4,         %     +S, +P, +O, +G
    dcg_q_print_quad//5,         %     +S, +P, +O, +G, +Opts
    dcg_q_print_quads//1,        %     +Tuples
    dcg_q_print_quads//2,        %     +Tuples,        +Opts
    dcg_q_print_quads//5,        % ?M, ?S, ?P, ?O, ?G
    dcg_q_print_quads//6,        % ?M, ?S, ?P, ?O, ?G, +Opts
    dcg_q_print_term//1,         %     +Term
    dcg_q_print_term//2,         %     +Term,          +Opts
    dcg_q_print_triple//1,       %     +Tuple
    dcg_q_print_triple//2,       %     +Tuple,         +Opts
    dcg_q_print_triple//3,       %     +S, +P, +O
    dcg_q_print_triple//4,       %     +S, +P, +O,     +Opts
    dcg_q_print_triples//1,      %     +Triples
    dcg_q_print_triples//2,      %     +Triples,       +Opts
    dcg_q_print_triples//4,      % ?M, ?S, ?P, ?O
    dcg_q_print_triples//5,      % ?M, ?S, ?P, ?O, ?G
    dcg_q_print_triples//6,      % ?M, ?S, ?P, ?O, ?G, +Opts
    q_print_cbd/2,               % ?M, ?S
    q_print_cbd/3,               % ?M, ?S,         ?G
    q_print_cbd/4,               % ?M, ?S,         ?G, +Opts
    q_print_dataset_term/1,      %     +D
    q_print_dataset_term/2,      %     +D,             +Opts
    q_print_datatype/1,          %     +D
    q_print_datatype/2,          %     +D,             +Opts
    q_print_graph/2,             % ?M,             +G
    q_print_graph/3,             % ?M,             +G, +Opts
    q_print_graph_term/1,        %                 +G
    q_print_graph_term/2,        %                 +G, +Opts
    q_print_iri/1,               %             +Iri
    q_print_iri/2,               %             +Iri,   +Opts
    q_print_literal/1,           %             +Lit
    q_print_literal/2,           %             +Lit,   +Opts
    q_print_object/1,            %             +O
    q_print_object/2,            %             +O,     +Opts
    q_print_pagination/5,        % ?M, ?S, ?P, ?O, ?G
    q_print_pagination/6,        % ?M, ?S, ?P, ?O, ?G, +Opts
    q_print_predicate/1,         %         +P
    q_print_predicate/2,         %         +P,         +Opts
    q_print_quad/1,              %     +Tuple
    q_print_quad/2,              %     +Tuple,         +Opts
    q_print_quad/4,              %     +S, +P, +O, +G
    q_print_quad/5,              %     +S, +P, +O, +G, +Opts
    q_print_quads/1,             %     +Tuples
    q_print_quads/2,             %     +Tuples,        +Opts
    q_print_quads/5,             % ?M, ?S, ?P, ?O, ?G
    q_print_quads/6,             % ?M, ?S, ?P, ?O, ?G, +Opts
    q_print_root/2,              % ?M, ?S
    q_print_root/3,              % ?M, ?S,         ?G
    q_print_root/4,              % ?M, ?S,         ?G, +Opts
    q_print_scbd/2,              % ?M, ?Node
    q_print_scbd/3,              % ?M, ?Node,      ?G
    q_print_scbd/4,              % ?M, ?Node,      ?G, +Opts
    q_print_term/1,              %     +Term
    q_print_term/2,              %     +Term,          +Opts
    q_print_tree/2,              % ?M, ?S
    q_print_tree/3,              % ?M, ?S,         ?G
    q_print_tree/4,              % ?M, ?S,         ?G, +Opts
    q_print_triple/1,            %     +Tuple
    q_print_triple/2,            %     +Tuple,         +Opts
    q_print_triple/3,            %     +S, +P, +O
    q_print_triple/4,            %     +S, +P, +O,     +Opts
    q_print_triples/1,           %     +Triples
    q_print_triples/2,           %     +Triples,       +Opts
    q_print_triples/4,           % ?M, ?S, ?P, ?O
    q_print_triples/5,           % ?M, ?S, ?P, ?O, ?G
    q_print_triples/6            % ?M, ?S, ?P, ?O, ?G, +Opts
  ]
).

/** <module> Quine print

Print RDF statements.

| **Key**       | **Value** | **Default** | **Description**                  |
|:--------------|:----------|:-----------:|:---------------------------------|
| `bnode_map`   | boolean   | `true`      | Whether blank node labels are    |
|               |           |             | replaced by integers.            |
| `indent`      | nonneg    | 0           |                                  |
| `iri_abbr`    | boolean   | `true`      | Whether IRIs are abbreviated     |
|               |           |             | based on the current aliases.    |
| `max_iri_len` | nonneg    | `inf`       | The maximum length of an IRI.    |
| `max_lit_len` | nonneg    | `inf`       | The maximum length of a literal. |

@tbd Turtle container abbreviation.

@tbd Turtle collection abbreviation.

@tbd More fine-grained control for RDF term ellipsis: `max_bnode_len`,
     `max_datatype_iri_len`, `max_lex_len`, `max_graph_term_len`,
     `max_dataset_term_len`.

@author Wouter Beek
@version 2016/06-2016/08
*/

:- use_module(library(semweb/rdf11)).
:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(dcg/dcg_cli)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_pl)).
:- use_module(library(dcg/dcg_table)).
:- use_module(library(dict_ext)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(pagination)).
:- use_module(library(pair_ext)).
:- use_module(library(print_ext)).
:- use_module(library(q/q_bnode_map)).
:- use_module(library(q/q_graph)).
:- use_module(library(q/q_shape)).
:- use_module(library(q/q_stmt)).
:- use_module(library(q/q_term)).
:- use_module(library(rdfs/rdfs_ext)).
:- use_module(library(yall)).


:- dynamic
    var_map/2.


:- multifile
    q:dcg_q_print_literal_hook//2.


:- rdf_meta
   dcg_q_print_dataset(r, ?, ?),
   dcg_q_print_dataset(r, +, ?, ?),
   dcg_q_print_datatype(r, ?, ?),
   dcg_q_print_datatype(r, +, ?, ?),
   dcg_q_print_graph(?, r, ?, ?),
   dcg_q_print_graph(?, r, +, ?, ?),
   dcg_q_print_graph_term(r, ?, ?),
   dcg_q_print_graph_term(r, +, ?, ?),
   dcg_q_print_iri(r, ?, ?),
   dcg_q_print_iri(r, +, ?, ?),
   dcg_q_print_literal(o, ?, ?),
   dcg_q_print_literal(o, +, ?, ?),
   dcg_q_print_object(o, ?, ?),
   dcg_q_print_object(o, +, ?, ?),
   dcg_q_print_node(o, ?, ?),
   dcg_q_print_node(o, +, ?, ?),
   dcg_q_print_predicate(r, ?, ?),
   dcg_q_print_predicate(r, +, ?, ?),
   dcg_q_print_quad(t, ?, ?),
   dcg_q_print_quad(t, +, ?, ?),
   dcg_q_print_quad(r, r, o, r, ?, ?),
   dcg_q_print_quad(r, r, o, r, +, ?, ?),
   dcg_q_print_quads(?, r, r, o, r, ?, ?),
   dcg_q_print_quads(?, r, r, o, r, +, ?, ?),
   dcg_q_print_term(o, ?, ?),
   dcg_q_print_term(o, +, ?, ?),
   dcg_q_print_triple(t, ?, ?),
   dcg_q_print_triple(t, +, ?, ?),
   dcg_q_print_triple(r, r, o, ?, ?),
   dcg_q_print_triple(r, r, o, +, ?, ?),
   dcg_q_print_triples(?, r, r, o, ?, ?),
   dcg_q_print_triples(?, r, r, o, r, ?, ?),
   dcg_q_print_triples(?, r, r, o, r, +, ?, ?),
   q_print_cbd(?, r),
   q_print_cbd(?, r, r),
   q_print_cbd(?, r, r, +),
   q_print_dataset(r),
   q_print_dataset(r, +),
   q_print_datatype(r),
   q_print_datatype(r, +),
   q_print_graph(?, r),
   q_print_graph(?, r, +),
   q_print_graph_term(r),
   q_print_graph_term(r, +),
   q_print_iri(r),
   q_print_iri(r, +),
   q_print_literal(o),
   q_print_literal(o, +),
   q_print_object(o),
   q_print_object(o, +),
   q_print_pagination(?, r, r, o, r),
   q_print_pagination(?, r, r, o, r, +),
   q_print_predicate(r),
   q_print_predicate(r, +),
   q_print_quad(t),
   q_print_quad(t, +),
   q_print_quad(r, r, o, r),
   q_print_quad(r, r, o, r, +),
   q_print_quads(?, r, r, o, r),
   q_print_quads(?, r, r, o, r, +),
   q_print_root(?, r),
   q_print_root(?, r, r),
   q_print_root(?, r, r, +),
   q_print_scbd(?, o),
   q_print_scbd(?, o, r),
   q_print_scbd(?, o, r, +),
   q_print_term(o),
   q_print_term(o, +),
   q_print_tree(?, r),
   q_print_tree(?, r, r),
   q_print_tree(?, r, r, +),
   q_print_triple(t),
   q_print_triple(t, +),
   q_print_triple(r, r, o),
   q_print_triple(r, r, o, +),
   q_print_triples(?, r, r, o),
   q_print_triples(?, r, r, o, r),
   q_print_triples(?, r, r, o, r, +).





% NON-DCG INVOCATIONS %

%! q_print_dataset_term(+D) is det.
%! q_print_dataset_term(+D, +Opts) is det.

q_print_dataset_term(D) :-
  q_print_dataset_term(D, _{}).


q_print_dataset_term(D, Opts1) :-
  q_print_default_options(Opts1, Out, Opts2),
  dcg_with_output_to(Out, dcg_q_print_dataset_term(D, Opts2)).



%! q_print_datatype(+D) is det.
%! q_print_datatype(+D, +Opts) is det.

q_print_datatype(D) :-
  q_print_datatype(D, _{}).


q_print_datatype(D, Opts1) :-
  q_print_default_options(Opts1, Out, Opts2),
  dcg_with_output_to(Out, dcg_q_print_datatype(D, Opts2)).



%! q_print_cbd(?M, ?S) is det.
%! q_print_cbd(?M, ?S, ?G) is det.
%! q_print_cbd(?M, ?S, ?G, +Opts) is det.
%
% Print the Concise-Bounded Description (CBD) of subject terms.

q_print_cbd(M, S) :-
  q_print_cbd(M, S, _).


q_print_cbd(M, S, G) :-
  q_print_cbd(M, S, G, []).


q_print_cbd(M, S, G, Opts) :-
  q_cbd_triples(M, S, G, Triples),
  q_print_triples(Triples, Opts).



q_print_graph(M, G) :-
  q_print_graph(M, G, _{}).


q_print_graph(M, G, Opts1) :-
  q_print_default_options(Opts1, Out, Opts2),
  dcg_with_output_to(Out, dcg_q_print_graph(M, G, Opts2)).



q_print_graph_term(G) :-
  q_print_graph_term(G, _{}).


q_print_graph_term(G, Opts1) :-
  q_print_default_options(Opts1, Out, Opts2),
  dcg_with_output_to(Out, dcg_q_print_graph_term(G, Opts2)).



q_print_iri(Lit) :-
  q_print_object(Lit, _{}).


q_print_iri(Lit, Opts1) :-
  q_print_default_options(Opts1, Out, Opts2),
  dcg_with_output_to(Out, dcg_q_print_iri(Lit, Opts2)).



q_print_literal(Lit) :-
  q_print_literal(Lit, _{}).


q_print_literal(Lit, Opts1) :-
  q_print_default_options(Opts1, Out, Opts2),
  dcg_with_output_to(Out, dcg_q_print_literal(Lit, Opts2)).



q_print_object(O) :-
  q_print_object(O, _{}).


q_print_object(O, Opts1) :-
  q_print_default_options(Opts1, Out, Opts2),
  dcg_with_output_to(Out, dcg_q_print_object(O, Opts2)).



q_print_pagination(M, S, P, O, G) :-
  q_print_pagination(M, S, P, O, G, _{}).


q_print_pagination(M, S, P, O, G, Opts) :-
  pagination(Triple, q_triple(M, S, P, O, G, Triple), Opts, Result),
  pagination_result(
    Result,
    {Opts}/[Results]>>q_print_triples(Results, Opts)
  ).



q_print_predicate(P) :-
  q_print_predicate(P, _{}).


q_print_predicate(P, Opts1) :-
  q_print_default_options(Opts1, Out, Opts2),
  dcg_with_output_to(Out, dcg_q_print_predicate(P, Opts2)).



q_print_quad(Tuple) :-
  q_print_quad(Tuple, _{}).


q_print_quad(Tuple, Opts1) :-
  q_print_default_options(Opts1, Out, Opts2),
  dcg_with_output_to(Out, dcg_q_print_quad(Tuple, Opts2)).



q_print_quad(S, P, O, G) :-
  q_print_quad(S, P, O, G, _{}).


q_print_quad(S, P, O, G, Opts1) :-
  q_print_default_options(Opts1, Out, Opts2),
  dcg_with_output_to(Out, dcg_q_print_quad(S, P, O, G, Opts2)).



q_print_quads(Tuples) :-
  q_print_quads(Tuples, _{}).


q_print_quads(Tuples, Opts1) :-
  q_print_default_options(Opts1, Out, Opts2),
  dcg_with_output_to(Out, dcg_q_print_quads(Tuples, Opts2)).



q_print_quads(M, S, P, O, G) :-
  q_print_quads(M, S, P, O, G, _{}).


q_print_quads(M, S, P, O, G, Opts1) :-
  q_print_default_options(Opts1, Out, Opts2),
  dcg_with_output_to(Out, dcg_q_print_quads(M, S, P, O, G, Opts2)).



%! q_print_root(?M, ?S) is det.
%! q_print_root(?M, ?S, ?G) is det.
%! q_print_root(?M, ?S, ?G, +Opts) is det.
%
% Print the tree for an RDF root node.

q_print_root(M, S) :-
  q_print_root(M, S, _).


q_print_root(M, S, G) :-
  q_print_root(M, S, G, []).


q_print_root(M, S, G, Opts) :-
  q_root(M, S, G),
  q_tree_triples(M, S, G, Triples),
  q_print_triples(Triples, Opts).



%! q_print_scbd(?M, ?Node) is det.
%! q_print_scbd(?M, ?Node, ?G) is det.
%! q_print_scbd(?M, ?Node, ?G, +Opts) is det.
%
% Print the Symmetric CBD (SCBD) for an RDF node.

q_print_scbd(M, Node) :-
  q_print_scbd(M, Node, _).


q_print_scbd(M, Node, G) :-
  q_print_scbd(M, Node, G, []).


q_print_scbd(M, Node, G, Opts) :-
  q_scbd_triples(M, Node, G, Triples),
  q_print_triples(Triples, Opts).



q_print_term(T) :-
  q_print_term(T, _{}).


q_print_term(T, Opts1) :-
  q_print_default_options(Opts1, Out, Opts2),
  dcg_with_output_to(Out, dcg_q_print_term(T, Opts2)).



%! q_print_tree(?M, ?S) is det.
%! q_print_tree(?M, ?S, ?G) is det.
%! q_print_tree(?M, ?S, ?G, +Opts) is det.
%
% Print the tree for a subject term.

q_print_tree(M, S) :-
  q_print_tree(M, S, _).


q_print_tree(M, S, G) :-
  q_print_tree(M, S, G, []).


q_print_tree(M, S, G, Opts) :-
  q_tree(M, S, G, Triples),
  q_print_triples(Triples, Opts).



q_print_triple(Tuple) :-
  q_print_triple(Tuple, _{}).


q_print_triple(Tuple, Opts1) :-
  q_print_default_options(Opts1, Out, Opts2),
  dcg_with_output_to(Out, dcg_q_print_triple(Tuple, Opts2)).



q_print_triple(S, P, O) :-
  q_print_triple(S, P, O, _{}).


q_print_triple(S, P, O, Opts1) :-
  q_print_default_options(Opts1, Out, Opts2),
  dcg_with_output_to(Out, dcg_q_print_triple(S, P, O, Opts2)).



q_print_triples(Triples) :-
  q_print_triples(Triples, _{}).


q_print_triples(Triples, Opts1) :-
  q_print_default_options(Opts1, Out, Opts2),
  dcg_with_output_to(Out, dcg_q_print_triples(Triples, Opts2)).



q_print_triples(M, S, P, O) :-
  q_print_triples(M, S, P, O, _).


q_print_triples(M, S, P, O, G) :-
  q_print_triples(M, S, P, O, G, _{}).


q_print_triples(M, S, P, O, G, Opts1) :-
  q_print_default_options(Opts1, Out, Opts2),
  dcg_with_output_to(Out, dcg_q_print_triples(M, S, P, O, G, Opts2)).





% PRINT MULTIPLE TUPLES %

dcg_q_print_graph(M, G) -->
  {dcg_q_print_default_options(Opts)},
  dcg_q_print_graph(M, G, Opts).


dcg_q_print_graph(M, G, Opts) -->
  dcg_q_print_quads(M, _, _, _, G, Opts).



dcg_q_print_quads(Tuples) -->
  {dcg_q_print_default_options(Opts)},
  dcg_q_print_quads(Tuples, Opts).


dcg_q_print_quads([rdf(S,P,O)], Opts) --> !,
  dcg_q_print_triples0(0, [rdf(S,P,O,default)], Opts).
dcg_q_print_quads([rdf(S,P,O,G)], Opts) --> !,
  dcg_q_print_subject(S, Opts),
  " ",
  dcg_q_print_predicate(P, Opts),
  " ",
  dcg_q_print_object(O, Opts),
  " ",
  dcg_q_print_graph_term(G, Opts),
  " .",
  nl.
dcg_q_print_quads(Tuples, Opts) -->
  {
    maplist(graph_triple_pair0, Tuples, Pairs),
    keysort(Pairs, SortedPairs)
  },
  dcg_q_print_sorted_pairs0(SortedPairs, Opts).


graph_triple_pair0(rdf(S,P,O), G-rdf(S,P,O)) :-
  q_default_graph(G).
graph_triple_pair0(rdf(S,P,O,G), G-rdf(S,P,O)).



dcg_q_print_quads(M, S, P, O, G) -->
  {dcg_q_print_default_options(Opts)},
  dcg_q_print_quads(M, S, P, O, G, Opts).


dcg_q_print_quads(M, S, P, O, G, Opts) -->
  {aggregate_all(set(G-rdf(S,P,O)), q(M, S, P, O, G), SortedPairs)},
  dcg_q_print_sorted_pairs0(SortedPairs, Opts).



dcg_q_print_triples(Triples) -->
  {dcg_q_print_default_options(Opts)},
  dcg_q_print_triples(Triples, Opts).


dcg_q_print_triples(Triples, Opts) -->
  {q_default_graph(G)},
  dcg_q_print_groups0([G-Triples], Opts).


dcg_q_print_triples(M, S, P, O) -->
  dcg_q_print_triples(M, S, P, O, _).


dcg_q_print_triples(M, S, P, O, G) -->
  {dcg_q_print_default_options(Opts)},
  dcg_q_print_triples(M, S, P, O, G, Opts).


dcg_q_print_triples(M, S, P, O, G, Opts) -->
  {q_triples(M, S, P, O, G, Triples)},
  dcg_q_print_triples(Triples, Opts).


dcg_q_print_sorted_pairs0(SortedPairs, Opts) -->
  {group_pairs_by_key(SortedPairs, Groups)},
  dcg_q_print_groups0(Groups, Opts).


dcg_q_print_groups0([], _) --> !, [].
dcg_q_print_groups0([G-Triples|Groups], Opts) -->
  {get_dict(indent, Opts, I1, 0)},
  (   {q_default_graph(G)}
  ->  {I2 = I1}
  ;   tab(I1),
      dcg_q_print_graph_term(G, Opts),
      " {\n",
      {I2 = I1 + 1}
  ),
  dcg_q_print_triples0(I2, Triples, Opts),
  ({q_default_graph(G)} -> "" ; "}\n"),
  dcg_q_print_groups0(Groups, Opts).


dcg_q_print_triples0(I, Triples, Opts) -->
  {
    aggregate_all(set(S-po(P,O)), member(rdf(S,P,O), Triples), SortedPairs),
    group_pairs_by_key(SortedPairs, Groups)
  },
  dcg_q_print_subjects0(I, Groups, Opts).


dcg_q_print_subjects0(_, [], _) --> !, [].
dcg_q_print_subjects0(I1, [S-POs|Groups1], Opts) -->
  tab(I1),
  dcg_q_print_subject(S, Opts),
  {
    aggregate_all(set(P-O), member(po(P,O), POs), SortedPairs),
    group_pairs_by_key(SortedPairs, Groups2),
    I2 is I1 + 1
  },
  dcg_q_print_predicates1(I2, Groups2, Opts),
  nl,
  dcg_q_print_subjects0(I1, Groups1, Opts).


% There is exactly one predicate.  Emit it on the same line.
dcg_q_print_predicates1(I, [P-Os], Opts) --> !,
  " ",
  dcg_q_print_predicate(P, Opts),
  dcg_q_print_objects1(I, Os, Opts),
  " .".
dcg_q_print_predicates1(I, Groups, Opts) -->
  dcg_q_print_predicates2(I, Groups, Opts).


dcg_q_print_predicates2(_, [], _) --> !, [].
dcg_q_print_predicates2(I1, [P-Os|Groups], Opts) -->
  nl,
  tab(I1),
  dcg_q_print_predicate(P, Opts),
  {I2 is I1 + 1},
  dcg_q_print_objects1(I2, Os, Opts),
  " ",
  ({Groups == []} -> "." ; ";"),
  dcg_q_print_predicates2(I1, Groups, Opts).


% There is exactly one object.  Emit it on the same line.
dcg_q_print_objects1(_, [O], Opts) --> !,
  " ",
  dcg_q_print_object(O, Opts).
dcg_q_print_objects1(I, Os, Opts) -->
  dcg_q_print_objects2(I, Os, Opts).


dcg_q_print_objects2(_, [], _) --> !, [].
dcg_q_print_objects2(I, [O|Os], Opts) -->
  nl,
  tab(I),
  dcg_q_print_object(O, Opts),
  ({Os == []} -> "" ; " ,"),
  dcg_q_print_objects2(I, Os, Opts).





% PRINT A SINGLE TUPLE %

dcg_q_print_quad(Tuple) -->
  {dcg_q_print_default_options(Opts)},
  dcg_q_print_quad(Tuple, Opts).


dcg_q_print_quad(rdf(S,P,O), Opts) --> !,
  {q_default_graph(G)},
  dcg_q_print_quad(rdf(S,P,O,G), Opts).
dcg_q_print_quad(rdf(S,P,O,G), Opts) -->
  dcg_q_print_quad(S, P, O, G, Opts).


dcg_q_print_quad(S, P, O, G) -->
  {dcg_q_print_default_options(Opts)},
  dcg_q_print_quad(S, P, O, G, Opts).


dcg_q_print_quad(S, P, O, G, Opts) -->
  dcg_q_print_quads([rdf(S,P,O,G)], Opts).



dcg_q_print_triple(Tuple) -->
  {dcg_q_print_default_options(Opts)},
  dcg_q_print_triple(Tuple, Opts).


dcg_q_print_triple(rdf(S,P,O), Opts) --> !,
  dcg_q_print_triple(S, P, O, Opts).
dcg_q_print_triple(rdf(S,P,O,_), Opts) -->
  dcg_q_print_triple(rdf(S,P,O), Opts).


dcg_q_print_triple(S, P, O) -->
  {dcg_q_print_default_options(Opts)},
  dcg_q_print_triple(S, P, O, Opts).


dcg_q_print_triple(S, P, O, Opts) -->
  dcg_q_print_triples([rdf(S,P,O)], Opts).





% PRINT A TERM BY ITS POSITIONALITY %

dcg_q_print_dataset_term(G) -->
  {dcg_q_print_default_options(Opts)},
  dcg_q_print_dataset_term(G, Opts).


dcg_q_print_dataset_term(G, Opts) -->
  dcg_q_print_iri(G, Opts).



dcg_q_print_graph_term(G) -->
  {dcg_q_print_default_options(Opts)},
  dcg_q_print_graph_term(G, Opts).


dcg_q_print_graph_term(G, Opts) -->
  dcg_q_print_iri(G, Opts).



dcg_q_print_node(Node) -->
  {dcg_q_print_default_options(Opts)},
  dcg_q_print_node(Node, Opts).


dcg_q_print_node(Node, Opts) -->
  dcg_q_print_term(Node, Opts).



dcg_q_print_object(O) -->
  {dcg_q_print_default_options(Opts)},
  dcg_q_print_object(O, Opts).


dcg_q_print_object(O, Opts) -->
  {q_is_literal(O)}, !,
  dcg_q_print_literal(O, Opts).
dcg_q_print_object(O, Opts) -->
  dcg_q_print_subject(O, Opts).



dcg_q_print_predicate(P) -->
  {dcg_q_print_default_options(Opts)},
  dcg_q_print_predicate(P, Opts).


dcg_q_print_predicate(P, _) -->
  {
    ground(P),
    rdf_equal(rdf:type, P)
  }, !,
  "a".
dcg_q_print_predicate(P, _) -->
  {var(P)}, !,
  dcg_q_print_var(P).
dcg_q_print_predicate(P, Opts) -->
  dcg_q_print_iri(P, Opts).



dcg_q_print_subject(S, Opts) -->
  {q_is_bnode(S)}, !,
  dcg_q_print_bnode(S, Opts).
dcg_q_print_subject(S, _) -->
  {var(S)}, !,
  dcg_q_print_var(S).
dcg_q_print_subject(S, Opts) -->
  {q_is_iri(S)}, !,
  dcg_q_print_iri(S, Opts).



dcg_q_print_term(T) -->
  {dcg_q_print_default_options(Opts)},
  dcg_q_print_term(T, Opts).


dcg_q_print_term(T, Opts) -->
  dcg_q_print_object(T, Opts).





% PRINT A TERM BY ITS KIND %

dcg_q_print_bnode(B, Opts) -->
  get_dict(bnode_map, Opts, false), !,
  atom_ellipsis(B, Opts.max_bnode_len).
dcg_q_print_bnode(B, _) -->
  {q_bnode_map(B, Name)},
  "_:", integer(Name).



%! dcg_q_print_datatype(+D)// is det.
%! dcg_q_print_datatype(+D, +Opts)// is det.

dcg_q_print_datatype(D) -->
  {dcg_q_print_default_options(Opts)},
  dcg_q_print_datatype(D, Opts).


dcg_q_print_datatype(D, Opts) -->
  dcg_q_print_iri(D, Opts).



%! dcg_q_print_iri(+Iri)// is det.
%! dcg_q_print_iri(+Iri, +Opts)// is det.

dcg_q_print_iri(Iri) -->
  {dcg_q_print_default_options(Opts)},
  dcg_q_print_iri(Iri, Opts).


dcg_q_print_iri(Full, Opts) -->
  {
    Opts.iri_abbr == true,
    rdf_global_id(Alias:Local, Full), !,
    atom_length(Alias, AliasLen),
    Minus is AliasLen + 1,
    inf_minus(Opts.max_iri_len, Minus, Max)
  },
  atom(Alias),
  ":",
  atom_ellipsis(Local, Max).
dcg_q_print_iri(Full, Opts) -->
  "<",
  atom_ellipsis(Full, Opts.max_iri_len),
  ">".



dcg_q_print_language_tag(LTag, Opts) -->
  atom_ellipsis(LTag, Opts.max_lit_len).



dcg_q_print_lexical_form(Lex, Opts) -->
  "\"",
  atom_ellipsis(Lex, Opts.max_lit_len),
  "\"".



%! dcg_q_print_literal(+Lit)// is det.
%! dcg_q_print_literal(+Lit, +Opts)// is det.

dcg_q_print_literal(Lit) -->
  {dcg_q_print_default_options(Opts)},
  dcg_q_print_literal(Lit, Opts).


% Datatype hooks.
dcg_q_print_literal(Lit, Opts) -->
  q:dcg_q_print_literal_hook(Lit, Opts), !.
% Abbreviate XSD Boolean.
dcg_q_print_literal(Lex^^D, Opts) -->
  {rdf_equal(xsd:boolean, D)}, !,
  dcg_q_print_lexical_form(Lex, Opts).
% Abbreviate XSD string.
dcg_q_print_literal(V^^D, Opts) -->
  {rdf_equal(xsd:string, D)}, !,
  {atom_string(Lex, V)},
  dcg_q_print_lexical_form(Lex, Opts).
% Abbreviate XSD integers.
dcg_q_print_literal(Val^^D, _) -->
  {rdf_equal(xsd:integer, D)}, !,
  thousands(Val).
% Abbreviate XSD decimals and doubles.
dcg_q_print_literal(Val^^D, Opts) -->
  {
    (rdf_equal(xsd:decimal, D) ; rdf_equal(xsd:double, D)), !,
    atom_number(Lex, Val)
  },
  dcg_q_print_lexical_form(Lex, Opts).
% Unabbreviated datatype IRI that is not `rdf:langString`.
dcg_q_print_literal(V^^D, Opts) --> !,
  {q_literal_lex(V^^D, Lex)},
  dcg_q_print_lexical_form(Lex, Opts),
  "^^",
  dcg_q_print_datatype(D, Opts).
% Language-tagged string datatype IRI.
dcg_q_print_literal(V@LTag, Opts) --> !,
  {atom_string(Lex, V)},
  dcg_q_print_lexical_form(Lex, Opts),
  "@",
  dcg_q_print_language_tag(LTag, Opts).


dcg_q_print_var(Var) -->
  {var_number(Var, N)},
  "?q",
  integer(N).





% HELPERS %

%! dcg_q_print_default_options(-Opts) is det.

dcg_q_print_default_options(
  _{iri_abbr: true, max_iri_len: inf, max_lit_len: inf}
).



%! inf_minus(+X, +Y, -Z) is det.
  
inf_minus(inf, _, inf) :- !.
inf_minus(X, Y, X) :-
  X =< Y, !.
inf_minus(X, Y, Z) :-
  Z is X - Y.



%! q_print_default_options(+Opts1, -Out, -Opts2) is det.

q_print_default_options(Opts1, Out, Opts5) :-
  mod_dict(out, Opts1, current_output, Out, Opts3),
  dcg_q_print_default_options(Opts4),
  merge_dicts(Opts4, Opts3, Opts5).



%! var_number(@Var, -N) is det.

var_number(Var, N) :-
  var_map(Var0, N),
  Var == Var0, !.
var_number(Var, N) :-
  flag(var_counter, N0, N0 + 1),
  N is N0 + 1,
  assert(var_map(Var, N)).
