:- module(
  rdf_print,
  [
    dcg_rdf_print_options/2,       % +Opts1, -Opts2
    dcg_rdf_print_class//1,        %     +C
    dcg_rdf_print_class//2,        %     +C,             +Opts
    dcg_rdf_print_dataset_term//1, %     +D
    dcg_rdf_print_dataset_term//2, %     +D,             +Opts
    dcg_rdf_print_datatype//1,     %     +D
    dcg_rdf_print_datatype//2,     %     +D,             +Opts
    dcg_rdf_print_graph//2,        % ?M,             +G
    dcg_rdf_print_graph//3,        % ?M,             +G, +Opts
    dcg_rdf_print_graph_term//1,   %                 +G
    dcg_rdf_print_graph_term//2,   %                 +G, +Opts
    dcg_rdf_print_iri//1,          %     +Iri
    dcg_rdf_print_iri//2,          %     +Iri,           +Opts
    dcg_rdf_print_literal//1,      %     +Lit
    dcg_rdf_print_literal//2,      %     +Lit,           +Opts
    dcg_rdf_print_node//1,         %     +Node
    dcg_rdf_print_node//2,         %     +Node,          +Opts
    dcg_rdf_print_object//1,       %     +O
    dcg_rdf_print_object//2,       %     +O,             +Opts
    dcg_rdf_print_predicate//1,    %     +P
    dcg_rdf_print_predicate//2,    %     +P,             +Opts
    dcg_rdf_print_quad//1,         %     +Tuple
    dcg_rdf_print_quad//2,         %     +Tuple,         +Opts
    dcg_rdf_print_quad//4,         %     +S, +P, +O, +G
    dcg_rdf_print_quad//5,         %     +S, +P, +O, +G, +Opts
    dcg_rdf_print_quads//1,        %     +Tuples
    dcg_rdf_print_quads//2,        %     +Tuples,        +Opts
    dcg_rdf_print_quads//5,        % ?M, ?S, ?P, ?O, ?G
    dcg_rdf_print_quads//6,        % ?M, ?S, ?P, ?O, ?G, +Opts
    dcg_rdf_print_subject//1,      %     +S
    dcg_rdf_print_subject//2,      %     +S,             +Opts
    dcg_rdf_print_term//1,         %     +Term
    dcg_rdf_print_term//2,         %     +Term,          +Opts
    dcg_rdf_print_triple//1,       %     +Tuple
    dcg_rdf_print_triple//2,       %     +Tuple,         +Opts
    dcg_rdf_print_triple//3,       %     +S, +P, +O
    dcg_rdf_print_triple//4,       %     +S, +P, +O,     +Opts
    dcg_rdf_print_triples//1,      %     +Triples
    dcg_rdf_print_triples//2,      %     +Triples,       +Opts
    dcg_rdf_print_triples//4,      % ?M, ?S, ?P, ?O
    dcg_rdf_print_triples//5,      % ?M, ?S, ?P, ?O, ?G
    dcg_rdf_print_triples//6,      % ?M, ?S, ?P, ?O, ?G, +Opts
    dcg_rdf_print_var//2,          % +Var,               +Opts
    pp_jsonld/2,                   % +Context, :Goal_1
    rdf_print_cbd/2,               % ?M, ?S
    rdf_print_cbd/3,               % ?M, ?S,         ?G
    rdf_print_cbd/4,               % ?M, ?S,         ?G, +Opts
    rdf_print_dataset_term/1,      %     +D
    rdf_print_dataset_term/2,      %     +D,             +Opts
    rdf_print_datatype/1,          %     +D
    rdf_print_datatype/2,          %     +D,             +Opts
    rdf_print_graph/2,             % ?M,             +G
    rdf_print_graph/3,             % ?M,             +G, +Opts
    rdf_print_graph_term/1,        %                 +G
    rdf_print_graph_term/2,        %                 +G, +Opts
    rdf_print_iri/1,               %             +Iri
    rdf_print_iri/2,               %             +Iri,   +Opts
    rdf_print_literal/1,           %             +Lit
    rdf_print_literal/2,           %             +Lit,   +Opts
    rdf_print_object/1,            %             +O
    rdf_print_object/2,            %             +O,     +Opts
    rdf_print_pagination/5,        % ?M, ?S, ?P, ?O, ?G
    rdf_print_pagination/6,        % ?M, ?S, ?P, ?O, ?G, +Opts
    rdf_print_predicate/1,         %         +P
    rdf_print_predicate/2,         %         +P,         +Opts
    rdf_print_quad/1,              %     +Tuple
    rdf_print_quad/2,              %     +Tuple,         +Opts
    rdf_print_quad/4,              %     +S, +P, +O, +G
    rdf_print_quad/5,              %     +S, +P, +O, +G, +Opts
    rdf_print_quads/1,             %     +Tuples
    rdf_print_quads/2,             %     +Tuples,        +Opts
    rdf_print_quads/5,             % ?M, ?S, ?P, ?O, ?G
    rdf_print_quads/6,             % ?M, ?S, ?P, ?O, ?G, +Opts
    rdf_print_root/2,              % ?M, ?S
    rdf_print_root/3,              % ?M, ?S,         ?G
    rdf_print_root/4,              % ?M, ?S,         ?G, +Opts
    rdf_print_scbd/2,              % ?M, ?Node
    rdf_print_scbd/3,              % ?M, ?Node,      ?G
    rdf_print_scbd/4,              % ?M, ?Node,      ?G, +Opts
    rdf_print_term/1,              %     +Term
    rdf_print_term/2,              %     +Term,          +Opts
    rdf_print_tree/2,              % ?M, ?S
    rdf_print_tree/3,              % ?M, ?S,         ?G
    rdf_print_tree/4,              % ?M, ?S,         ?G, +Opts
    rdf_print_triple/1,            %     +Tuple
    rdf_print_triple/2,            %     +Tuple,         +Opts
    rdf_print_triple/3,            %     +S, +P, +O
    rdf_print_triple/4,            %     +S, +P, +O,     +Opts
    rdf_print_triples/1,           %     +Triples
    rdf_print_triples/2,           %     +Triples,       +Opts
    rdf_print_triples/4,           % ?M, ?S, ?P, ?O
    rdf_print_triples/5,           % ?M, ?S, ?P, ?O, ?G
    rdf_print_triples/6            % ?M, ?S, ?P, ?O, ?G, +Opts
  ]
).

/** <module> RDF printing

| **Key**       | **Value**  | **Default** | **Description**                  |
|:--------------|:-----------|:-----------:|:---------------------------------|
| `indent`      | nonneg     | 0           |                                  |
| `iri_abbr`    | boolean    | `true`      | Whether IRIs are abbreviated     |
|               |            |             | based on the current aliases.    |
| `iri_lbl`     | boolean    | `false`     | Whether labels should be         |
|               |            |             | displayed i.o. IRIs.  This       |
|               |            |             | overrides option `iri_abbr`.     |
| `max_iri_len` | nonneg     | `inf`       | The maximum length of an IRI.    |
| `max_lit_len` | nonneg     | `inf`       | The maximum length of a literal. |
| `prefixes`    | list(pair) | `$null$`    | A custom list of alias/prefix    |
|               |            |             | mappings that overrules and/or   |
|               |            |             | extends the alias declarations.  |

@tbd Turtle container abbreviation.

@tbd Turtle collection abbreviation.

@tbd More fine-grained control for RDF term ellipsis:
     `max_datatype_iri_len`, `max_lex_len`, `max_graph_term_len`,
     `max_dataset_term_len`.

@author Wouter Beek
@version 2016/06-2017/01
*/

:- use_module(library(semweb/rdf11)). % @bug Is this needed?
:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(dcg/dcg_cli)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_pl)).
:- use_module(library(dcg/dcg_table)).
:- use_module(library(dialect/hprolog)).
:- use_module(library(dict_ext)).
:- use_module(library(jsonld/jsonld_read)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(pagination/cli_pagination)).
:- use_module(library(pair_ext)).
:- use_module(library(print_ext)).
:- use_module(library(rdf/rdf_api)).
:- use_module(library(rdf/rdfs_api)).
:- use_module(library(rdf/rdf_shape)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(settings)).
:- use_module(library(typecheck)).
:- use_module(library(yall)).

:- meta_predicate
    pp_jsonld(+, 1).

:- multifile
    dcg:dcg_hook//1,
    q:dcg_rdf_print_literal_hook//2.

dcg:dcg_hook(rdf_dataset_term(D)) -->
  dcg_rdf_print_dataset_term(D).
dcg:dcg_hook(rdf_graph_term(G)) -->
  dcg_rdf_print_graph_term(G).
dcg:dcg_hook(rdf_iri(Iri)) -->
  dcg_rdf_print_iri(Iri).
dcg:dcg_hook(rdf_object(O)) -->
  dcg_rdf_print_object(O).
dcg:dcg_hook(rdf_predicate(P)) -->
  dcg_rdf_print_predicate(P).

:- rdf_meta
   dcg_rdf_print_class(r, ?, ?),
   dcg_rdf_print_class(r, +, ?, ?),
   dcg_rdf_print_dataset(r, ?, ?),
   dcg_rdf_print_dataset(r, +, ?, ?),
   dcg_rdf_print_datatype(r, ?, ?),
   dcg_rdf_print_datatype(r, +, ?, ?),
   dcg_rdf_print_graph(?, r, ?, ?),
   dcg_rdf_print_graph(?, r, +, ?, ?),
   dcg_rdf_print_graph_term(r, ?, ?),
   dcg_rdf_print_graph_term(r, +, ?, ?),
   dcg_rdf_print_iri(r, ?, ?),
   dcg_rdf_print_iri(r, +, ?, ?),
   dcg_rdf_print_literal(o, ?, ?),
   dcg_rdf_print_literal(o, +, ?, ?),
   dcg_rdf_print_object(o, ?, ?),
   dcg_rdf_print_object(o, +, ?, ?),
   dcg_rdf_print_node(o, ?, ?),
   dcg_rdf_print_node(o, +, ?, ?),
   dcg_rdf_print_predicate(r, ?, ?),
   dcg_rdf_print_predicate(r, +, ?, ?),
   dcg_rdf_print_quad(t, ?, ?),
   dcg_rdf_print_quad(t, +, ?, ?),
   dcg_rdf_print_quad(r, r, o, r, ?, ?),
   dcg_rdf_print_quad(r, r, o, r, +, ?, ?),
   dcg_rdf_print_quads(?, r, r, o, r, ?, ?),
   dcg_rdf_print_quads(?, r, r, o, r, +, ?, ?),
   dcg_rdf_print_term(o, ?, ?),
   dcg_rdf_print_term(o, +, ?, ?),
   dcg_rdf_print_triple(t, ?, ?),
   dcg_rdf_print_triple(t, +, ?, ?),
   dcg_rdf_print_triple(r, r, o, ?, ?),
   dcg_rdf_print_triple(r, r, o, +, ?, ?),
   dcg_rdf_print_triples(?, r, r, o, ?, ?),
   dcg_rdf_print_triples(?, r, r, o, r, ?, ?),
   dcg_rdf_print_triples(?, r, r, o, r, +, ?, ?),
   rdf_print_cbd(?, r),
   rdf_print_cbd(?, r, r),
   rdf_print_cbd(?, r, r, +),
   rdf_print_dataset(r),
   rdf_print_dataset(r, +),
   rdf_print_datatype(r),
   rdf_print_datatype(r, +),
   rdf_print_graph(?, r),
   rdf_print_graph(?, r, +),
   rdf_print_graph_term(r),
   rdf_print_graph_term(r, +),
   rdf_print_iri(r),
   rdf_print_iri(r, +),
   rdf_print_literal(o),
   rdf_print_literal(o, +),
   rdf_print_object(o),
   rdf_print_object(o, +),
   rdf_print_pagination(?, r, r, o, r),
   rdf_print_pagination(?, r, r, o, r, +),
   rdf_print_predicate(r),
   rdf_print_predicate(r, +),
   rdf_print_quad(t),
   rdf_print_quad(t, +),
   rdf_print_quad(r, r, o, r),
   rdf_print_quad(r, r, o, r, +),
   rdf_print_quads(?, r, r, o, r),
   rdf_print_quads(?, r, r, o, r, +),
   rdf_print_root(?, r),
   rdf_print_root(?, r, r),
   rdf_print_root(?, r, r, +),
   rdf_print_scbd(?, o),
   rdf_print_scbd(?, o, r),
   rdf_print_scbd(?, o, r, +),
   rdf_print_term(o),
   rdf_print_term(o, +),
   rdf_print_tree(?, r),
   rdf_print_tree(?, r, r),
   rdf_print_tree(?, r, r, +),
   rdf_print_triple(t),
   rdf_print_triple(t, +),
   rdf_print_triple(r, r, o),
   rdf_print_triple(r, r, o, +),
   rdf_print_triples(?, r, r, o),
   rdf_print_triples(?, r, r, o, r),
   rdf_print_triples(?, r, r, o, r, +).

:- setting(
     backend,
     oneof([hdt,trp]),
     hdt,
     "The backend that is used for looking up textual labels for RDF terms."
   ).





% DCG OPTIONS  %

%! dcg_rdf_print_options(+Opts1, -Opts2) is det.
%
% Options for the ‘rdf_print_*’ predicates are automatically resolved,
% but for the DCG rules ‘dcg_rdf_print_*’ options need to merged
% explicitly using this predicate.

dcg_rdf_print_options(Opts1, Opts3) :-
  dcg_rdf_print_default_options(Opts2),
  merge_dicts(Opts2, Opts1, Opts3).





% META %

%! pp_jsonld(+Context, :Goal_1) .
%
% Print the reply for a TAPIR predicate using Turtle.

pp_jsonld(Context, Goal_1) :-
  catch(call(Goal_1, Dict), E, true),
  (   var(E)
  ->  jsonld_tuples(Dict, Triples, [context(Context)]),
      rdf_print_triples(Triples)
  ;   print_message(warning, E)
  ).





% NON-DCG INVOCATIONS %

%! rdf_print_dataset_term(+D) is det.
%! rdf_print_dataset_term(+D, +Opts) is det.

rdf_print_dataset_term(D) :-
  rdf_print_dataset_term(D, _{}).


rdf_print_dataset_term(D, Opts1) :-
  rdf_print_default_options(Opts1, Out, Opts2),
  dcg_with_output_to(Out, dcg_rdf_print_dataset_term(D, Opts2)).



%! rdf_print_datatype(+D) is det.
%! rdf_print_datatype(+D, +Opts) is det.

rdf_print_datatype(D) :-
  rdf_print_datatype(D, _{}).


rdf_print_datatype(D, Opts1) :-
  rdf_print_default_options(Opts1, Out, Opts2),
  dcg_with_output_to(Out, dcg_rdf_print_datatype(D, Opts2)).



%! rdf_print_cbd(?M, ?S) is det.
%! rdf_print_cbd(?M, ?S, ?G) is det.
%! rdf_print_cbd(?M, ?S, ?G, +Opts) is det.
%
% Print the Concise-Bounded Description (CBD) of subject terms.

rdf_print_cbd(M, S) :-
  rdf_print_cbd(M, S, _).


rdf_print_cbd(M, S, G) :-
  rdf_print_cbd(M, S, G, []).


rdf_print_cbd(M, S, G, Opts) :-
  rdf_cbd_triples(M, S, G, Triples),
  rdf_print_triples(Triples, Opts).



rdf_print_graph(M, G) :-
  rdf_print_graph(M, G, _{}).


rdf_print_graph(M, G, Opts1) :-
  rdf_print_default_options(Opts1, Out, Opts2),
  dcg_with_output_to(Out, dcg_rdf_print_graph(M, G, Opts2)).



rdf_print_graph_term(G) :-
  rdf_print_graph_term(G, _{}).


rdf_print_graph_term(G, Opts1) :-
  rdf_print_default_options(Opts1, Out, Opts2),
  dcg_with_output_to(Out, dcg_rdf_print_graph_term(G, Opts2)).



rdf_print_iri(Lit) :-
  rdf_print_iri(Lit, _{}).


rdf_print_iri(Lit, Opts1) :-
  rdf_print_default_options(Opts1, Out, Opts2),
  dcg_with_output_to(Out, dcg_rdf_print_iri(Lit, Opts2)).



rdf_print_literal(Lit) :-
  rdf_print_literal(Lit, _{}).


rdf_print_literal(Lit, Opts1) :-
  rdf_print_default_options(Opts1, Out, Opts2),
  dcg_with_output_to(Out, dcg_rdf_print_literal(Lit, Opts2)).



rdf_print_object(O) :-
  rdf_print_object(O, _{}).


rdf_print_object(O, Opts1) :-
  rdf_print_default_options(Opts1, Out, Opts2),
  dcg_with_output_to(Out, dcg_rdf_print_object(O, Opts2)).



rdf_print_pagination(M, S, P, O, G) :-
  rdf_print_pagination(M, S, P, O, G, _{}).


rdf_print_pagination(M, S, P, O, G, Opts) :-
  create_pagination(Triple, rdf_triple(M, S, P, O, G, Triple), Opts, Result),
  cli_pagination_result(
    Result,
    {Opts}/[Results]>>rdf_print_triples(Results, Opts)
  ).



rdf_print_predicate(P) :-
  rdf_print_predicate(P, _{}).


rdf_print_predicate(P, Opts1) :-
  rdf_print_default_options(Opts1, Out, Opts2),
  dcg_with_output_to(Out, dcg_rdf_print_predicate(P, Opts2)).



rdf_print_quad(Tuple) :-
  rdf_print_quad(Tuple, _{}).


rdf_print_quad(Tuple, Opts1) :-
  rdf_print_default_options(Opts1, Out, Opts2),
  dcg_with_output_to(Out, dcg_rdf_print_quad(Tuple, Opts2)).



rdf_print_quad(S, P, O, G) :-
  rdf_print_quad(S, P, O, G, _{}).


rdf_print_quad(S, P, O, G, Opts1) :-
  rdf_print_default_options(Opts1, Out, Opts2),
  dcg_with_output_to(Out, dcg_rdf_print_quad(S, P, O, G, Opts2)).



rdf_print_quads(Tuples) :-
  rdf_print_quads(Tuples, _{}).


rdf_print_quads(Tuples, Opts1) :-
  rdf_print_default_options(Opts1, Out, Opts2),
  dcg_with_output_to(Out, dcg_rdf_print_quads(Tuples, Opts2)).



rdf_print_quads(M, S, P, O, G) :-
  rdf_print_quads(M, S, P, O, G, _{}).


rdf_print_quads(M, S, P, O, G, Opts1) :-
  rdf_print_default_options(Opts1, Out, Opts2),
  dcg_with_output_to(Out, dcg_rdf_print_quads(M, S, P, O, G, Opts2)).



%! rdf_print_root(?M, ?S) is det.
%! rdf_print_root(?M, ?S, ?G) is det.
%! rdf_print_root(?M, ?S, ?G, +Opts) is det.
%
% Print the tree for an RDF root node.

rdf_print_root(M, S) :-
  rdf_print_root(M, S, _).


rdf_print_root(M, S, G) :-
  rdf_print_root(M, S, G, []).


rdf_print_root(M, S, G, Opts) :-
  rdf_root(M, S, G),
  rdf_tree_triples(M, S, G, Triples),
  rdf_print_triples(Triples, Opts).



%! rdf_print_scbd(?M, ?Node) is det.
%! rdf_print_scbd(?M, ?Node, ?G) is det.
%! rdf_print_scbd(?M, ?Node, ?G, +Opts) is det.
%
% Print the Symmetric CBD (SCBD) for an RDF node.

rdf_print_scbd(M, Node) :-
  rdf_print_scbd(M, Node, _).


rdf_print_scbd(M, Node, G) :-
  rdf_print_scbd(M, Node, G, []).


rdf_print_scbd(M, Node, G, Opts) :-
  rdf_scbd_triples(M, Node, G, Triples),
  rdf_print_triples(Triples, Opts).



rdf_print_term(T) :-
  rdf_print_term(T, _{}).


rdf_print_term(T, Opts1) :-
  rdf_print_default_options(Opts1, Out, Opts2),
  dcg_with_output_to(Out, dcg_rdf_print_term(T, Opts2)).



%! rdf_print_tree(?M, ?S) is det.
%! rdf_print_tree(?M, ?S, ?G) is det.
%! rdf_print_tree(?M, ?S, ?G, +Opts) is det.
%
% Print the tree for a subject term.

rdf_print_tree(M, S) :-
  rdf_print_tree(M, S, _).


rdf_print_tree(M, S, G) :-
  rdf_print_tree(M, S, G, []).


rdf_print_tree(M, S, G, Opts) :-
  rdf_tree_triples(M, S, G, Triples),
  rdf_print_triples(Triples, Opts).



rdf_print_triple(Tuple) :-
  rdf_print_triple(Tuple, _{}).


rdf_print_triple(Tuple, Opts1) :-
  rdf_print_default_options(Opts1, Out, Opts2),
  dcg_with_output_to(Out, dcg_rdf_print_triple(Tuple, Opts2)).



rdf_print_triple(S, P, O) :-
  rdf_print_triple(S, P, O, _{}).


rdf_print_triple(S, P, O, Opts1) :-
  rdf_print_default_options(Opts1, Out, Opts2),
  dcg_with_output_to(Out, dcg_rdf_print_triple(S, P, O, Opts2)).



rdf_print_triples(Triples) :-
  rdf_print_triples(Triples, _{}).


rdf_print_triples(Triples, Opts1) :-
  rdf_print_default_options(Opts1, Out, Opts2),
  dcg_with_output_to(Out, dcg_rdf_print_triples(Triples, Opts2)).



rdf_print_triples(M, S, P, O) :-
  rdf_print_triples(M, S, P, O, _).


rdf_print_triples(M, S, P, O, G) :-
  rdf_print_triples(M, S, P, O, G, _{}).


rdf_print_triples(M, S, P, O, G, Opts1) :-
  rdf_print_default_options(Opts1, Out, Opts2),
  dcg_with_output_to(Out, dcg_rdf_print_triples(M, S, P, O, G, Opts2)).





% PRINT MULTIPLE TUPLES %

dcg_rdf_print_graph(M, G) -->
  {dcg_rdf_print_default_options(Opts)},
  dcg_rdf_print_graph(M, G, Opts).


dcg_rdf_print_graph(M, G, Opts) -->
  dcg_rdf_print_quads(M, _, _, _, G, Opts).



dcg_rdf_print_quads(Tuples) -->
  {dcg_rdf_print_default_options(Opts)},
  dcg_rdf_print_quads(Tuples, Opts).


dcg_rdf_print_quads([rdf(S,P,O)], Opts) --> !,
  dcg_rdf_print_triples0(0, [rdf(S,P,O,default)], Opts).
dcg_rdf_print_quads([rdf(S,P,O,G)], Opts) --> !,
  dcg_rdf_print_subject(S, Opts),
  " ",
  dcg_rdf_print_predicate(P, Opts),
  " ",
  dcg_rdf_print_object(O, Opts),
  " ",
  dcg_rdf_print_graph_term(G, Opts),
  " .",
  nl.
dcg_rdf_print_quads(Tuples, Opts) -->
  {
    maplist(graph_triple_pair0, Tuples, Pairs),
    keysort(Pairs, SortedPairs)
  },
  dcg_rdf_print_sorted_pairs0(SortedPairs, Opts).


graph_triple_pair0(rdf(S,P,O), G-rdf(S,P,O)) :-
  rdf_default_graph(G).
graph_triple_pair0(rdf(S,P,O,G), G-rdf(S,P,O)).



dcg_rdf_print_quads(M, S, P, O, G) -->
  {dcg_rdf_print_default_options(Opts)},
  dcg_rdf_print_quads(M, S, P, O, G, Opts).


dcg_rdf_print_quads(M, S, P, O, G, Opts) -->
  {aggregate_all(set(G-rdf(S,P,O)), q(M, S, P, O, G), SortedPairs)},
  dcg_rdf_print_sorted_pairs0(SortedPairs, Opts).



dcg_rdf_print_triples(Triples) -->
  {dcg_rdf_print_default_options(Opts)},
  dcg_rdf_print_triples(Triples, Opts).


dcg_rdf_print_triples(Triples, Opts) -->
  {rdf_default_graph(G)},
  dcg_rdf_print_groups0([G-Triples], Opts).


dcg_rdf_print_triples(M, S, P, O) -->
  dcg_rdf_print_triples(M, S, P, O, _).


dcg_rdf_print_triples(M, S, P, O, G) -->
  {dcg_rdf_print_default_options(Opts)},
  dcg_rdf_print_triples(M, S, P, O, G, Opts).


dcg_rdf_print_triples(M, S, P, O, G, Opts) -->
  {rdf_triples(M, S, P, O, G, Triples)},
  dcg_rdf_print_triples(Triples, Opts).


dcg_rdf_print_sorted_pairs0(SortedPairs, Opts) -->
  {group_pairs_by_key(SortedPairs, Groups)},
  dcg_rdf_print_groups0(Groups, Opts).


dcg_rdf_print_groups0([], _) --> !, [].
dcg_rdf_print_groups0([G-Triples|Groups], Opts) -->
  {dict_get(indent, Opts, 0, I1)},
  (   {rdf_default_graph(G)}
  ->  {I2 = I1}
  ;   tab(I1),
      dcg_rdf_print_graph_term(G, Opts),
      " {\n",
      {I2 = I1 + 1}
  ),
  dcg_rdf_print_triples0(I2, Triples, Opts),
  ({rdf_default_graph(G)} -> "" ; "}\n"),
  dcg_rdf_print_groups0(Groups, Opts).


dcg_rdf_print_triples0(I, Triples, Opts) -->
  {
    aggregate_all(set(S-po(P,O)), member(rdf(S,P,O), Triples), SortedPairs),
    group_pairs_by_key(SortedPairs, Groups)
  },
  dcg_rdf_print_subjects0(I, Groups, Opts).


dcg_rdf_print_subjects0(_, [], _) --> !, [].
dcg_rdf_print_subjects0(I1, [S-POs|Groups1], Opts) -->
  tab(I1),
  dcg_rdf_print_subject(S, Opts),
  {
    aggregate_all(set(P-O), member(po(P,O), POs), SortedPairs),
    group_pairs_by_key(SortedPairs, Groups2),
    I2 is I1 + 1
  },
  dcg_rdf_print_predicates1(I2, Groups2, Opts),
  ({Opts.newline == true} -> nl ; ""),
  dcg_rdf_print_subjects0(I1, Groups1, Opts).


% There is exactly one predicate.  Emit it on the same line.
dcg_rdf_print_predicates1(I, [P-Os], Opts) --> !,
  " ",
  dcg_rdf_print_predicate(P, Opts),
  dcg_rdf_print_objects1(I, Os, Opts),
  " .".
dcg_rdf_print_predicates1(I, Groups, Opts) -->
  dcg_rdf_print_predicates2(I, Groups, Opts).


dcg_rdf_print_predicates2(_, [], _) --> !, [].
dcg_rdf_print_predicates2(I1, [P-Os|Groups], Opts) -->
  nl,
  tab(I1),
  dcg_rdf_print_predicate(P, Opts),
  {I2 is I1 + 1},
  dcg_rdf_print_objects1(I2, Os, Opts),
  " ",
  ({Groups == []} -> "." ; ";"),
  dcg_rdf_print_predicates2(I1, Groups, Opts).


% There is exactly one object.  Emit it on the same line.
dcg_rdf_print_objects1(_, [O], Opts) --> !,
  " ",
  dcg_rdf_print_object(O, Opts).
dcg_rdf_print_objects1(I, Os, Opts) -->
  dcg_rdf_print_objects2(I, Os, Opts).


dcg_rdf_print_objects2(_, [], _) --> !, [].
dcg_rdf_print_objects2(I, [O|Os], Opts) -->
  nl,
  tab(I),
  dcg_rdf_print_object(O, Opts),
  ({Os == []} -> "" ; " ,"),
  dcg_rdf_print_objects2(I, Os, Opts).





% PRINT A SINGLE TUPLE %

dcg_rdf_print_quad(Tuple) -->
  {dcg_rdf_print_default_options(Opts)},
  dcg_rdf_print_quad(Tuple, Opts).


dcg_rdf_print_quad(rdf(S,P,O), Opts) --> !,
  {rdf_default_graph(G)},
  dcg_rdf_print_quad(rdf(S,P,O,G), Opts).
dcg_rdf_print_quad(rdf(S,P,O,G), Opts) -->
  dcg_rdf_print_quad(S, P, O, G, Opts).


dcg_rdf_print_quad(S, P, O, G) -->
  {dcg_rdf_print_default_options(Opts)},
  dcg_rdf_print_quad(S, P, O, G, Opts).


dcg_rdf_print_quad(S, P, O, G, Opts) -->
  dcg_rdf_print_quads([rdf(S,P,O,G)], Opts).



dcg_rdf_print_triple(Tuple) -->
  {dcg_rdf_print_default_options(Opts)},
  dcg_rdf_print_triple(Tuple, Opts).


dcg_rdf_print_triple(rdf(S,P,O), Opts) --> !,
  dcg_rdf_print_triple(S, P, O, Opts).
dcg_rdf_print_triple(rdf(S,P,O,_), Opts) -->
  dcg_rdf_print_triple(rdf(S,P,O), Opts).


dcg_rdf_print_triple(S, P, O) -->
  {dcg_rdf_print_default_options(Opts)},
  dcg_rdf_print_triple(S, P, O, Opts).


dcg_rdf_print_triple(S, P, O, Opts) -->
  dcg_rdf_print_subject(S, Opts),
  " ",
  dcg_rdf_print_predicate(P, Opts),
  " ",
  dcg_rdf_print_object(O, Opts).





% PRINT A TERM BY ITS POSITIONALITY %

dcg_rdf_print_dataset_term(D) -->
  {dcg_rdf_print_default_options(Opts)},
  dcg_rdf_print_dataset_term(D, Opts).


dcg_rdf_print_dataset_term(D, Opts) -->
  dcg_rdf_print_iri(D, Opts).



dcg_rdf_print_graph_term(G) -->
  {dcg_rdf_print_default_options(Opts)},
  dcg_rdf_print_graph_term(G, Opts).


dcg_rdf_print_graph_term(G, Opts) -->
  dcg_rdf_print_iri(G, Opts).



dcg_rdf_print_node(Node) -->
  {dcg_rdf_print_default_options(Opts)},
  dcg_rdf_print_node(Node, Opts).


dcg_rdf_print_node(Node, Opts) -->
  dcg_rdf_print_term(Node, Opts).



dcg_rdf_print_object(O) -->
  {dcg_rdf_print_default_options(Opts)},
  dcg_rdf_print_object(O, Opts).


dcg_rdf_print_object(O, Opts) -->
  {q_is_literal(O)}, !,
  dcg_rdf_print_literal(O, Opts).
dcg_rdf_print_object(O, Opts) -->
  dcg_rdf_print_subject(O, Opts).



dcg_rdf_print_predicate(P) -->
  {dcg_rdf_print_default_options(Opts)},
  dcg_rdf_print_predicate(P, Opts).


dcg_rdf_print_predicate(P, _) -->
  {
    ground(P),
    rdf_equal(rdf:type, P)
  }, !,
  "a".
dcg_rdf_print_predicate(P, Opts) -->
  {var(P)}, !,
  dcg_rdf_print_var(P, Opts).
dcg_rdf_print_predicate(P, Opts) -->
  dcg_rdf_print_iri(P, Opts).



dcg_rdf_print_subject(S) -->
  {dcg_rdf_print_default_options(Opts)},
  dcg_rdf_print_subject(S, Opts).


dcg_rdf_print_subject(S, _) -->
  {q_is_bnode(S)}, !,
  dcg_rdf_print_bnode(S).
dcg_rdf_print_subject(S, Opts) -->
  {var(S)}, !,
  dcg_rdf_print_var(S, Opts).
dcg_rdf_print_subject(S, Opts) -->
  {is_http_iri(S)}, !,
  dcg_rdf_print_iri(S, Opts).



dcg_rdf_print_term(T) -->
  {dcg_rdf_print_default_options(Opts)},
  dcg_rdf_print_term(T, Opts).


dcg_rdf_print_term(T, Opts) -->
  dcg_rdf_print_object(T, Opts).





% PRINT A TERM BY ITS KIND %

dcg_rdf_print_bnode(BNode) -->
  {q_is_bnode(BNode)}, !,
  atom(BNode).
dcg_rdf_print_bnode(BNode) -->
  {q_bnode_label(BNode, Lbl)}, !,
  "_:",
  atom(Lbl).



%! dcg_rdf_print_datatype(+D)// is det.
%! dcg_rdf_print_datatype(+D, +Opts)// is det.

dcg_rdf_print_datatype(D) -->
  {dcg_rdf_print_default_options(Opts)},
  dcg_rdf_print_datatype(D, Opts).


dcg_rdf_print_datatype(D, Opts) -->
  dcg_rdf_print_iri(D, Opts).



%! dcg_rdf_print_iri(+Iri)// is det.
%! dcg_rdf_print_iri(+Iri, +Opts)// is det.

dcg_rdf_print_iri(Iri) -->
  {dcg_rdf_print_default_options(Opts)},
  dcg_rdf_print_iri(Iri, Opts).


dcg_rdf_print_iri(Iri, Opts) -->
  {Opts.iri_lbl == true}, !,
  {
    setting(backend, M),
    q_pref_label_lex(M, Iri, Lex)
  },
  str(Lex).
/*
dcg_rdf_print_iri(Iri, Opts) -->
  {
    atom_after_char(S, '#', Local),
    Local \= ''
  }, !,
  atom(Local).
dcg_rdf_print_iri(Iri, Opts) -->
  {
    atom_after_char(S, '/', Local),
    Local \= ''
  }, !,
  atom(Local).
*/
dcg_rdf_print_iri(Full, Opts) -->
  {
    Opts.iri_abbr == true,
    (   dict_get(prefixes, Opts, Pairs),
        member(Alias-Prefix, Pairs),
        atom_prefix(Full, Prefix)
    ->  atom_concat(Prefix, Local, Full)
    ;   rdf_global_id(Alias:Local, Full)
    ->  true
    ), !,
    atom_length(Alias, AliasLen),
    Minus is AliasLen + 1,
    inf_minus(Opts.max_iri_len, Minus, Max)
  },
  atom(Alias),
  ":",
  atom_ellipsis(Local, Max).
dcg_rdf_print_iri(Full, Opts) -->
  "<",
  atom_ellipsis(Full, Opts.max_iri_len),
  ">".



dcg_rdf_print_language_tag(LTag, Opts) -->
  atom_ellipsis(LTag, Opts.max_lit_len).



dcg_rdf_print_lexical_form(Lex, Opts) -->
  "\"",
  atom_ellipsis(Lex, Opts.max_lit_len),
  "\"".



%! dcg_rdf_print_literal(+Lit)// is det.
%! dcg_rdf_print_literal(+Lit, +Opts)// is det.

dcg_rdf_print_literal(Lit) -->
  {dcg_rdf_print_default_options(Opts)},
  dcg_rdf_print_literal(Lit, Opts).


% Datatype hooks.
dcg_rdf_print_literal(Lit, Opts) -->
  q:dcg_rdf_print_literal_hook(Lit, Opts), !.
% Abbreviate XSD Boolean.
dcg_rdf_print_literal(Lex^^D, Opts) -->
  {rdf_equal(xsd:boolean, D)}, !,
  dcg_rdf_print_lexical_form(Lex, Opts).
% Abbreviate XSD string.
dcg_rdf_print_literal(V^^D, Opts) -->
  {rdf_equal(xsd:string, D)}, !,
  {atom_string(Lex, V)},
  dcg_rdf_print_lexical_form(Lex, Opts).
% Abbreviate XSD integers.
dcg_rdf_print_literal(Val^^D, _) -->
  {rdf_equal(xsd:integer, D)}, !,
  thousands(Val).
% Abbreviate XSD decimals and doubles.
dcg_rdf_print_literal(Val^^D, Opts) -->
  {
    (rdf_equal(xsd:decimal, D) ; rdf_equal(xsd:double, D)), !,
    atom_number(Lex, Val)
  },
  dcg_rdf_print_lexical_form(Lex, Opts).
% Unabbreviated datatype IRI that is not `rdf:langString`.
dcg_rdf_print_literal(V^^D, Opts) --> !,
  {rdf_literal_lexical_form(V^^D, Lex)},
  dcg_rdf_print_lexical_form(Lex, Opts),
  "^^",
  dcg_rdf_print_datatype(D, Opts).
% Language-tagged string datatype IRI.
dcg_rdf_print_literal(V@LTag, Opts) --> !,
  {atom_string(Lex, V)},
  dcg_rdf_print_lexical_form(Lex, Opts),
  "@",
  dcg_rdf_print_language_tag(LTag, Opts).



%! dcg_rdf_print_var(+Var, +Opts)// is det.

dcg_rdf_print_var(Var, Opts) -->
  "?",
  dcg_var(Opts.var_map, Var).





% PRINT A TERM BY THE SEMANTIC OBJECT IT PURPORTEDLY DENOTES %

%! dcg_rdf_print_class(+C)// is det.
%! dcg_rdf_print_class(+C, +Opts)// is det.

dcg_rdf_print_class(C) -->
  {dcg_rdf_print_default_options(Opts)},
  dcg_rdf_print_class(C, Opts).


dcg_rdf_print_class(C, Opts) -->
  dcg_rdf_print_iri(C, Opts).

  



% HELPERS %

%! dcg_rdf_print_default_options(-Opts) is det.

dcg_rdf_print_default_options(
  _{
    iri_abbr: true,
    iri_lbl: false,
    max_iri_len: inf,
    max_lit_len: inf,
    newline: true
  }
).



%! inf_minus(+X, +Y, -Z) is det.
  
inf_minus(inf, _, inf) :- !.
inf_minus(X, Y, X) :-
  X =< Y, !.
inf_minus(X, Y, Z) :-
  Z is X - Y.



%! rdf_print_default_options(+Opts1, -Out, -Opts2) is det.

rdf_print_default_options(Opts1, Out, Opts5) :-
  del_dict_or_default(out, Opts1, current_output, Out, Opts3),
  dcg_rdf_print_default_options(Opts4),
  merge_dicts(Opts4, Opts3, Opts5).
