:- module(
  rdf_print,
  [
    rdf_pp_deref_triple/1,   % +Uri
    rdf_pp_deref_triple/2,   % +Uri, +Options
    rdf_pp_graph/2,          % +M, +G
    rdf_pp_options/3,        % +Options1, -Out, -Options2
    rdf_pp_quad_groups/1,    % +JoinedPairs:list(pair(atom,list(compound)))
    rdf_pp_quad_groups/2,    % +JoinedPairs:list(pair(atom,list(compound))), +Options
    rdf_pp_triple/3,         % +S, +P, +O
    rdf_pp_triple/4,         % +S, +P, +O, +Options
    rdf_pp_triples/1,        % +Triples
    rdf_pp_triples/2,        % +Triples, +Options
    % DCG
    rdf_dcg_bnode//1,        % +BNode
    rdf_dcg_datatype_iri//1, % +D
    rdf_dcg_datatype_iri//2, % +D, +Options
    rdf_dcg_graph_term//1,   % +G
    rdf_dcg_graph_term//2,   % +G, +Options
    rdf_dcg_iri//1,          % +Iri
    rdf_dcg_iri//2,          % +Iri, +Options
    rdf_dcg_language_tag//2, % +LTag, +Options
    rdf_dcg_lexical_form//2, % +Lex, +Options
    rdf_dcg_literal//1,      % +Lit
    rdf_dcg_literal//2,      % +Lit, +Options
    rdf_dcg_node//1,         % +Node
    rdf_dcg_node//2,         % +Node, +Options
    rdf_dcg_object//1,       % +O
    rdf_dcg_object//2,       % +O, +Options
    rdf_dcg_options/2,       % +Options1, -Options2
    rdf_dcg_predicate//1,    % +P
    rdf_dcg_predicate//2,    % +P, +Options
    rdf_dcg_subject//1,      % +S
    rdf_dcg_subject//2,      % +S, +Options
    rdf_dcg_term//1,         % +Term
    rdf_dcg_term//2,         % +Term, +Options
    rdf_dcg_triple//3,       % +S, +P, +O
    rdf_dcg_triple//4,       % +S, +P, +O, +Options
    rdf_dcg_triples//1,      % +Triples
    rdf_dcg_triples//2,      % +Triples, +Options
    rdf_dcg_var//2           % +Var, +Options
  ]
).

/** <module> RDF print

| **Key**        | **Value**  | **Default** | **Description**                  |
|:---------------|:-----------|:-----------:|:---------------------------------|
| `indent`       | nonneg     | 0           |                                  |
| `iri_abbr`     | boolean    | `true`      | Whether IRIs are abbreviated     |
|                |            |             | based on the current prefixes.   |
| `max_iri_len`  | nonneg     | `inf`       | The maximum length of an IRI.    |
| `max_lit_len`  | nonneg     | `inf`       | The maximum length of a literal. |
| `prefix_map`   | list(pair( |             | A custom list of prefix/IRI      |
|                | atom))     |             | mappings that overrules and/or   |
|                |            |             | extends the prefix declarations. |
| `variable_map` | list(pair( |             | A list of variable/atom mappings |
|                | var,atom)) |             | that are required when variables |
|                |            |             | are printed.                     |

@author Wouter Beek
@version 2017/04-2017/08
*/

:- use_module(library(aggregate)).
:- use_module(library(atom_ext)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(semweb/rdf_ext)).

:- multifile
    rdf_print:rdf_dcg_literal_hook//2.

:- rdf_meta
   rdf_deref_triple(r),
   rdf_deref_triple(r, +),
   rdf_pp_graph(+, r),
   rdf_pp_triple(r, r, o),
   rdf_pp_triple(r, r, o, +),
   % DCG
   rdf_dcg_datatype_iri(r, ?, ?),
   rdf_dcg_datatype_iri(r, +, ?, ?),
   rdf_dcg_graph_term(r, ?, ?),
   rdf_dcg_graph_term(r, +, ?, ?),
   rdf_dcg_iri(r, ?, ?),
   rdf_dcg_iri(r, +, ?, ?),
   rdf_dcg_literal(o, ?, ?),
   rdf_dcg_literal(o, +, ?, ?),
   rdf_dcg_node(o, ?, ?),
   rdf_dcg_node(o, +, ?, ?),
   rdf_dcg_object(o, ?, ?),
   rdf_dcg_object(o, +, ?, ?),
   rdf_dcg_predicate(o, ?, ?),
   rdf_dcg_predicate(o, +, ?, ?),
   rdf_dcg_subject(o, ?, ?),
   rdf_dcg_subject(o, +, ?, ?),
   rdf_dcg_term(o, ?, ?),
   rdf_dcg_term(o, +, ?, ?),
   rdf_dcg_triple(o, o, o, ?, ?),
   rdf_dcg_triple(o, o, o, +, ?, ?).





%! rdf_pp_deref_triple(+Uri) is nondet.
%! rdf_pp_deref_triple(+Uri, +Options) is nondet.

rdf_pp_deref_triple(Uri) :-
  rdf_pp_deref_triple(Uri, []).


rdf_pp_deref_triple(Uri, Options) :-
  rdf_deref_triple(Uri, rdf(S,P,O), Options),
  rdf_pp_triple(S, P, O, Options).



%! rdf_pp_graph(+M, +G) is det.

rdf_pp_graph(M, G) :-
  aggregate_all(set(rdf(S,P,O)), rdf(M, S, P, O, G), Triples),
  rdf_pp_triples(Triples).



%! rdf_pp_options(+Options1, -Out, -Options2) is det.

rdf_pp_options(Options1, Out, Options5) :-
  dict_delete_or_default(out, Options1, current_output, Out, Options3),
  rdf_dcg_options(Options4),
  merge_dicts(Options3, Options4, Options5).



%! rdf_pp_quad_groups(+JoinedPairs:list(pair(atom,list(compound)))) is det.
%! rdf_pp_quad_groups(+JoinedPairs:list(pair(atom,list(compound))), +Options) is det.

rdf_pp_quad_groups(JoinedPairs) :-
  rdf_pp_quad_groups(JoinedPairs, _{}).


rdf_pp_quad_groups(JoinedPairs, Options1) :-
  rdf_pp_options(Options1, Out, Options2),
  dcg_with_output_to(Out, rdf_dcg_groups0(JoinedPairs, Options2)).



%! rdf_pp_triple(+S, +P, +O) is det.
%! rdf_pp_triple(+S, +P, +O, +Options) is det.

rdf_pp_triple(S, P, O) :-
  rdf_pp_triple(S, P, O, _{}).


rdf_pp_triple(S, P, O, Options1) :-
  rdf_pp_options(Options1, Out, Options2),
  dcg_with_output_to(Out, rdf_dcg_triple(S, P, O, Options2)).



%! rdf_pp_triples(+Triples) is det.
%! rdf_pp_triples(+Triples, +Options) is det.

rdf_pp_triples(Triples) :-
  rdf_pp_triples(Triples, _{}).


rdf_pp_triples(Triples, Options1) :-
  rdf_pp_options(Options1, Out, Options2),
  dcg_with_output_to(Out, rdf_dcg_triples(Triples, Options2)).





% DCG %

%! rdf_dcg_bnode(+BNode)// is det.

rdf_dcg_bnode(BNode) -->
  {rdf_is_bnode(BNode)}, !,
  atom(BNode).
rdf_dcg_bnode(BNode) -->
  {rdf_global_id(bnode:Local, BNode)},
  "_:",
  atom(Local).



%! rdf_dcg_datatype_iri(+D)// is det.
%! rdf_dcg_datatype_iri(+D, +Options)// is det.

rdf_dcg_datatype_iri(D) -->
  {rdf_dcg_options(Options)},
  rdf_dcg_datatype_iri(D, Options).


rdf_dcg_datatype_iri(D, Options) -->
  rdf_dcg_iri(D, Options).



%! rdf_dcg_graph_term(+G)// is det.

rdf_dcg_graph_term(G) -->
  {rdf_dcg_options(Options)},
  rdf_dcg_graph_term(G, Options).


rdf_dcg_graph_term(G, Options) -->
  rdf_dcg_iri(G, Options).



%! rdf_dcg_iri(+Iri)// is det.
%! rdf_dcg_iri(+Iri, +Options)// is det.

rdf_dcg_iri(Iri) -->
  {rdf_dcg_options(Options)},
  rdf_dcg_iri(Iri, Options).


rdf_dcg_iri(Full, Options) -->
  {
    Options.iri_abbr == true,
    (   % Abbreviated based on a manually passed prefix/IRI pair.
        dict_get(prefix_map, Options, PrefixMap),
        member(Prefix-Iri, PrefixMap),
        atom_prefix(Full, Iri)
    ->  atom_concat(Iri, Local, Full)
    ;   % Abbreviated based on a global prefix declaration.
        rdf_global_id(Prefix:Local, Full)
    ->  true
    ), !,
    atom_length(Prefix, PrefixLength),
    Minus is PrefixLength + 1,
    inf_minus(Options.max_iri_len, Minus, Max)
  },
  atom(Prefix),
  ":",
  ellipsis(Local, Max).
rdf_dcg_iri(Full, Options) -->
  "<",
  ellipsis(Full, Options.max_iri_len),
  ">".



%! rdf_dcg_language_tag(+LTag, +Options)// is det.

rdf_dcg_language_tag(LTag, Options) -->
  ellipsis(LTag, Options.max_lit_len).



%! rdf_dcg_lexical_form(+Lex, +Options)// is det.

rdf_dcg_lexical_form(Lex, Options) -->
  [34],
  ellipsis(Lex, Options.max_lit_len),
  [34].



%! rdf_dcg_literal(+Lit)// is det.
%! rdf_dcg_literal(+Lit, +Options)// is det.

rdf_dcg_literal(Lit) -->
  {rdf_dcg_options(Options)},
  rdf_dcg_literal(Lit, Options).


% Datatype hooks.
rdf_dcg_literal(Lit, Options) -->
  rdf_print:rdf_dcg_literal_hook(Lit, Options), !.
% Abbreviate XSD Boolean.
rdf_dcg_literal(Lex^^D, Options) -->
  {rdf_equal(xsd:boolean, D)}, !,
  rdf_dcg_lexical_form(Lex, Options).
% Abbreviate XSD string.
rdf_dcg_literal(V^^D, Options) -->
  {rdf_equal(xsd:string, D)}, !,
  {atom_string(Lex, V)},
  rdf_dcg_lexical_form(Lex, Options).
% Abbreviate XSD integers.
rdf_dcg_literal(Val^^D, _) -->
  {(rdf_equal(xsd:integer, D) ; rdf_equal(xsd:int, D))}, !,
  thousands(Val).
% Abbreviate XSD decimals and doubles.
rdf_dcg_literal(Val^^D, Options) -->
  {
    (rdf_equal(xsd:decimal, D) ; rdf_equal(xsd:double, D)), !,
    atom_number(Lex, Val)
  },
  rdf_dcg_lexical_form(Lex, Options).
% Unabbreviated datatype IRI that is not `rdf:langString`.
rdf_dcg_literal(V^^D, Options) --> !,
  {rdf_literal(V^^D, _, Lex, _)},
  rdf_dcg_lexical_form(Lex, Options),
  "^^",
  rdf_dcg_datatype_iri(D, Options).
% Language-tagged string datatype IRI.
rdf_dcg_literal(V@LTag, Options) --> !,
  {atom_string(Lex, V)},
  rdf_dcg_lexical_form(Lex, Options),
  "@",
  rdf_dcg_language_tag(LTag, Options).



%! rdf_dcg_node(+Node)// is det.
%! rdf_dcg_node(+Node, +Options)// is det.

rdf_dcg_node(Node) -->
  {rdf_dcg_options(Options)},
  rdf_dcg_node(Node, Options).


rdf_dcg_node(Node, Options) -->
  rdf_dcg_term(Node, Options).



%! rdf_dcg_object(+O)// is det.
%! rdf_dcg_object(+O, +Options)// is det.

rdf_dcg_object(O) -->
  {rdf_dcg_options(Options)},
  rdf_dcg_object(O, Options).


rdf_dcg_object(O, Options) -->
  rdf_dcg_term(O, Options).



%! rdf_dcg_options(+Options1, -Options2) is det.
%
% Options for the ‘rdf_print_*’ predicates are automatically resolved,
% but for the DCG rules ‘rdf_dcg_*’ options need to merged
% explicitly using this predicate.

rdf_dcg_options(Options1, Options3) :-
  rdf_dcg_options(Options2),
  merge_dicts(Options1, Options2, Options3).



%! rdf_dcg_predicate(+P)// is det.
%! rdf_dcg_predicate(+P, +Options)// is det.

rdf_dcg_predicate(P) -->
  {rdf_dcg_options(Options)},
  rdf_dcg_predicate(P, Options).


rdf_dcg_predicate(P, _) -->
  {
    ground(P),
    rdf_equal(rdf:type, P)
  }, !,
  "a".
rdf_dcg_predicate(P, Options) -->
  rdf_dcg_term(P, Options).



%! rdf_dcg_subject(+S)// is det.
%! rdf_dcg_subject(+S, +Options)// is det.

rdf_dcg_subject(S) -->
  {rdf_dcg_options(Options)},
  rdf_dcg_subject(S, Options).


rdf_dcg_subject(S, Options) -->
  rdf_dcg_term(S, Options).



%! rdf_dcg_term(+Term)// is det.
%! rdf_dcg_term(+Term, +Options)// is det.

rdf_dcg_term(Term) -->
  {rdf_dcg_options(Options)},
  rdf_dcg_term(Term, Options).


rdf_dcg_term(BNode, _) -->
  {rdf_is_bnode(BNode)}, !,
  atom(BNode).
rdf_dcg_term(Iri, Options) -->
  {rdf_is_iri(Iri)}, !,
  rdf_dcg_iri(Iri, Options).
rdf_dcg_term(Lit, Options) -->
  {rdf_is_literal(Lit)}, !,
  rdf_dcg_literal(Lit, Options).
rdf_dcg_term(Var, Options) -->
  {var(Var)}, !,
  rdf_dcg_var(Var, Options).



%! rdf_dcg_triple(+S, +P, +O)// is det.
%! rdf_dcg_triple(+S, +P, +O, +Options)// is det.

rdf_dcg_triple(S, P, O) -->
  {rdf_dcg_options(Options)},
  rdf_dcg_triple(S, P, O, Options).


rdf_dcg_triple(S, P, O, Options) -->
  rdf_dcg_subject(S, Options),
  " ",
  rdf_dcg_predicate(P, Options),
  " ",
  rdf_dcg_object(O, Options).



rdf_dcg_triples(Triples) -->
  {rdf_dcg_options(Options)},
  rdf_dcg_triples(Triples, Options).


rdf_dcg_triples(Triples, Options) -->
  rdf_dcg_groups0([_NoGraph-Triples], Options).

rdf_dcg_groups0([], _) --> !, [].
rdf_dcg_groups0([G-Triples|Groups], Options) -->
  {dict_get(indent, Options, 0, I1)},
  (   {var(G)}
  ->  {I2 = I1}
  ;   dcg_tab(I1),
      rdf_dcg_graph_term(G, Options),
      " {\n",
      {I2 = I1 + 1}
  ),
  rdf_dcg_triples0(I2, Triples, Options),
  ({var(G)} -> "" ; "}\n"),
  rdf_dcg_groups0(Groups, Options).

rdf_dcg_triples0(I, Triples, Options) -->
  {
    aggregate_all(set(S-po(P,O)), member(rdf(S,P,O), Triples), SortedPairs),
    group_pairs_by_key(SortedPairs, Groups)
  },
  rdf_dcg_subjects0(I, Groups, Options).

rdf_dcg_subjects0(_, [], _) --> !, [].
rdf_dcg_subjects0(I1, [S-POs|Groups1], Options) -->
  dcg_tab(I1),
  rdf_dcg_subject(S, Options),
  {
    aggregate_all(set(P-O), member(po(P,O), POs), SortedPairs),
    group_pairs_by_key(SortedPairs, Groups2),
    I2 is I1 + 1
  },
  rdf_dcg_predicates1(I2, Groups2, Options),
  ({Options.newline == true} -> nl ; ""),
  rdf_dcg_subjects0(I1, Groups1, Options).

% There is exactly one predicate.  Emit it on the same line.
rdf_dcg_predicates1(I, [P-Os], Options) --> !,
  " ",
  rdf_dcg_predicate(P, Options),
  rdf_dcg_objects1(I, Os, Options),
  " .".
rdf_dcg_predicates1(I, Groups, Options) -->
  rdf_dcg_predicates2(I, Groups, Options).

rdf_dcg_predicates2(_, [], _) --> !, [].
rdf_dcg_predicates2(I1, [P-Os|Groups], Options) -->
  nl,
  dcg_tab(I1),
  rdf_dcg_predicate(P, Options),
  {I2 is I1 + 1},
  rdf_dcg_objects1(I2, Os, Options),
  " ",
  ({Groups == []} -> "." ; ";"),
  rdf_dcg_predicates2(I1, Groups, Options).

% There is exactly one object.  Emit it on the same line.
rdf_dcg_objects1(_, [O], Options) --> !,
  " ",
  rdf_dcg_object(O, Options).
rdf_dcg_objects1(I, Os, Options) -->
  rdf_dcg_objects2(I, Os, Options).

rdf_dcg_objects2(_, [], _) --> !, [].
rdf_dcg_objects2(I, [O|Os], Options) -->
  nl,
  dcg_tab(I),
  rdf_dcg_object(O, Options),
  ({Os == []} -> "" ; " ,"),
  rdf_dcg_objects2(I, Os, Options).



%! rdf_dcg_var(+Var, +Options)// is det.

rdf_dcg_var(Var, Options) -->
  {dict_get(variable_map, Options, VarMap)},
  "?",
  {memberchk_eq_key(Var, VarMap, VarName)}, !,
  atom(VarName).
rdf_dcg_var(Var, _) -->
  {format(codes(Cs), "~w", [Var])},
  Cs.

memberchk_eq_key(Key, [Key0-Val|_], Val) :-
  Key == Key0, !.
memberchk_eq_key(Key, [_|T], Val) :-
  memberchk_eq_key(Key, T, Val).





% HELPERS %

%! inf_minus(+X, +Y, -Z) is det.
  
inf_minus(inf, _, inf) :- !.
inf_minus(X, Y, X) :-
  X =< Y, !.
inf_minus(X, Y, Z) :-
  Z is X - Y.



%! rdf_dcg_options(-Options) is det.

rdf_dcg_options(
  _{
    iri_abbr: true,
    iri_lbl: false,
    max_iri_len: inf,
    max_lit_len: inf,
    newline: true
  }
).



%! tab// is det.

tab --> "  ".
