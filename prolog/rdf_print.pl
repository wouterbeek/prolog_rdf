:- encoding(utf8).
:- module(
  rdf_print,
  [
    rdf_dcg_node//1,      % +Node
    rdf_dcg_node//2,      % +Node, +Options
    rdf_dcg_predicate//1, % +Predicate
    rdf_dcg_predicate//2, % +Predicate, +Options
    rdf_dcg_tp//1,        % +TP
    rdf_dcg_tp//2,        % +TP, +Options
    rdf_dcg_tree//1,      % +Tree
    rdf_dcg_tree//2,      % +Tree, +Options
    rdf_dcg_triples//1,   % +Triples
    rdf_dcg_triples//2    % +Triples, +Options
  ]
).

/** <module> RDF printing

| **Key**        | **Value**            | **Default** | **Description**                    |
|:---------------|:---------------------|:-----------:|:-----------------------------------|
| `indent`       | nonneg               | 0           | The number of spaces for the outer |
|                |                      |             | indentation level.                 |
| `iri_abbr`     | boolean              | `true`      | Whether IRIs are abbreviated       |
|                |                      |             | based on the current prefixes.     |
| `max_length`   | nonneg               | `inf`       | The maximum length of an RDF term. |
| `prefix_map`   | list(pair(atom))     |             | A custom list of prefix/IRI        |
|                |                      |             | mappings that overrules and/or     |
|                |                      |             | extends the prefix declarations.   |

*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(clpfd)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(solution_sequences)).

:- use_module(library(abnf)).
:- use_module(library(atom_ext)).
:- use_module(library(call_ext)).
:- use_module(library(dcg)).
:- use_module(library(dict)).
:- use_module(library(rdf_prefix)).
:- use_module(library(rdf_term)).

:- rdf_meta
   rdf_dcg_literal(t, +, ?, ?),
   rdf_dcg_node(o, ?, ?),
   rdf_dcg_node(o, +, ?, ?),
   rdf_dcg_predicate(r, ?, ?),
   rdf_dcg_predicate(r, +, ?, ?),
   rdf_dcg_tp(t, ?, ?),
   rdf_dcg_tp(t, +, ?, ?),
   rdf_dcg_tree(t),
   rdf_dcg_tree(t, +),
   rdf_dcg_triples(t, ?, ?),
   rdf_dcg_triples(t, +, ?, ?).



%! rdf_dcg_bnode(+BNode:rdf_bnode, +Options:options)// is det.

rdf_dcg_bnode(BNode, Options) -->
  {dict_get(max_length, Options, inf, Max)},
  "_:",
  ellipsis(BNode, Max).



%! rdf_dcg_iri(+Iri:iri, +Options:options)// is det.

% Abbreviated IRI notation.
rdf_dcg_iri(Iri, Options) -->
  {
    dict_get(iri_abbr, Options, true, true),
    (   dict_get(prefix_map, Options, Prefix2Alias)
    ->  % Abbreviated based on the prefix map specified in options.
        (   gen_assoc(Prefix, Prefix2Alias, Alias),
            atom_prefix(Iri, Prefix)
        ->  atom_concat(Prefix, Local, Iri)
        )
    ;   % Abbreviated based on the global prefix declarations.
        rdf_prefix_iri(Alias:Local, Iri),
        \+ sub_atom(Local, /)
    )
  }, !,
  {
    atom_length(Alias, AliasLength),
    Minus #= AliasLength + 1,
    dict_get(max_length, Options, inf, Length),
    (   Length == inf
    ->  Max = inf
    ;   Length =< Minus
    ->  Max = Length
    ;   Max = Length - Minus
    )
  },
  atom(Alias),
  ":",
  ellipsis(Local, Max).
% Full IRI notation.
rdf_dcg_iri(Iri, Options) -->
  {dict_get(max_length, Options, inf, Max)},
  "<",
  ellipsis(Iri, Max),
  ">".



%! rdf_dcg_lexical_form(+LexicalForm:atom, +Options:options)// is det.

rdf_dcg_lexical_form(Lex, Options) -->
  {
    dict_get(max_length, Options, inf, Length),
    extra_quotes_(Lex, ExtraQuotes)
  },
  ({ExtraQuotes == true} -> "\"\"\"" ; "\""),
  ellipsis(Lex, Length),
  ({ExtraQuotes == true} -> "\"\"\"" ; "\"").

lex_([0'\\|T]) --> !,
  "\\\\",
  lex_(T).
lex_([H|T]) --> !,
  [H],
  lex_(T).
lex_([]) --> "".

extra_quotes_(Atom, true) :-
  atom_codes(Atom, Codes),
  member(Code, [0'",0'\n]),%"
  memberchk(Code, Codes), !.
extra_quotes_(_, false).



%! rdf_dcg_litteral(+Litteral:rdf_literal, +Options:options)// is det.

% Language-tagged string.
rdf_dcg_literal(literal(lang(LTag,Lex)), Options) --> !,
  rdf_dcg_lexical_form(Lex, Options),
  "@",
  atom(LTag).
% Typed literal with abbreviated notation.
rdf_dcg_literal(literal(type(D,Lex)), Options) -->
  {
    rdf_prefix_memberchk(
      D,
      [xsd:boolean,xsd:decimal,xsd:float,xsd:integer,xsd:string]
    ), !,
    rdf_canonical_lexical_form(D, Lex, CanonicalLex)
  },
  rdf_dcg_lexical_form(CanonicalLex, Options).
% Typed literal without abbreviated notation.
rdf_dcg_literal(literal(type(D,Lex)), Options) -->
  rdf_dcg_lexical_form(Lex, Options),
  "^^",
  rdf_dcg_iri(D, Options).



%! rdf_dcg_node(+Node:rdf_node)// is det.
%! rdf_dcg_node(+Node:rdf_node, +Options:options)// is det.

rdf_dcg_node(Node) -->
  rdf_dcg_node(Node, options{}).


rdf_dcg_node(Var, _) -->
  {var(Var)}, !,
  {instantiation_error(Var)}.
% Blank nodes.
rdf_dcg_node(BNode, Options) -->
  {rdf_is_bnode(BNode)}, !,
  rdf_dcg_bnode(BNode, Options).
% Literals.
rdf_dcg_node(Literal, Options) -->
  {rdf_is_literal(Literal)}, !,
  rdf_dcg_literal(Literal, Options).
% Well-known IRIs (blank node notation).
rdf_dcg_node(Iri, Options) -->
  {well_known_iri([''|Segments], Iri)}, !,
  {atomic_list_concat(Segments, /, BNode)},
  rdf_dcg_bnode(BNode, Options).
% IRIs that are not well-known IRIs.
rdf_dcg_node(Iri, Options) -->
  {rdf_is_iri(Iri)}, !,
  rdf_dcg_iri(Iri, Options).
% Syntax error.
rdf_dcg_node(Term, _) -->
  {syntax_error(rdf_term(Term))}.



%! rdf_dcg_predicate(+Predicate:iri)// is det.
%! rdf_dcg_predicate(+Predicate:iri, +Options:options)// is det.

rdf_dcg_predicate(P) -->
  rdf_dcg_predicate(P, options{}).


% BUG: RDF prefix expansion does not work.
rdf_dcg_predicate(rdf:type, Options) -->
  {dict_get(iri_abbr, Options, true, true)}, !,
  "a".
rdf_dcg_predicate(Iri, Options) -->
  rdf_dcg_iri(Iri, Options).



%! rdf_dcg_tp(+TriplePattern:compound)// is det.
%! rdf_dcg_tp(+TriplePattern:compound, +Options:options)// is det.

rdf_dcg_tp(tp(S,P,O)) -->
  rdf_dcg_tp(tp(S,P,O), options{}).


rdf_dcg_tp(tp(S,P,O), Options) -->
  rdf_dcg_node(S, Options),
  " ",
  rdf_dcg_predicate(P, Options),
  " ",
  rdf_dcg_node(O, Options).



%! rdf_dcg_tree(+Tree:compound)// is det.
%! rdf_dcg_tree(+Tree:compound, +Options:options)// is det.

rdf_dcg_tree(Tree) -->
  rdf_dcg_tree(Tree, options{}).


rdf_dcg_tree(Tree, Options) -->
  {dict_get(indent, Options, 0, N)},
  rdf_dcg_tree_(N, Options, Tree).

rdf_dcg_tree_(N1, Options, tree(Rule,Concl,Trees)) -->
  tab(N1),
  ({N1 =:= 0} -> "" ; "⤷ "),
  "[",
  term(Rule),
  "] ",
  rdf_dcg_tp(Concl, Options),
  nl,
  {N2 is N1 + 2},
  '*'(rdf_dcg_tree_(N2, Options), Trees).



%! rdf_dcg_triples(+Triples:list(tp))// is det.
%! rdf_dcg_triples(+Triples:list(tp), +Options:options)// is det.
%
% Prints the given triples, using the abbreviations defined in Turtle
% 1.1.
%
% @see https://www.w3.org/TR/turtle/

rdf_dcg_triples(Triples) -->
  rdf_dcg_triples(Triples, options{}).


rdf_dcg_triples(Triples, Options) -->
  {dict_get(graph, Options, _NoGraph, GraphName)},
  rdf_dcg_prefixes(Triples),
  rdf_dcg_groups0([GraphName-Triples], Options).

rdf_dcg_prefixes(Triples) -->
  {
    aggregate_all(
      set(Alias-Prefix),
      (
        member(tp(S,P,O), Triples),
        member(Term, [S,P,O]),
        term_iri_(Term, Iri),
        rdf_prefix_iri(Alias, _, Iri),
        rdf_prefix(Alias, Prefix)
      ),
      Pairs
    )
  },
  '*'(rdf_dcg_prefix, Pairs), !,
  ({Pairs == []} -> "" ; "\n").

term_iri_(Iri, Iri) :-
  rdf_is_iri(Iri), !.
term_iri_(Literal, Iri) :-
  rdf_literal_datatype_iri(Literal, Iri).

rdf_dcg_prefix(Alias-Prefix) -->
  "prefix ",
  atom(Alias),
  ": <",
  atom(Prefix),
  ">\n".

rdf_dcg_groups0([], _) --> !, "".
rdf_dcg_groups0([G-Triples|Groups], Options) -->
  {dict_get(indent, Options, 0, I1)},
  (   {var(G)}
  ->  {I2 = I1}
  ;   tab(I1),
      rdf_dcg_node(G, Options),
      " {\n",
      {I2 = I1 + 2}
  ),
  rdf_dcg_triples0(I2, Triples, Options),
  ({var(G)} -> "" ; "}\n"),
  rdf_dcg_groups0(Groups, Options).

rdf_dcg_triples0(I, Triples, Options) -->
  {
    partition(is_skip_tp_(Triples), Triples, SkipTriples, NonSkipTriples),
    triples_to_groups0(NonSkipTriples, Groups)
  },
  rdf_dcg_subjects0(I, Groups, SkipTriples, Options).

%! is_skip_tp_(+TP:tp) is semidet.
%
% Succeeds if TP should be skipped for the purposes of printing.

is_skip_tp_(Triples, tp(S,_,_)) :-
  ( % blank node subject term
    rdf_is_bnode(S)
  ; % well-known IRI subject term
    rdf_is_bnode_iri(S)
  ), !,
  memberchk(tp(_,_,S), Triples), !.
% RDF list
is_skip_tp_(_, tp(_,P,_)) :-
  rdf_prefix_memberchk(P, [rdf:first,rdf:rest]), !.
% blank node object term
is_skip_tp_(_, tp(_,_,O)) :-
  rdf_is_bnode(O).

%! triples_to_groups0(+Triples:list(tp),
%!                    -Groups:ordset(pair(rdf_node,ordset(pair(rdf_predicate,ordset(rdf_node)))))) is det.

triples_to_groups0(Triples, Groups) :-
  aggregate_all(
    set(S-SGroups),
    (
      distinct(S, member(tp(S,_,_), Triples)),
      triples_to_groups1(S, Triples, SGroups)
    ),
    Groups
  ).

triples_to_groups1(S, Triples, SGroups) :-
  aggregate_all(
    set(P-Os),
    (
      distinct(P, member(tp(S,P,_), Triples)),
      triples_to_groups2(S, P, Triples, Os)
    ),
    SGroups
  ).

triples_to_groups2(S, P, Triples, Os) :-
  aggregate_all(set(O), member(tp(S,P,O), Triples), Os).

%! rdf_dcg_subjects0(+Indent:nonneg,
%!                   +Groups,
%!                   +SkipTriples:list(tp),
%!                   +Options:options)// is det.

rdf_dcg_subjects0(_, [], _, _) --> !, "".
rdf_dcg_subjects0(I1, [S-SGroups|Groups], SkipTriples1, Options) -->
  tab(I1),
  rdf_dcg_node(S, Options),
  {I2 is I1 + 2},
  rdf_dcg_predicates1(I2, SGroups, SkipTriples1, SkipTriples2, Options),
  nl,
  ({Groups == []} -> "" ; nl),
  rdf_dcg_subjects0(I1, Groups, SkipTriples2, Options).

% There is exactly one predicate-object pair; emit it on the same
% line.
rdf_dcg_predicates1(I, [P-[O]], SkipTriples1, SkipTriples2, Options) --> !,
  " ",
  rdf_dcg_predicate(P, Options),
  rdf_dcg_complex_object_(I, true, O, SkipTriples1, SkipTriples2, Options),
  ".".
% There is more than one predicate-object pair; do something special.
rdf_dcg_predicates1(I, SGroups1, SkipTriples1, SkipTriples2, Options) -->
  % Display instance-of statements first (regardless of predicate term
  % order).
  {types_to_the_front(SGroups1, SGroups2)},
  rdf_dcg_predicates2(I, false, false, SGroups2, SkipTriples1, SkipTriples2, Options).

types_to_the_front(SGroups1, [P-Os|SGroups2]) :-
  rdf_prefix_iri(rdf, type, P),
  selectchk(P-Os, SGroups1, SGroups2), !.
types_to_the_front(SGroups, SGroups).

% No more predicates.
rdf_dcg_predicates2(_, _, _, [], SkipTriples, SkipTriples, _) --> !, "".
% Another predicate.
rdf_dcg_predicates2(I1, StartOfBlock, InBlock, [P-Os|Groups], SkipTriples1, SkipTriples3, Options) -->
  start_of_block(I1, StartOfBlock),
  rdf_dcg_predicate(P, Options),
  {I2 is I1 + 2},
  rdf_dcg_objects1(I2, Os, SkipTriples1, SkipTriples2, Options),
  ({Groups == []} -> ({InBlock == true} -> " " ; ".") ; ";"),
  rdf_dcg_predicates2(I1, false, InBlock, Groups, SkipTriples2, SkipTriples3, Options).

start_of_block(I, false) --> !,
  nl,
  tab(I).
start_of_block(_, true) --> " ".

% There is exactly one object.  Emit it on the same line.
rdf_dcg_objects1(I, [O], SkipTriples1, SkipTriples2, Options) --> !,
  rdf_dcg_complex_object_(I, true, O, SkipTriples1, SkipTriples2, Options).
% There are multiple objects: emit each one on its own line.
rdf_dcg_objects1(I, Os, SkipTriples1, SkipTriples2, Options) -->
  rdf_dcg_objects2(I, Os, SkipTriples1, SkipTriples2, Options).

% No more objects.
rdf_dcg_objects2(_, [], SkipTriples, SkipTriples, _) --> !, "".
% Another object.
rdf_dcg_objects2(I, [O|Os], SkipTriples1, SkipTriples3, Options) -->
  rdf_dcg_complex_object_(I, false, O, SkipTriples1, SkipTriples2, Options),
  ({Os == []} -> "" ; ","),
  rdf_dcg_objects2(I, Os, SkipTriples2, SkipTriples3, Options).

%! rdf_dcg_complex_object_(+Indent:nonneg,
%!                         +InLine:boolean,
%!                         +Node:rdf_node,
%!                         +SkipTriples1:list(tp),
%!                         +SkipTriples2:list(tp),
%!                         +Options:options)// is det.

% The object term is an RDF list (collection).
rdf_dcg_complex_object_(I, InLine, RdfList, SkipTriples1, SkipTriples2, Options) -->
  {
    linear_list_(RdfList, SkipTriples1, SkipTriples2, Terms),
    Terms = [_|_]
  }, !,
  ({InLine == true} -> " " ; nl, tab(I)),
  rdf_dcg_list_(Terms, Options).
% The object term can be emitted an anonymous node.
rdf_dcg_complex_object_(I1, _, Node, SkipTriples1, SkipTriples3, Options) -->
  {
    turtle_object_(Node, SkipTriples1, SkipTriples2, Pairs),
    Pairs = [_|_], !,
    group_pairs_by_key(Pairs, Groups),
    I2 is I1 + 2
  },
  nl,
  tab(I1),
  "[",
  rdf_dcg_predicates2(I2, true, true, Groups, SkipTriples2, SkipTriples3, Options),
  "]".
% The object term is an atomic term.
rdf_dcg_complex_object_(I, InLine, Term, SkipTriples, SkipTriples, Options) -->
  ({InLine == true} -> " " ; nl, tab(I)),
  rdf_dcg_node(Term, Options).

%! turtle_object_(+RdfList:rdf_node,
%!                +SkipTriples1:list(tp),
%!                -SkipTriples2:list(tp),
%!                -P_O_Pairs:list(pair(rdf_predicate,rdf_node))) is det.
turtle_object_(S, SkipTriples1, SkipTriples3, [P-O|T]) :-
  rdf_prefix_selectchk(tp(S,P,O), SkipTriples1, SkipTriples2), !,
  turtle_object_(S, SkipTriples2, SkipTriples3, T).
turtle_object_(_, SkipTriples, SkipTriples, []).

%! linear_list_(+RdfList:rdf_node,
%!              +SkipTriples1:list(tp),
%!              -SkipTriples2:list(tp),
%!              -Terms:list(rdf_term)) is det.

linear_list_(S1, SkipTriples1, SkipTriples4, [H|T]) :-
  rdf_prefix_selectchk(tp(S1,rdf:first,H), SkipTriples1, SkipTriples2),
  rdf_prefix_selectchk(tp(S1,rdf:rest,S2), SkipTriples2, SkipTriples3), !,
  linear_list_(S2, SkipTriples3, SkipTriples4, T).
linear_list_(_, SkipTriples, SkipTriples, []).

%! rdf_dcg_list_(+Terms:list(rdf_term), +Options:options)// is det.

rdf_dcg_list_(Terms, Options) -->
  "(",
  (   {Terms = [H|T]}
  ->  " ",
      rdf_dcg_node(H, Options),
      rdf_dcg_list_tail_(T, Options),
      " "
  ;   ""
  ),
  ")".

%! rdf_dcg_list_tail_(+Terms:list(rdf_term), +Options:options)// is det.

rdf_dcg_list_tail_([H|T], Options) --> !,
  " ",
  rdf_dcg_node(H, Options),
  rdf_dcg_list_tail_(T, Options).
rdf_dcg_list_tail_([], _) --> "".
