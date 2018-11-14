:- encoding(utf8).
:- module(
  rdf_print,
  [
    rdf_dcg_node//1,      % +Node
    rdf_dcg_node//2,      % +Node, +Options
    rdf_dcg_pp//1,        % +PP
    rdf_dcg_pp//2,        % +PP, +Options
    rdf_dcg_predicate//1, % +Predicate
    rdf_dcg_predicate//2, % +Predicate, +Options
    rdf_dcg_qp//4,        % ?S, ?P, ?O, ?G
    rdf_dcg_qp//5,        % ?S, ?P, ?O, ?G, +Options
    rdf_dcg_tp//3,        % ?S, ?P, ?O
    rdf_dcg_tp//4,        % ?S, ?P, ?O, +Options
    rdf_dcg_tps//1,       % +TPs
    rdf_dcg_tps//2        % +TPs, +Options
  ]
).

/** <module> RDF printing

| **Key**        | **Value**            | **Default** | **Description**                    |
|:---------------|:---------------------|:-----------:|:-----------------------------------|
| `indent`       | nonneg               | 0           |                                    |
| `iri_abbr`     | boolean              | `true`      | Whether IRIs are abbreviated       |
|                |                      |             | based on the current prefixes.     |
| `'rdfs:label'` | compound             |             | Use `rdfs:label` from the given    |
|                |                      |             | backend.                           |
| `max_iri_len`  | nonneg               | `∞`         | The maximum length of an IRI.      |
| `max_lit_len`  | nonneg               | `∞`         | The maximum length of a literal.   |
| `prefix_map`   | list(pair(atom))     |             | A custom list of prefix/IRI        |
|                |                      |             | mappings that overrules and/or     |
|                |                      |             | extends the prefix declarations.   |
| `pp`           | boolean              | `false`     | Whether non-Turtle pretty printing |
|                |                      |             | should be applied.                 |
| `variable_map` | list(pair(var,atom)) |             | A list of variable/atom mappings   |
|                |                      |             | that are required when variables   |
|                |                      |             | are printed.                       |

---

@author Wouter Beek
@version 2016-2018
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(clpfd)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(solution_sequences)).
:- use_module(library(yall)).

:- use_module(library(dcg)).
:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dict)).
:- use_module(library(semweb/rdf_api)).
:- use_module(library(semweb/rdf_prefix)).
:- use_module(library(semweb/rdf_term)).

:- rdf_meta
   rdf_dcg_node(o, ?, ?),
   rdf_dcg_node(o, +, ?, ?),
   rdf_dcg_pp(t, ?, ?),
   rdf_dcg_pp(t, +, ?, ?),
   rdf_dcg_predicate(r, ?, ?),
   rdf_dcg_predicate(r, +, ?, ?),
   rdf_dcg_qp(r, r, o, r, ?, ?),
   rdf_dcg_qp(r, r, o, r, +, ?, ?),
   rdf_dcg_tp(r, r, o, ?, ?),
   rdf_dcg_tp(r, r, o, +, ?, ?),
   rdf_dcg_tps(t, ?, ?),
   rdf_dcg_tps(t, +, ?, ?).





%! rdf_dcg_node(+Node:rdf_node)// is det.
%! rdf_dcg_node(+Node:rdf_node, +Options:dict)// is det.

rdf_dcg_node(Node) -->
  rdf_dcg_node(Node, options{}).


rdf_dcg_node(Node, Options) -->
  rdf_dcg_term_(Node, Options).



%! rdf_dcg_pp(+PP:list(rdf_predicate))// is det.
%! rdf_dcg_pp(+PP:list(rdf_predicate), +Options:dict)// is det.

rdf_dcg_pp(PP) -->
  rdf_dcg_pp(PP, options{}).


rdf_dcg_pp([P], Options) --> !,
  rdf_dcg_predicate(P, Options).
rdf_dcg_pp(PP, Options) -->
  *&({Options}/[P]>>rdf_dcg_predicate(P, "/", Options), PP).



%! rdf_dcg_predicate(+P:rdf_predicate)// is det.
%! rdf_dcg_predicate(+P:rdf_predicate, +Options:dict)// is det.

rdf_dcg_predicate(P) -->
  rdf_dcg_predicate(P, options{}).


% @bug RDF prefix expansion does not work.
rdf_dcg_predicate(P, _) -->
  {rdf_equal(P, rdf:type)}, !,
  "a".
rdf_dcg_predicate(P, Options) -->
  rdf_dcg_term_(P, Options).



%! rdf_dcg_qp(?S:rdf_subject, ?P:rdf_predicate, ?O:rdf_object, ?G:rdf_graph)// is det.
%! rdf_dcg_qp(?S"rdf_subject, ?P:rdf_predicate, ?O:rdf_object, ?G:rdf_graph, +Options:dict)// is det.

rdf_dcg_qp(S, P, O, G) -->
  rdf_dcg_qp(S, P, O, G, options{}).


rdf_dcg_qp(S, P, O, G, Options) -->
  rdf_dcg_tp(S, P, O, Options),
  " @",
  rdf_dcg_term_(G, Options).



%! rdf_dcg_term_(+Term:rdf_term)// is det.
%! rdf_dcg_term_(+Term:rdf_term, +Options:dict)// is det.

rdf_dcg_term_(Term) -->
  rdf_dcg_term_(Term, options{}).


% rdfs:label
rdf_dcg_term_(Term, Options) -->
  {
    dict_get('rdfs:label', Options, B),
    tp_chk(B, Term, rdfs:label, Literal)
  }, !,
  rdf_dcg_term_(Literal, Options).
% language-tagged string
rdf_dcg_term_(literal(lang(LTag,Lex)), Options) --> !,
  rdf_dcg_lexical_form_(Lex, Options),
  "@",
  atom(LTag).
% typed literal
rdf_dcg_term_(literal(type(D,Lex)), Options) --> !,
  (   {rdf_equal(D, xsd:boolean)}
  ->  {rdf_canonical_lexical_form(xsd:boolean, Lex, CanonicalLex)},
      (   {dict_get(pp, Options, true)}
      ->  dcg_bool(CanonicalLex)
      ;   atom(CanonicalLex)
      )
  ;   % Do not show the datatype IRI for ‘xsd:decimal’, ‘xsd:float’,
      % and ‘xsd:integer’.
      {rdf_prefix_memberchk(D, [xsd:decimal,xsd:float,xsd:integer])}
  ->  {rdf_canonical_lexical_form(D, Lex, CanonicalLex)},
      atom(CanonicalLex)
  ;   % Do not show the datatype IRI for ‘xsd:string’.
      {rdf_equal(D, xsd:string)}
  ->  "\"", atom(Lex), "\""
  ;   % Typed literal without special treatment: lexical form +
      % datatype IRI.
      rdf_dcg_lexical_form_(Lex, Options),
      "^^",
      rdf_dcg_term_(D, Options)
  ).
% IRI
rdf_dcg_term_(Iri, Options) -->
  {rdf_is_iri(Iri)}, !,
  (   {well_known_iri(Segments, Iri)}
  ->  % Blank node notation for well-known IRIs.
      {atomic_list_concat(Segments, Local)},
      "_:",
      atom(Local)
      % Use custom symbols for some recurring IRIs.
  ;   {
        dict_get(pp, Options, true),
        rdf_equal(Iri, owl:sameAs)
      }
  ->  "≡"
  ;   {
        dict_get(pp, Options, true),
        rdf_equal(Iri, rdf:nil)
      }
  ->  "∅"
  ;   {
        dict_get(pp, Options, true),
        rdf_equal(Iri, rdf:type)
      }
  ->  "∈"
  ;   {
        dict_get(pp, Options, true),
        rdf_equal(Iri, rdfs:subClassOf)
      }
  ->  "⊆"
  ;   {
        dict_get(iri_abbr, Options, true, true),
        (   dict_get(prefix_map, Options, Prefix2Alias)
        ->  % Abbreviated based on the prefix map specified in options.
            (   gen_assoc(Prefix, Prefix2Alias, Alias),
                atom_prefix(Iri, Prefix)
            ->  atom_concat(Prefix, Local, Iri)
            )
        ;   % Abbreviated based on the global prefix declarations.
            rdf_prefix_iri(Alias:Local, Iri)
        )
      }
  ->  {
        atom_length(Alias, AliasLength),
        Minus #= AliasLength + 1,
        dict_get(max_iri_len, Options, ∞, Length),
        (   Length == '∞'
        ->  Max = '∞'
        ;   Length  =< Minus
        ->  Max = Length
        ;   Max = Length - Minus
        )
      },
      atom(Alias),
      ":",
      ({Max == ∞} -> atom(Local) ; ellipsis(Local, Max))
  ;   {dict_get(max_iri_len, Options, ∞, Length)},
      "<",
      ({Length == ∞} -> atom(Iri) ; ellipsis(Iri, Length)),
      ">"
  ).
% blank node
rdf_dcg_term_(BNode, _) -->
  {rdf_is_bnode(BNode)}, !,
  atom(BNode).

rdf_dcg_lexical_form_(Lex, Options) -->
  {dict_get(max_lit_len, Options, ∞, Length)},
  "\"",
  ({Length == ∞} -> atom(Lex) ; ellipsis(Lex, Length)),
  "\"".



%! rdf_dcg_tp(?S:rdf_subject, ?P:rdf_predicate, ?O:rdf_object)// is det.
%! rdf_dcg_tp(?S:rdf_subject, ?P:rdf_predicate, ?O:rdf_object, +Options:dict)// is det.

rdf_dcg_tp(S, P, O) -->
  rdf_dcg_tp(S, P, O, options{}).


rdf_dcg_tp(S, P, O, Options) -->
  "〈",
  rdf_dcg_node(S, Options),
  ", ",
  rdf_dcg_predicate(P, Options),
  ", ",
  rdf_dcg_node(O, Options),
  "〉".



%! rdf_dcg_tps(+TPs:list(tp))// is det.
%! rdf_dcg_tps(+TPs:list(tp), +Options:dict)// is det.
%
% Prints the given TPs, using the abbreviations defined in Turtle
% 1.1.
%
% @see https://www.w3.org/TR/turtle/

rdf_dcg_tps(TPs) -->
  rdf_dcg_tps(TPs, options{}).


rdf_dcg_tps(TPs, Options) -->
  rdf_dcg_groups0([_NoGraph-TPs], Options).

rdf_dcg_groups0([], _) --> !, [].
rdf_dcg_groups0([G-TPs|Groups], Options) -->
  {dict_get(indent, Options, 0, I1)},
  (   {var(G)}
  ->  {I2 = I1}
  ;   tab(I1),
      rdf_dcg_term_(G, Options),
      " {\n",
      {I2 = I1 + 4}
  ),
  rdf_dcg_tps0(I2, TPs, Options),
  ({var(G)} -> "" ; "}\n"),
  rdf_dcg_groups0(Groups, Options).

rdf_dcg_tps0(I, TPs, Options) -->
  {
    partition(is_skip_tp_, TPs, SkipTPs, NonSkipTPs),
    tps_to_groups0(NonSkipTPs, Groups)
  },
  rdf_dcg_subjects0(I, Groups, SkipTPs, Options).

%! is_skip_tp_(+TP:tp) is semidet.
%
% Succeeds if TP should be skipped for the purposes of printing.

% blank node subject term
is_skip_tp_(tp(S,_,_)) :-
  rdf_is_bnode(S).
% RDF list
is_skip_tp_(tp(_,P,_)) :-
  rdf_prefix_memberchk(P, [rdf:first,rdf:rest]).
% blank node object term
is_skip_tp_(tp(_,_,O)) :-
  rdf_is_bnode(O).

%! tps_to_groups0(+TPs:list(tp), -Groups:ordset(pair(rdf_node,ordset(pair(rdf_predicate,ordset(rdf_node)))))) is det.

tps_to_groups0(TPs, Groups) :-
  aggregate_all(
    set(S-SGroups),
    (
      distinct(S, member(tp(S,_,_), TPs)),
      tps_to_groups1(S, TPs, SGroups)
    ),
    Groups
  ).

tps_to_groups1(S, TPs, SGroups) :-
  aggregate_all(
    set(P-Os),
    (
      distinct(P, member(tp(S,P,_), TPs)),
      tps_to_groups2(S, P, TPs, Os)
    ),
    SGroups
  ).

tps_to_groups2(S, P, TPs, Os) :-
  aggregate_all(set(O), member(tp(S,P,O), TPs), Os).

%! rdf_dcg_subjects0(+Indent:nonneg, +Groups, +SkipTPs:list(tp), +Options:dict)// is det.

rdf_dcg_subjects0(_, [], _, _) --> !, [].
rdf_dcg_subjects0(I1, [S-SGroups|Groups], SkipTPs1, Options) -->
  tab(I1),
  rdf_dcg_node(S, Options),
  {I2 is I1 + 4},
  rdf_dcg_predicates1(I2, SGroups, SkipTPs1, SkipTPs2, Options),
  ({dict_get(newline, Options, false)} -> "" ; nl),
  rdf_dcg_subjects0(I1, Groups, SkipTPs2, Options).

% There is exactly one predicate.  Emit it on the same line.
rdf_dcg_predicates1(I, [P-Os], SkipTPs1, SkipTPs2, Options) --> !,
  " ",
  rdf_dcg_predicate(P, Options),
  rdf_dcg_objects1(I, Os, SkipTPs1, SkipTPs2, Options).
rdf_dcg_predicates1(I, Groups, SkipTPs1, SkipTPs2, Options) -->
  rdf_dcg_predicates2(I, Groups, SkipTPs1, SkipTPs2, Options).

rdf_dcg_predicates2(_, [], SkipTPs, SkipTPs, _) --> !, [].
rdf_dcg_predicates2(I1, [P-Os|Groups], SkipTPs1, SkipTPs3, Options) -->
  nl,
  tab(I1),
  rdf_dcg_predicate(P, Options),
  {I2 is I1 + 4},
  rdf_dcg_objects1(I2, Os, SkipTPs1, SkipTPs2, Options),
  ({Groups == []} -> "." ; ";"),
  rdf_dcg_predicates2(I1, Groups, SkipTPs2, SkipTPs3, Options).

% There is exactly one object.  Emit it on the same line.
rdf_dcg_objects1(I, [O], SkipTPs1, SkipTPs2, Options) --> !,
  " ",
  rdf_dcg_complex_object_(I, O, SkipTPs1, SkipTPs2, Options).
% There are multiple objects: group them together.
rdf_dcg_objects1(I, Os, SkipTPs1, SkipTPs2, Options) -->
  rdf_dcg_objects2(I, Os, SkipTPs1, SkipTPs2, Options).

rdf_dcg_objects2(_, [], SkipTPs, SkipTPs, _) --> !, [].
rdf_dcg_objects2(I, [O|Os], SkipTPs1, SkipTPs3, Options) -->
  nl,
  tab(I),
  rdf_dcg_complex_object_(I, O, SkipTPs1, SkipTPs2, Options),
  ({Os == []} -> "" ; ","),
  rdf_dcg_objects2(I, Os, SkipTPs2, SkipTPs3, Options).

%! rdf_dcg_complex_object_(+Indent:nonneg,
%!                        +Node:rdf_node,
%!                        +SkipTPs1:list(tp),
%!                        +SkipTPs2:list(tp),
%!                        +Options:dict)// is det.

rdf_dcg_complex_object_(I1, Node, SkipTPs1, SkipTPs3, Options) -->
  {
    turtle_object_(Node, SkipTPs1, SkipTPs2, Pairs),
    Pairs = [_|_], !,
    group_pairs_by_key(Pairs, Groups),
    I2 is I1 + 4
  },
  "[",
  rdf_dcg_predicates1(I2, Groups, SkipTPs2, SkipTPs3, Options),
  "]".
rdf_dcg_complex_object_(_, RdfList, SkipTPs1, SkipTPs2, Options) -->
  {
    linear_list_(RdfList, SkipTPs1, SkipTPs2, Terms),
    Terms = [_|_]
  }, !,
  rdf_dcg_list_(Terms, Options).
rdf_dcg_complex_object_(_, Term, SkipTPs, SkipTPs, Options) -->
  rdf_dcg_term_(Term, Options).

%! turtle_object_(+RdfList:rdf_node,
%!                +SkipTPs1:list(tp),
%!                -SkipTPs2:list(tp),
%!                -P_O_Pairs:list(pair(rdf_predicate,rdf_node))) is det.
turtle_object_(S, SkipTPs1, SkipTPs3, [P-O|T]) :- !,
  rdf_prefix_selectchk(tp(S,P,O), SkipTPs1, SkipTPs2),
  turtle_object_(S, SkipTPs2, SkipTPs3, T).
turtle_object_(_, SkipTPs, SkipTPs, []).

%! linear_list_(+RdfList:rdf_node,
%!              +SkipTPs1:list(tp),
%!              -SkipTPs2:list(tp),
%!              -Terms:list(rdf_term)) is det.

linear_list_(S1, SkipTPs1, SkipTPs4, [H|T]) :-
  rdf_prefix_selectchk(tp(S1,rdf:first,H), SkipTPs1, SkipTPs2),
  rdf_prefix_selectchk(tp(S1,rdf:rest,S2), SkipTPs2, SkipTPs3), !,
  linear_list_(S2, SkipTPs3, SkipTPs4, T).
linear_list_(_, SkipTPs, SkipTPs, []).

%! rdf_dcg_list_(+Terms:list(rdf_term), +Options:dict)// is det.

rdf_dcg_list_(Terms, Options) -->
  "(",
  (   {Terms = [H|T]}
  ->  " ",
      rdf_dcg_term_(H, Options),
      rdf_dcg_list_tail_(T, Options),
      " "
  ;   ""
  ),
  ")".

%! rdf_dcg_list_tail_(+Terms:list(rdf_term), +Options:dict)// is det.

rdf_dcg_list_tail_([H|T], Options) --> !,
  ", ",
  rdf_dcg_term_(H, Options),
  rdf_dcg_list_tail_(T, Options).
rdf_dcg_list_tail_([], _) --> "".
