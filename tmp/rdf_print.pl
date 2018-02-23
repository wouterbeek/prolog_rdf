:- module(
  rdf_print,
  [
  % PP
    rdf_pp_quad_groups/1,      % +JoinedPairs:list(pair(atom,list(compound)))
    rdf_pp_quad_groups/2,      % +JoinedPairs:list(pair(atom,list(compound))), +Options
    rdf_pp_triple/1,           % +Triple
    rdf_pp_triple/2,           % +Triple, +Options
    rdf_pp_triple/3,           % +S, +P, +O
    rdf_pp_triple/4,           % +S, +P, +O, +Options
    rdf_pp_triples/1,          % +Triples
    rdf_pp_triples/2,          % +Triples, +Options
  % DCG
    rdf_dcg_bnode//1,          % +BNode
    rdf_dcg_iri//1,            % +Iri
    rdf_dcg_iri//2,            % +Iri, +Options
    rdf_dcg_language_tag//2,   % +LTag, +Options
    rdf_dcg_lexical_form//2,   % +Lex, +Options
    rdf_dcg_literal//1,        % +Lit
    rdf_dcg_literal//2,        % +Lit, +Options
    rdf_dcg_nonliteral//1,     % +S
    rdf_dcg_nonliteral//2,     % +S, +Options
    rdf_dcg_predicate//1,      % +P
    rdf_dcg_predicate//2,      % +P, +Options
    rdf_dcg_term//1,           % +Term
    rdf_dcg_term//2,           % +Term, +Options
    rdf_dcg_triple//3,         % +S, +P, +O
    rdf_dcg_triple//4,         % +S, +P, +O, +Options
    rdf_dcg_triples//1,        % +Triples
    rdf_dcg_triples//2,        % +Triples, +Options
    rdf_dcg_var//2             % +Var, +Options
  ]
).

/** <module> RDF print

| **Key**        | **Value**  | **Default** | **Description**                  |
|:---------------|:-----------|:-----------:|:---------------------------------|
| `indent`       | nonneg     | 0           |                                  |
| `iri_abbr`     | boolean    | `true`      | Whether IRIs are abbreviated     |
|                |            |             | based on the current prefixes.   |
| `max_iri_len`  | nonneg     | `∞`         | The maximum length of an IRI.    |
| `max_lit_len`  | nonneg     | `∞`         | The maximum length of a literal. |
| `prefix_map`   | list(pair( |             | A custom list of prefix/IRI      |
|                | atom))     |             | mappings that overrules and/or   |
|                |            |             | extends the prefix declarations. |
| `variable_map` | list(pair( |             | A list of variable/atom mappings |
|                | var,atom)) |             | that are required when variables |
|                |            |             | are printed.                     |

@author Wouter Beek
@version 2017/04-2017/12
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(date_time)).
:- use_module(library(dcg)).
:- use_module(library(dict)).
:- use_module(library(nlp/nlp_lang)).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_api)).
:- use_module(library(xml/xsd_dt)).

:- multifile
    rdf_dcg_literal_hook//2.

:- rdf_meta
   % PP
   rdf_pp_triple(t),
   rdf_pp_triple(t, +),
   rdf_pp_triple(r, r, o),
   rdf_pp_triple(r, r, o, +),
   % DCG
   rdf_dcg_iri(r, ?, ?),
   rdf_dcg_iri(r, +, ?, ?),
   rdf_dcg_literal(o, ?, ?),
   rdf_dcg_literal(o, +, ?, ?),
   rdf_dcg_nonliteral(o, ?, ?),
   rdf_dcg_nonliteral(o, +, ?, ?),
   rdf_dcg_term(o, ?, ?),
   rdf_dcg_term(o, +, ?, ?),
   rdf_dcg_triple(o, o, o, ?, ?),
   rdf_dcg_triple(o, o, o, +, ?, ?).





%! rdf_pp_quad_groups(+JoinedPairs:list(pair(atom,list(compound)))) is det.
%! rdf_pp_quad_groups(+JoinedPairs:list(pair(atom,list(compound))), +Options) is det.

rdf_pp_quad_groups(JoinedPairs) :-
  rdf_pp_quad_groups(JoinedPairs, _{}).


rdf_pp_quad_groups(JoinedPairs, Options) :-
  dict_get(out, Options, current_output, Out),
  dcg_with_output_to(Out, rdf_dcg_groups0(JoinedPairs, Options)).



%! rdf_pp_triple(+Triple) is det.
%! rdf_pp_triple(+Triple, +Options) is det.
%! rdf_pp_triple(+S, +P, +O) is det.
%! rdf_pp_triple(+S, +P, +O, +Options) is det.

rdf_pp_triple(rdf(S,P,O)) :-
  rdf_pp_triple(S, P, O, _{}).


rdf_pp_triple(rdf(S,P,O), Options) :-
  rdf_pp_triple(S, P, O, Options).


rdf_pp_triple(S, P, O) :-
  rdf_pp_triple(S, P, O, _{}).


rdf_pp_triple(S, P, O, Options) :-
  dict_get(out, Options, current_output, Out),
  dcg_with_output_to(Out, rdf_dcg_triple(S, P, O, Options)).



%! rdf_pp_triples(+Triples) is det.
%! rdf_pp_triples(+Triples, +Options) is det.

rdf_pp_triples(Triples) :-
  rdf_pp_triples(Triples, _{}).


rdf_pp_triples(Triples, Options) :-
  dict_get(out, Options, current_output, Out),
  dcg_with_output_to(Out, rdf_dcg_triples(Triples, Options)).





% DCG %

%! rdf_dcg_bnode(+BNode)// is det.

rdf_dcg_bnode(BNode) -->
  {rdf_is_bnode(BNode)}, !,
  atom(BNode).
rdf_dcg_bnode(BNode) -->
  {rdf_prefix_iri(bnode:Local, BNode)},
  "_:",
  atom(Local).



%! rdf_dcg_iri(+Iri)// is det.
%! rdf_dcg_iri(+Iri, +Options)// is det.

rdf_dcg_iri(Iri) -->
  rdf_dcg_iri(Iri, _{}).



%! rdf_dcg_language_tag(+LTag, +Options)// is det.

rdf_dcg_language_tag(LTag, Options) -->
  {dict_get(max_lit_len, Options, ∞, Len)},
  ellipsis(LTag, Len).



%! rdf_dcg_list(+Terms:list(rdf_term), +Options:list(compound))// is det.

rdf_dcg_list(L, Options) -->
  "(",
  (   {L = [H|T]}
  ->  " ",
      rdf_dcg_term(H, Options),
      rdf_dcg_list_items(T, Options),
      " "
  ;   ""
  ),
  ")".

rdf_dcg_list_items([], _) --> !.
rdf_dcg_list_items([H|T], Options) -->
  ", ",
  rdf_dcg_term(H, Options),
  rdf_dcg_list_items(T, Options).



%! rdf_dcg_literal(+Literal:compound)// is det.
%! rdf_dcg_literal(+Literal:compound, +Options:list(compound))// is det.

rdf_dcg_literal(Lit) -->
  rdf_dcg_literal(Lit, _{}).


% hook
rdf_dcg_literal(Lit, Options) -->
  rdf_dcg_literal_hook(Lit, Options), !.
% xsd:boolean
rdf_dcg_literal(literal(type(xsd:boolean,Lex)), Options) --> !,
  rdf_dcg_lexical_form(Lex, Options).
% xsd:decimal: before other numeric types
rdf_dcg_literal(literal(type(xsd:decimal,Lex)), _) --> !,
  {atom_phrase(decimalLexicalMap(N), Lex)},
  atom(N).
% xsd:byte,
% xsd:double
% xsd:float
% xsd:int
% xsd:integer
% xsd:long
% xsd:negativeInteger
% xsd:nonNegativeInteger
% xsd:nonPositiveInteger
% xsd:positiveInteger
% xsd:short
% xsd:unsignedByte
% xsd:unsignedInt
% xsd:unsignedLong
% xsd:unsignedShort
rdf_dcg_literal(literal(type(D,Lex)), _) -->
  {rdf11:xsd_numerical(D, _, Type)}, !,
  {xsd_number_string(N, Lex)},
  ({Type == integer} -> thousands(N) ; number(N)).
% xsd:date
% xsd:dateTime
% xsd:gDay
% xsd:gMonth
% xsd:gMonthDay
% xsd:gYear
% xsd:gYearMonth
% xsd:time
rdf_dcg_literal(literal(type(D,Lex)), Options) -->
  {
    rdf11:xsd_date_time_type(D), !,
    xsd_time_string(DateTime, D, Lex),
    xsd_date_time_to_dt(DateTime, D, DT),
    dt_label(DT, Label, Options)
  },
  atom(Label).
% xsd:string
rdf_dcg_literal(literal(type(xsd:string,Lex)), Options) --> !,
  rdf_dcg_lexical_form(Lex, Options).
% xsd:anyURI
rdf_dcg_literal(literal(type(xsd:anyURI,Uri)), _) --> !,
  atom(Uri).



%! rdf_dcg_predicate(+P)// is det.
%! rdf_dcg_predicate(+P, +Options)// is det.

rdf_dcg_predicate(P) -->
  rdf_dcg_predicate(P, _{}).


rdf_dcg_predicate(P, _) -->
  {
    ground(P),
    rdf_equal(rdf:type, P)
  }, !,
  "a".
rdf_dcg_predicate(P, Options) -->
  rdf_dcg_term(P, Options).



%! rdf_dcg_nonliteral(+S)// is det.
%! rdf_dcg_nonliteral(+S, +Options)// is det.

rdf_dcg_nonliteral(S) -->
  rdf_dcg_nonliteral(S, _{}).


rdf_dcg_nonliteral(S, Options) -->
  rdf_dcg_term(S, Options).



%! rdf_dcg_term(+Term)// is det.
%! rdf_dcg_term(+Term, +Options)// is det.

rdf_dcg_term(Term) -->
  rdf_dcg_term(Term, _{}).


%! rdf_dcg_complex_object(+Indent:nonneg, +Node:rdf_term,
%!                        +SkipTriples1:list(compound),
%!                        +SkipTriples2:list(compound),
%!                        +Options:list(compound))// is det.

rdf_dcg_complex_object(I1, Node, SkipTriples1, SkipTriples3, Options) -->
  {
    turtle_object(Node, SkipTriples1, SkipTriples2, Pairs),
    Pairs = [_|_], !,
    group_pairs_by_key(Pairs, Groups),
    I2 is I1 + 1
  },
  "[",
  rdf_dcg_predicates1(I2, Groups, SkipTriples2, SkipTriples3, Options),
  "]".
rdf_dcg_complex_object(_, RdfList, SkipTriples1, SkipTriples2, Options) -->
  {
    linear_list(RdfList, SkipTriples1, SkipTriples2, Terms),
    Terms = [_|_]
  }, !,
  rdf_dcg_list(Terms, Options).
rdf_dcg_complex_object(_, Term, SkipTriples, SkipTriples, Options) -->
  rdf_dcg_term(Term, Options).

linear_list(Node1, SkipTriples1, SkipTriples4, [H|T]) :-
  rdf_prefix_selectchk(rdf(Node1,rdf:first,H), SkipTriples1, SkipTriples2),
  rdf_prefix_selectchk(rdf(Node1,rdf:rest,Node2), SkipTriples2, SkipTriples3), !,
  linear_list(Node2, SkipTriples3, SkipTriples4, T).
linear_list(_, SkipTriples, SkipTriples, []).

turtle_object(S, SkipTriples1, SkipTriples3, [P-O|T]) :-
  rdf_prefix_selectchk(rdf(S,P,O), SkipTriples1, SkipTriples2),
  turtle_object(S, SkipTriples2, SkipTriples3, T).
turtle_object(_, SkipTriples, SkipTriples, []).



%! rdf_dcg_triple(+S, +P, +O)// is det.
%! rdf_dcg_triple(+S, +P, +O, +Options)// is det.

rdf_dcg_triple(S, P, O) -->
  rdf_dcg_triple(S, P, O, _{}).


rdf_dcg_triple(S, P, O, Options) -->
  rdf_dcg_nonliteral(S, Options),
  " ",
  rdf_dcg_predicate(P, Options),
  " ",
  rdf_dcg_term(O, Options).



rdf_dcg_triples(Triples) -->
  rdf_dcg_triples(Triples, _{}).


rdf_dcg_triples(Triples, Options) -->
  rdf_dcg_groups0([_NoGraph-Triples], Options).

rdf_dcg_groups0([], _) --> !, [].
rdf_dcg_groups0([G-Triples|Groups], Options) -->
  {dict_get(indent, Options, 0, I1)},
  (   {var(G)}
  ->  {I2 = I1}
  ;   dcg_tab0(I1),
      rdf_dcg_iri(G, Options),
      " {\n",
      {I2 = I1 + 1}
  ),
  rdf_dcg_triples0(I2, Triples, Options),
  ({var(G)} -> "" ; "}\n"),
  rdf_dcg_groups0(Groups, Options).

rdf_dcg_triples0(I, Triples, Options) -->
  {
    partition(is_skip_triple, Triples, SkipTriples, NonSkipTriples),
    aggregate_all(set(S-po(P,O)), member(rdf(S,P,O), NonSkipTriples), SortedPairs),
    group_pairs_by_key(SortedPairs, Groups)
  },
  rdf_dcg_subjects0(I, Groups, SkipTriples, Options).

is_skip_triple(rdf(S,_,_)) :-
  rdf_is_bnode(S).
is_skip_triple(rdf(_,P,_)) :-
  rdf_prefix_memberchk(P, [rdf:first,rdf:rest]).
is_skip_triple(rdf(_,_,O)) :-
  rdf_is_bnode(O).

rdf_dcg_subjects0(_, [], _, _) --> !, [].
rdf_dcg_subjects0(I1, [S-POs|Groups1], SkipTriples1, Options) -->
  dcg_tab0(I1),
  rdf_dcg_nonliteral(S, Options),
  {
    aggregate_all(set(P-O), member(po(P,O), POs), SortedPairs),
    group_pairs_by_key(SortedPairs, Groups2),
    I2 is I1 + 1
  },
  rdf_dcg_predicates0(I2, Groups2, SkipTriples1, SkipTriples2, Options),
  ({dict_get(newline, Options, false)} -> "" ; nl),
  rdf_dcg_subjects0(I1, Groups1, SkipTriples2, Options).

rdf_dcg_predicates0(I, Groups, SkipTriples1, SkipTriples2, Options) -->
  rdf_dcg_predicates1(I, Groups, SkipTriples1, SkipTriples2, Options),
  " .".

% There is exactly one predicate.  Emit it on the same line.
rdf_dcg_predicates1(I, [P-Os], SkipTriples1, SkipTriples2, Options) --> !,
  " ",
  rdf_dcg_predicate(P, Options),
  rdf_dcg_objects1(I, Os, SkipTriples1, SkipTriples2, Options).
rdf_dcg_predicates1(I, Groups, SkipTriples1, SkipTriples2, Options) -->
  rdf_dcg_predicates2(I, Groups, SkipTriples1, SkipTriples2, Options).

rdf_dcg_predicates2(_, [], SkipTriples, SkipTriples, _) --> !, [].
rdf_dcg_predicates2(I1, [P-Os|Groups], SkipTriples1, SkipTriples3, Options) -->
  nl,
  dcg_tab0(I1),
  rdf_dcg_predicate(P, Options),
  {I2 is I1 + 1},
  rdf_dcg_objects1(I2, Os, SkipTriples1, SkipTriples2, Options),
  " ",
  ({Groups == []} -> "." ; ";"),
  rdf_dcg_predicates2(I1, Groups, SkipTriples2, SkipTriples3, Options).

% There is exactly one object.  Emit it on the same line.
rdf_dcg_objects1(I, [O], SkipTriples1, SkipTriples2, Options) --> !,
  " ",
  rdf_dcg_complex_object(I, O, SkipTriples1, SkipTriples2, Options).
rdf_dcg_objects1(I, Os, SkipTriples1, SkipTriples2, Options) -->
  rdf_dcg_objects2(I, Os, SkipTriples1, SkipTriples2, Options).

rdf_dcg_objects2(_, [], SkipTriples, SkipTriples, _) --> !, [].
rdf_dcg_objects2(I, [O|Os], SkipTriples1, SkipTriples3, Options) -->
  nl,
  dcg_tab0(I),
  rdf_dcg_complex_object(I, O, SkipTriples1, SkipTriples2, Options),
  ({Os == []} -> "" ; " ,"),
  rdf_dcg_objects2(I, Os, SkipTriples2, SkipTriples3, Options).





% HELPERS %

%! dcg_tab0(+N:nonneg)// is det.

dcg_tab0(0) --> !, "".
dcg_tab0(N1) -->
  "  ",
  {N2 is N1 - 1},
  dcg_tab0(N2).
