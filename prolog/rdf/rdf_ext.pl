:- module(
  rdf_ext,
  [
    rdf_aggregate_all/3,    % +Template, :Goal, -Result
    rdf_assert/1,           % +Tuple
    rdf_assert/2,           % +Triple, +G
    rdf_expect_graph/1,     % ?G
    rdf_graph_to_triples/2, % ?G, -Triples
    rdf_image/2,            % +S, -Img
    rdf_is_ground_quad/1,   % @Term
    rdf_is_ground_triple/1, % @Term
    rdf_langstring/3,       % ?S, ?P, -Lit
    rdf_nextto/3,           % ?X, ?Y, ?RdfList
    rdf_pref_string/3,      % ?S, ?P, -Lit
    rdf_print/1,            % +Tuples
    rdf_print/4,            % ?S, ?P, ?O, ?G
    rdf_retractall/1,       % +Tuple
    rdf_snap/1,             % :Goal_0
    rdf_string/2,           % +Lit, -String
    rdf_triples/4,          % ?S, ?P. ?O, -Triples:ordset
    rdf_tuple/1,            % -Tuple
    rdf_unload_db/0,
    rdfs_instance0/2,       % ?I, ?C
    rdfs_label/2           % +S, -Lit
  ]
).
:- reexport(library(semweb/rdf11)).

/** <module> RDF extensions

@author Wouter Beek
@compat RDF 1.1
@version 2015/12-2016/03
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(closure)).
:- use_module(library(error)).
:- use_module(library(nlp/nlp_lang)).
:- use_module(library(print_ext)).
:- use_module(library(rdf/rdf_prefix), []). % Load RDF prefixes.
:- use_module(library(uuid)).
:- use_module(library(yall)).

:- meta_predicate
    rdf_aggregate_all(+, 0, -),
    rdf_snap(0).

:- rdf_meta
   rdf_aggregate_all(+, t, -),
   rdf_assert(t),
   rdf_assert(t, r),
   rdf_expect_graph(r),
   rdf_graph_to_triples(r, -),
   rdf_image(r, -),
   rdf_langstring(r, r, o),
   rdf_nextto(o, o, r),
   rdf_pref_string(r, r, o),
   rdf_pref_string(r, r, -, o),
   rdf_pref_string(r, r, -, -, o),
   rdf_print(r, r, o, r),
   rdf_retractall(t),
   rdf_string(r, -),
   rdf_triples(r, r, o, -),
   rdfs_instance0(o, r),
   rdfs_label(r, o).





%! rdf_aggregate_all(+Template, :Goal, -Result) is det.

rdf_aggregate_all(Template, Goal, Result) :-
  aggregate_all(Template, Goal, Result).



%! rdf_assert(+Tuple) is det.

rdf_assert(rdf(S,P,O)) :- !,
  rdf_assert(S, P, O).
rdf_assert(rdf(S,P,O,G)) :-
  rdf_assert(S, P, O, G).



%! rdf_assert(+Triple, +G) is det.

rdf_assert(rdf(S,P,O), G) :-
  rdf_assert(S, P, O, G).



%! rdf_expect_graph(+G) is semidet.
%! rdf_expect_graph(-G) is nondet.
% If Term is uninstantiated it is non-deterministically
% instantiated to existing RDF graphs.
% If Term is instantiated and does not denote an existing RDF graph
% this results in an exception.
%
% @throws existence_error

rdf_expect_graph(G) :-
  rdf_graph(G), !.
rdf_expect_graph(G) :-
  existence_error(rdf_graph, G).



% ! rdf_graph_to_triples(?G, -Triples) is det.

rdf_graph_to_triples(G, Triples) :-
  rdf_expect_graph(G),
  aggregate_all(set(rdf(S,P,O)), rdf(S, P, O, G), Triples).



%! rdf_image(+S, -Image:iri) is nondet.

rdf_image(S, V) :-
  rdf_has(S, dbo:thumbnail, V^^xsd:anyURI).
rdf_image(S, V) :-
  rdf_has(S, foaf:depiction, V^^xsd:anyURI).
rdf_image(S, V) :-
  rdf(S, _, V),
  rdfs_instance0(V, dcmit:'Image').



%! rdf_is_ground_quad(@Term) is semidet.
% Succeeds if the given triple is ground, i.e., contains no blank node.

rdf_is_ground_quad(rdf(S,P,O,_)) :-
  rdf_is_ground_triple(rdf(S,P,O)).



%! rdf_is_ground_triple(@Term) is semidet.
% Succeeds if the given triple is ground, i.e., contains no blank node.

rdf_is_ground_triple(rdf(S,_,O)) :-
  \+ rdf_is_bnode(S),
  \+ rdf_is_bnode(O).



%! rdf_langstring(?S, ?P, -Lit) is nondet.
% Matches RDF statements whose object term is a language-tagged string
% that mathes the given language priory list.
% Notice that results for each of the prioritized languages are given
% in arbitrary order.

rdf_langstring(S, P, Lit) :-
  current_lrange(LRange),
  rdf_langstring(S, P, LRange, Lit).

rdf_langstring(S, P, LRange, Lit) :-
  rdf_has(S, P, V@LTag),
  basic_filtering(LRange, LTag),
  Lit = V@LTag.



%! rdf_nextto(?X, ?Y, ?RdfList) is nondet.

rdf_nextto(X, Y, L) :-
  closure([X,Y]>>rdf_directly_nextto(X, Y, L), X, Y).

rdf_directly_nextto(X, Y, L) :-
  rdf_has(L, rdf:first, X),
  rdf_has(L, rdf:rest, T),
  rdf_has(T, rdf:first, Y).



%! rdf_pref_string(?S, ?P, -Lit) is nondet.
% Returns, in this exact order:
%   1. The language-tagged strings that match the given
%      language priority list; returning results for higher
%      priority language earlier.
%   2. The language-tagged strings that do not match the given
%      language priority list.
%   3. XSD strings.

rdf_pref_string(S, P, Lit) :-
  current_lrange(LRange),
  rdf_pref_string(S, P, LRange, Lit).

% Matching language-tagged strings.
rdf_pref_string(S, P, LRange, Lit) :-
  rdf_langstring(S, P, LRange, Lit).
% Non-matching language-tagged strings.
rdf_pref_string(S, P, LRange, Lit) :-
  rdf_has(S, P, V@LTag),
  % Avoid duplicates.
  \+ basic_filtering(LRange, LTag),
  Lit = V@LTag.
% Plain XSD strings.
rdf_pref_string(S, P, _, V^^xsd:string) :-
  rdf_has(S, P, V^^xsd:string).


%! rdf_print(+Tuples) is det.

rdf_print(Tuples) :-
  rdf_snap((
    maplist(rdf_assert, Tuples),
    rdf_print(_, _, _, _)
  )).


%! rdf_print(?S, ?P, ?O, ?G) is det.

rdf_print(S, P, O, G) :-
  aggregate_all(set(G), rdf_graph(G), Gs),
  maplist(rdf_print_for_graph(S, P, O), Gs).

rdf_print_bnode(B) :-
  write(B).

%! rdf_print_for_graph(?S, ?P, ?O, +G) is det.

rdf_print_for_graph(S, P, O, G) :-
  (   rdf_default_graph(G)
  ->  I = 0
  ;   rdf_print_graph(G),
      write(" {\n"),
      I = 1
  ),
  aggregate_all(set(S), rdf(S, P, O, G), Ss),
  rdf_print_subjects(I, Ss, P, O, G),
  (rdf_default_graph(G) -> true ; write("}\n")).

rdf_print_subjects(I, [H|T], P, O, G) :- !,
  rdf_print_for_subject(I, H, P, O, G),
  rdf_print_subjects(I, T, P, O, G).
rdf_print_subjects(_, [], _, _, _).

%! rdf_print_for_subject(+I, +S, ?P, ?O, +G) is det.

rdf_print_for_subject(I1, S, P, O, G) :-
  tab0(I1),
  rdf_print_subject(S),
  aggregate_all(set(P), rdf(S, P, O, G), Ps),
  (   Ps = [P0]
  ->  write(" "),
      rdf_print_for_predicate(S, P0, O, G),
      writeln(" .")
  ;   nl,
      I2 is I1 + 1,
      rdf_print_predicates(I2, S, Ps, O, G)
  ).

rdf_print_predicates(I, S, [H], O, G) :-
  tab0(I),
  rdf_print_for_predicate(S, H, O, G),
  writeln(" .").
rdf_print_predicates(I, S, [H|T], O, G) :-
  tab0(I),
  rdf_print_for_predicate(S, H, O, G),
  writeln(" ;"),
  rdf_print_predicates(I, S, T, O, G).

%! rdf_print_for_predicate(+S, +P, ?O, +G) is det.

rdf_print_for_predicate(S, P, O, G) :-
  rdf_print_predicate(P),
  write(" "),
  aggregate_all(set(O), rdf(S, P, O, G), Os),
  rdf_print_objects(Os).

rdf_print_graph(G) :-
  rdf_print_iri(G).

rdf_print_iri(Full) :-
  rdf_global_id(Alias:Local, Full), !,
  write(Alias),
  write(":"),
  write(Local).
rdf_print_iri(Full) :-
  write("<"),
  write(Full),
  write(">").

rdf_print_literal(V^^D) :-
  (   rdf_equal(xsd:boolean, D)
  ;   rdf_equal(xsd:string, D)
  ), !,
  rdf_lexical_form(V^^D, Lex^^D),
  turtle:turtle_write_quoted_string(current_output, Lex).
rdf_print_literal(V^^D) :-
  (   rdf_equal(xsd:integer, D)
  ;   rdf_equal(xsd:decimal, D)
  ;   rdf_equal(xsd:double, D)
  ), !,
  rdf_lexical_form(V^^D, Lex^^D),
  turtle:turtle_write_quoted_string(current_output, Lex).
rdf_print_literal(V^^D) :- !,
  rdf_lexical_form(V^^D, Lex^^D),
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

rdf_print_object(O) :-
  rdf_is_literal(O), !,
  rdf_print_literal(O).
rdf_print_object(O) :-
  rdf_print_subject(O).

rdf_print_objects([H]) :- !,
  rdf_print_object(H).
rdf_print_objects([H|T]) :-
  rdf_print_object(H),
  write(", "),
  rdf_print_objects(T).

rdf_print_predicate(P) :-
  rdf_print_iri(P).

rdf_print_subject(S) :-
  rdf_is_bnode(S), !,
  rdf_print_bnode(S).
rdf_print_subject(S) :-
  rdf_print_iri(S).

tab0(N1) :- N2 is N1 * 4, tab(N2).



%! rdf_retractall(+Tuple) is det.

rdf_retractall(rdf(S,P,O)) :- !,
  rdf_retractall(S, P, O).
rdf_retractall(rdf(S,P,O,G)) :-
  rdf_retractall(S, P, O, G).



%! rdf_snap(:Goal_0) .

rdf_snap(Goal_0) :-
  rdf_transaction(Goal_0, _, [snapshot(true)]).



%! rdf_string(+Lit, -String) is det.

rdf_string(V^^xsd:string, V) :- !.
rdf_string(V@_, V).



%! rdf_tuple(-Tuple) is det.

rdf_tuple(Tuple) :-
  rdf(S, P, O, G),
  (G == default -> Tuple = rdf(S,P,O) ; Tuple = rdf(S,P,O,G)).
  


%! rdf_triples(?S, ?P, ?O, -Triples) is det.

rdf_triples(S, P, O, Triples):-
  aggregate_all(set(rdf(S,P,O)), rdf(S, P, O), Triples).



%! rdf_unload_db is det.

rdf_unload_db :-
  rdf_graph(G), !,
  rdf_unload_graph(G),
  rdf_unload_db.
rdf_unload_db.



%! rdfs_instance0(?I, ?C) is nondet.

rdfs_instance0(I, D) :-
  nonvar(D), !,
  rdf_reachable(C, rdfs:subClassOf, D),
  rdf_has(I, rdf:type, C).
rdfs_instance0(I, D) :-
  rdf_has(I, rdf:type, C),
  rdf_reachable(C, rdfs:subClassOf, D).



%! rdfs_label(+S, -Lit) is nondet.

rdfs_label(S, Lit) :-
  rdf_pref_string(S, rdfs:label, Lit).





% HELPER %

%! basic_filtering(
%!   +LanguagePriorityList:list(atom),
%!   +LanguageTag:atom
%! ) is semidet.
% Succeeds if the LanguagePriorityList matches the LanguageTag according to
% the basic filtering algorithm described in RFC 4647,
% i.e., if the former is a case-insensitive prefix of the latter,
% while also treating the `*` sign as a wildcard.
%
% @compat RFC 4647

basic_filtering(Ranges, Tag):-
  % NONDET
  member(Range, Ranges),
  atomic_list_concat(Subtags1, -, Range),
  atomic_list_concat(Subtags2, -, Tag),
  basic_filtering0(Subtags1, Subtags2), !.

basic_filtering0(_, []).
basic_filtering0([H1|T1], [H2|T2]):-
  subtag_match(H1, H2),
  basic_filtering0(T1, T2).


%! subtag_match(+RangeSubtag:atom, +Subtag:atom) is semidet.
% Two subtags match if either they are the same when compared
% case-insensitively or the language range's subtag is the wildcard `*`

subtag_match(*, _):- !.
subtag_match(X1, X2):-
  downcase_atom(X1, X),
  downcase_atom(X2, X).
