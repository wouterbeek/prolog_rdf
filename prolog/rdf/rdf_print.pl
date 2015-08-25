:- module(
  rdf_print,
  [
    rdf_print_describe/2, % +Subject, ?Graph
    rdf_print_describe/3, % +Subject:rdf_term
                          % ?Graph:atom
                          % +Options:list(compound)
    rdf_print_graph/1, % ?Graph
    rdf_print_graph/2, % ?Graph:atom
                       % +Options:list(compound)
    rdf_print_quadruple/4, % ?Subject, ?Predicate, ?Object, ?Graph
    rdf_print_quadruple/5, % ?Subject:rdf_term
                           % ?Predicate:iri
                           % ?Object:rdf_term
                           % ?Graph:atom
                           % +Options:list(compound)
    rdf_print_quadruples/1, % +Quadruples:list(compoud)
    rdf_print_quadruples/2, % +Quadruples:list(compoud)
                            % +Options:list(compound)
    rdf_print_statement/1, % +Statement:compoud
    rdf_print_statement/2, % +Statement:compoud
                           % +Options:list(compound)
    rdf_print_statements/1, % +Statements:list(compoud)
    rdf_print_statements/2, % +Statements:list(compoud)
                            % +Options:list(compound)
    rdf_print_term/1, % +Term
    rdf_print_term/2, % +Term:rdf_term
                      % +Options:list(compound)
    rdf_print_term//1, % +Term
    rdf_print_term//2, % +Term:rdf_term
                       % +Options:list(compound)
    rdf_print_triple/4, % ?Subject, ?Predicate, ?Object, ?Graph
    rdf_print_triple/5, % ?Subject:rdf_term
                        % ?Predicate:iri
                        % ?Object:rdf_term
                        % ?Graph:atom
                        % +Options:list(compound)
    rdf_print_triples/1, % +Triples:list(compoud)
    rdf_print_triples/2 % +Triples:list(compoud)
                        % +Options:list(compound)
  ]
).

/** <module> RDF print

Easy printing of RDF data to the terminal.

---

@author Wouter Beek
@version 2015/07-2015/08
*/

:- use_module(library(atom_ext)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/dcg_bracketed)).
:- use_module(library(dcg/dcg_collection)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_logic)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(dcg/dcg_quoted)).
:- use_module(library(error)).
:- use_module(library(option)).
:- use_module(library(rdf/rdf_bnode_name)).
:- use_module(library(rdf/rdf_graph)).
:- use_module(library(rdf/rdf_list)).
:- use_module(library(rdf/rdf_read)).
:- use_module(library(semweb/rdf_db)).

:- set_prolog_flag(toplevel_print_anon, false).

:- rdf_meta(rdf_print_describe(o,?)).
:- rdf_meta(rdf_print_describe(o,?,+)).
:- rdf_meta(rdf_print_quadruple(o,r,o,?)).
:- rdf_meta(rdf_print_quadruple(o,r,o,?,+)).
:- rdf_meta(rdf_print_quadruples(t)).
:- rdf_meta(rdf_print_quadruples(t,+)).
:- rdf_meta(rdf_print_statement(t)).
:- rdf_meta(rdf_print_statement(t,+)).
:- rdf_meta(rdf_print_statements(t)).
:- rdf_meta(rdf_print_statements(t,+)).
:- rdf_meta(rdf_print_term(o)).
:- rdf_meta(rdf_print_term(o,+)).
:- rdf_meta(rdf_print_term(o,?,?)).
:- rdf_meta(rdf_print_term(o,+,?,?)).
:- rdf_meta(rdf_print_triple(o,r,o,?)).
:- rdf_meta(rdf_print_triple(o,r,o,?,+)).
:- rdf_meta(rdf_print_triples(t)).
:- rdf_meta(rdf_print_triples(t,+)).

:- predicate_options(rdf_print_describe/3, 3, [
     pass_to(rdf_print_statement/5, 5)
   ]).
:- predicate_options(rdf_print_bnode//2, 2, [
     abbr_list(+boolean)
   ]).
:- predicate_options(rdf_print_graph/2, 2, [
     pass_to(rdf_print_triple/5, 5)
   ]).
:- predicate_options(rdf_print_iri//2, 2, [
     abbr_iri(+boolean),
     abbr_list(+boolean),
     ellip_ln(+or([nonneg,oneof([inf])])),
     logic_sym(+boolean)
   ]).
:- predicate_options(rdf_print_lexical//2, 2, [
     ellip_lit(+or([nonneg,oneof([inf])]))
   ]).
:- predicate_options(rdf_print_literal//2, 2, [
     pass_to(rdf_print_lexical//2, 2)
   ]).
:- predicate_options(rdf_print_object//2, 2, [
     pass_to(rdf_print_term//2, 2)
   ]).
:- predicate_options(rdf_print_predicate//2, 2, [
     pass_to(rdf_print_term//2, 2)
   ]).
:- predicate_options(rdf_print_quadruple/5, 5, [
     pass_to(rdf_print_statement/5, 5)
   ]).
:- predicate_options(rdf_print_quadruples/2, 2, [
     pass_to(rdf_print_statement/5, 5)
   ]).
:- predicate_options(rdf_print_statement/2, 2, [
     pass_to(rdf_print_statement/5, 5)
   ]).
:- predicate_options(rdf_print_statement/5, 5, [
     indent(+nonneg),
     pass_to(rdf_print_statement//5, 5)
   ]).
:- predicate_options(rdf_print_statements/2, 2, [
     pass_to(rdf_print_statement/2, 2)
   ]).
:- predicate_options(rdf_print_statement//5, 5, [
     style(+oneof([tuple,turtle])),
     pass_to(rdf_print_object//2, 2),
     pass_to(rdf_print_predicate//2, 2),
     pass_to(rdf_print_subject//2, 2)
   ]).
:- predicate_options(rdf_print_subject//2, 2, [
     pass_to(rdf_print_term//2, 2)
   ]).
:- predicate_options(rdf_print_term/2, 2, [
     pass_to(rdf_print_list//2, 2),
     pass_to(rdf_print_term//2, 2)
   ]).
:- predicate_options(rdf_print_term//2, 2, [
     pass_to(rdf_print_bnode//2, 2),
     pass_to(rdf_print_iri//2, 2),
     pass_to(rdf_print_literal//2, 2)
   ]).
:- predicate_options(rdf_print_triple/5, 5, [
     pass_to(rdf_print_statement/5, 5)
   ]).
:- predicate_options(rdf_print_triples/2, 2, [
     pass_to(rdf_print_statement/5, 5)
   ]).





%! rdf_print_describe(+Subject:rdf_term, ?Graph:atom) is det.
% Wrapper around rdf_print_describe/3 with default options.

rdf_print_describe(S, G):-
  rdf_print_describe(S, G, []).

%! rdf_print_describe(
%!   +Subject:rdf_term,
%!   ?Graph:atom,
%!   +Options:list(compound)
%! ) is det.
% The following options are supported:
%   * abbr_iri(+boolean)
%   * abbr_list(+boolean)
%   * ellip_lit(+or([nonneg,oneof([inf])]))
%   * indent(+nonneg)
%   * logic_sym(+boolean)
%   * style(+oneof([tuple,turtle])

rdf_print_describe(S, G, Opts):-
  (   var(G)
  ->  % No graph is given: display quadruples.
      forall(rdf_print_quadruple(S, _, _, G, Opts), true)
  ;   % A graph is given: display triples.
      forall(rdf_print_triple(S, _, _, G, Opts), true)
  ).



%! rdf_print_graph(?Graph:atom) is det.

rdf_print_graph(G):-
  rdf_print_graph(G, []).

%! rdf_print_graph(?Graph:atom, +Options:list(compound)) is det.
% The following options are supported:
%   * abbr_iri(+boolean)
%   * abbr_list(+boolean)
%   * ellip_lit(+or([nonneg,oneof([inf])]))
%   * indent(+nonneg)
%   * logic_sym(+boolean)
%   * style(+oneof([tuple,turtle])
%
% @throws existence_error

rdf_print_graph(G, Opts):-
  var(G), !,
  rdf_print_quadruple(_, _, _, _, Opts),
  fail.
rdf_print_graph(G, Opts):-
  rdf_is_graph(G), !,
  rdf_print_triple(_, _, _, G, Opts),
  fail.
rdf_print_graph(G, _):-
  existence_error(rdf_graph, G).



%! rdf_print_quadruple(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   ?Graph:atom
%! ) is det.

rdf_print_quadruple(S, P, O, G):-
  rdf_print_quadruple(S, P, O, G, []).

%! rdf_print_quadruple(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   ?Graph:atom,
%!   +Options:list(compound)
%! ) is nondet.
% The following options are supported:
%   * abbr_iri(+boolean)
%   * abbr_list(+boolean)
%   * ellip_lit(+or([nonneg,oneof([inf])]))
%   * indent(+nonneg)
%   * logic_sym(+boolean)
%   * style(+oneof([tuple,turtle])

% Ground quadruples are printing without them having to be present
% in the RDF DB.
rdf_print_quadruple(S, P, O, G, Opts):-
  ground(rdf(S,P,O,G)), !,
  rdf_print_statement(S, P, O, G, Opts).
% Non-ground quadruples are non-deterministically matched
% against the RDF DB.
rdf_print_quadruple(S, P, O, G, Opts):-
  rdf2(S, P, O, G),
  rdf_print_statement(S, P, O, G, Opts).



%! rdf_print_quadruples(+Quadruples:list(compound)) is det.
% Wrapper around rdf_print_quadruples/2 with default options.

rdf_print_quadruples(Qs):-
  rdf_print_quadruples(Qs, []).

%! rdf_print_quadruples(
%!   +Quadruples:list(compound),
%!   +Options:list(compound)
%! ) is det.

rdf_print_quadruples(Qs, Opts0):-
  merge_options([abbr_list(false)], Opts0, Opts),
  forall(member(rdf(S,P,O,G), Qs), rdf_print_statement(S, P, O, G, Opts)).



%! rdf_print_statement(+Statement:compound) is det.
% Wrapper around rdf_print_statement/2 with default options.

rdf_print_statement(T):-
  rdf_print_statement(T, []).

%! rdf_print_statement(+Statement:compound, +Options:list(compound)) is det.

% Statement is a triple.
rdf_print_statement(rdf(S,P,O), Opts0):- !,
  merge_options([abbr_list(false)], Opts0, Opts),
  rdf_print_statement(S, P, O, _, Opts).
% Statement is a quadruple.
rdf_print_statement(rdf(S,P,O,G), Opts0):-
  merge_options([abbr_list(false)], Opts0, Opts),
  rdf_print_statement(S, P, O, G, Opts).



%! rdf_print_statement(
%!   +Subject:rdf_term,
%!   +Predicate:iri,
%!   +Object:rdf_term,
%!   +Graph:atom,
%!   +Options:list(compound)
%! ) is det.
% The following options are supported:
%   * abbr_iri(+boolean)
%   * abbr_list(+boolean)
%   * ellip_lit(+or([nonneg,oneof([inf])]))
%   * indent(+nonneg)
%   * style(+oneof([tuple,turtle])

rdf_print_statement(S, P, O, G, Opts):-
  option(indent(I), Opts, 0),
  string_phrase(rdf_print_statement(S, P, O, G, Opts), X),
  tab(I),
  writeln(X).



%! rdf_print_statements(+Statements:list(compound)) is det.
% Wrapper around rdf_print_statements/2 with default options.

rdf_print_statements(L):-
  rdf_print_statements(L, []).

%! rdf_print_statements(
%!   +Statements:list(compound),
%!   +Options:list(compound)
%! ) is det.

rdf_print_statements([], _):- !.
rdf_print_statements([H|T], Opts):-
  rdf_print_statement(H, Opts),
  rdf_print_statements(T, Opts).



%! rdf_print_term(+Term:rdf_term) is det.

rdf_print_term(T):-
  rdf_print_term(T, []).

%! rdf_print_term(+Term:rdf_term, +Options:list(compound)) is det.
% The following options are supported:
%   * abbr_iri(+boolean)
%   * abbr_list(+boolean)
%   * ellip_lit(+or([nonneg,oneof([inf])]))
%   * logic_sym(+boolean)

rdf_print_term(T, Opts):-
  string_phrase(rdf_print_term(T, Opts), X),
  writeln(X).



%! rdf_print_triple(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   ?Graph:atom
%! ) is nondet.

rdf_print_triple(S, P, O, G):-
  rdf_print_triple(S, P, O, G, []).

%! rdf_print_triple(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   ?Graph:atom,
%!   +Options:list(compound)
%! ) is nondet.
% The following options are supported:
%   * abbr_iri(+boolean)
%   * abbr_list(+boolean)
%   * ellip_lit(+or([nonneg,oneof([inf])]))
%   * indent(+nonneg)
%   * logic_sym(+boolean)
%   * style(+oneof([tuple,turtle])

% Ground triples are printing without them having to be present
% in the RDF DB.
rdf_print_triple(S, P, O, G, Opts):-
  ground(rdf(S,P,O)), !,
  rdf_print_statement(S, P, O, G, Opts).
% Non-ground triples are non-deterministically matched
% against the RDF DB.
rdf_print_triple(S, P, O, G, Opts):-
  rdf2(S, P, O, G),
  rdf_print_statement(S, P, O, _, Opts).



%! rdf_print_triples(+Triples:list(compound)) is det.
% Wrapper around rdf_print_triples/2 with default options.

rdf_print_triples(Ts):-
  rdf_print_triples(Ts, []).

%! rdf_print_triples(+Triples:list(compound), +Options:list(compound)) is det.

rdf_print_triples(Ts, Opts0):-
  merge_options([abbr_list(false)], Opts0, Opts),
  forall(member(rdf(S,P,O), Ts), rdf_print_statement(S, P, O, _, Opts)).





% GRAMMAR %

%! rdf_print_bnode(+BNode:bnode, +Options:list(compound))// is det.
% The following options are supported:
%   * abbr_list(+boolean)
%     Whether RDF lists are shown in Prolog list notation.
%     Default is `true` for statements in the RDF DB
%     but `false` for RDF compound terms (since in the latter case
%     we cannot check whether something is an RDF list or not).

rdf_print_bnode(B, Opts) -->
  {option(abbr_list(true), Opts)},
  rdf_print_list(B, Opts), !.
rdf_print_bnode(B, _) -->
  {rdf_bnode_name(B, BName)},
  atom(BName).



%! rdf_print_graph(+Graph:atom)// is det.

rdf_print_graph(G) -->
  atom(G).



%! rdf_print_iri(+Iri:atom, +Options:list(compound))// is det.
% The following options are supported:
%   * abbr_iri(+boolean)
%     Whether IRIs should be abbreviated w.r.t.
%     the currently registered RDF prefixes.
%     Default is `true`.
%   * abbr_list(+boolean)
%     Whether RDF lists are shown in Prolog list notation.
%     Default is `true` for statements in the RDF DB
%     but `false` for RDF compound terms (since in the latter case
%     we cannot check whether something is an RDF list or not).
%   * ellip_ln(+or([nonneg,oneof([inf])]))
%     Elipses IRI local names so that they are assued to be at most
%     the given number of characters in length.
%     This is only used when `abbr_iri=true`.
%     Default is `20` to ensure that every triple fits within
%     an 80 character wide terminal.
%   * logic_sym(+boolean)
%     Whether logic symbols should be used i.o. IRIs.
%     Default is `true`.

rdf_print_iri(Iri, Opts) -->
  {option(abbr_list(true), Opts)},
  rdf_print_list(Iri, Opts), !.
rdf_print_iri(Iri, Opts) -->
  {option(logic_sym(true), Opts, true)},
  rdf_print_iri_sym(Iri), !.
rdf_print_iri(Global, Opts) -->
  {
    \+ option(abbr_iri(false), Opts),
    rdf_global_id(Prefix:Local, Global), !,
    option(ellip_ln(N), Opts, 20),
    atom_truncate(Local, N, Local0)
  },
  atom(Prefix),
  ":",
  atom(Local0).
rdf_print_iri(Global, Opts) -->
  {option(style(turtle), Opts)}, !,
  bracketed(angular, atom(Global)).
rdf_print_iri(Global, _) -->
  atom(Global).

rdf_print_iri_sym(Iri) -->
  {rdf_global_id(owl:equivalentClass, Iri)}, !,
  equivalence.
rdf_print_iri_sym(Iri) -->
  {rdf_global_id(rdfs:subClassOf, Iri)}, !,
  subclass.
rdf_print_iri_sym(Iri) -->
  {rdf_global_id(rdf:type, Iri)}, !,
  set_membership.



%! rdf_print_lexical(+LexicalForm:atom, +Options:list(compound))// is det.
% The following options are supported:
%   * ellip_lit(+or([nonneg,oneof([inf])]))
%     Elipses lexical forms so that they are assured to be at most
%     the given number of characters in length.
%     Default is `20` to ensure that every triple fits within
%     an 80 character wide terminal.

rdf_print_lexical(Lex, Opts) -->
  {
    option(ellip_lit(N), Opts, 20),
    atom_truncate(Lex, N, Lex0)
  },
  quoted(atom(Lex0)).



%! rdf_print_list(+Term:rdf_term, +Options:list(compound))// is semidet.
% The following options are supported:
%   * abbr_list(+boolean)
%     Whether or not RDF lists are displayed using Prolog list notation.
%     Default is `true`.

rdf_print_list(L0, Opts) -->
  {
    rdf_is_list(L0),
    rdf_list_raw(L0, L)
  },
  list(rdf_print_term0(Opts), L).

rdf_print_term0(Opts, T) -->
  rdf_print_term(T, Opts).



%! rdf_print_literal(+Literal:comound, +Options:list(compound))// is det.
% The following options are supported:
%   * abbr_iri(+boolean)
%   * ellip_lit(+or([nonneg,oneof([inf])]))
%   * ellip_ln(+or([nonneg,oneof([inf])]))
%   * logic_sym(+boolean)

rdf_print_literal(literal(type(D,Lex)), Opts) -->
  (   {option(style(turtle), Opts)}
  ->  rdf_print_lexical(Lex, Opts),
      "^^",
      rdf_print_iri(D, Opts)
  ;   bracketed(langular, (
        rdf_print_iri(D, Opts),
        ", ",
        rdf_print_lexical(Lex, Opts)
      ))
  ).
rdf_print_literal(literal(lang(LangTag,Lex)), Opts) --> !,
  (   {option(style(turtle), Opts)}
  ->  rdf_print_lexical(Lex, Opts),
      "@",
      atom(LangTag)
  ;   bracketed(langular, (
        "rdf:langString, ",
        bracketed(langular, (
          atom(LangTag),
          ", ",
          rdf_print_lexical(Lex, Opts)
        ))
      ))
  ).
rdf_print_literal(literal(Lex), Opts) -->
  {rdf_global_id(xsd:string, D)},
  rdf_print_literal(literal(type(D,Lex)), Opts).



%! rdf_print_object(+Object:rdf_term, +Options:list(compound))// is det.
% The following options are supported:
%   * abbr_iri(+boolean)
%   * abbr_list(+boolean)
%   * ellip_lit(+or([nonneg,oneof([inf])]))
%   * ellip_ln(+or([nonneg,oneof([inf])]))

rdf_print_object(O, Opts) -->
  rdf_print_term(O, Opts).



%! rdf_print_predicate(+Predicate:iri, +Options:list(compound))// is det.
%   * abbr_iri(+boolean)
%   * ellip_ln(+or([nonneg,oneof([inf])]))
%   * logic_sym(+boolean)

rdf_print_predicate(P, Opts) -->
  rdf_print_iri(P, Opts).



%! rdf_print_statement(
%!   +Subject:rdf_term,
%!   +Predicate:iri,
%!   +Object:rdf_term,
%!   ?Graph:atom,
%!   +Options:list(compound)
%! )// is det.
%   * abbr_iri(+boolean)
%   * abbr_list(+boolean)
%   * ellip_lit(+or([nonneg,oneof([inf])]))
%   * ellip_ln(+or([nonneg,oneof([inf])]))
%   * logic_sym(+boolean)
%   * style(+oneof([tuple,turtle])
%     The style that is used for printing the statement.
%     Style `turtle` is appropriate for enumerating multiple triples.
%     Style `tuple` is appropriate for singular triples.
%     Default is `tuple`.

rdf_print_statement(S, P, O, G, Opts) -->
  {option(style(turtle), Opts)}, !,
  rdf_print_subject(S, Opts),
  " ",
  rdf_print_predicate(P, Opts),
  " ",
  rdf_print_object(O, Opts),
  ({ground(G)} -> " ", rdf_print_graph(G) ; ""),
  " .".
rdf_print_statement(S, P, O, G, Opts) -->
  bracketed(langular, rdf_print_statement0(S, P, O, Opts)),
  ({ground(G)} -> "@", rdf_print_graph(G) ; "").

rdf_print_statement0(S, P, O, Opts) -->
  rdf_print_subject(S, Opts),
  ", ",
  rdf_print_predicate(P, Opts),
  ", ",
  rdf_print_object(O, Opts).



%! rdf_print_subject(+Subject:rdf_term, +Options:list(compound))// is det.
% The following options are supported:
%   * abbr_iri(+boolean)
%   * abbr_list(+boolean)
%   * ellip_ln(+or([nonneg,oneof([inf])]))

rdf_print_subject(S, Opts) -->
  rdf_print_term(S, Opts).



%! rdf_print_term(+Term:rdf_term)// is det.

rdf_print_term(T) -->
  rdf_print_term(T, []).


%! rdf_print_term(+Term:rdf_term, +Options:list(compound))// is det.
% The following options are supported:
%   * abbr_iri(+boolean)
%   * abbr_list(+boolean)
%   * ellip_lit(+or([nonneg,oneof([inf])]))
%   * ellip_ln(+or([nonneg,oneof([inf])]))
%   * logic_sym(+boolean)

rdf_print_term(T, Opts) -->
  {rdf_is_literal(T)}, !,
  rdf_print_literal(T, Opts).
rdf_print_term(T, Opts) -->
  {rdf_is_bnode(T)}, !,
  rdf_print_bnode(T, Opts).
rdf_print_term(T, Opts) -->
  rdf_print_iri(T, Opts).
