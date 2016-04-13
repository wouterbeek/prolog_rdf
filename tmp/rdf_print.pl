:- module(
  rdf_print,
  [
    rdf_print_deref/1,      % +Iri
    rdf_print_deref/2,      % +Iri, +Opts
    rdf_print_descr/1,      % +S
    rdf_print_descr/2,      % +S, +Opts
    rdf_print_descr/3,      % +S, ?G, +Opts
    rdf_print_graph/1,      % ?G
    rdf_print_graph/2,      % ?G, +Opts
    rdf_print_graph//1,     % ?G
    rdf_print_graph//2,     % ?G, +Opts
    rdf_print_graphs/0,
    rdf_print_graphs/1      % +Opts
    rdf_print_literal//1,   % +Lit
    rdf_print_literal//2,   % +Lit, +Opts
    rdf_print_object//1,    % +O
    rdf_print_object//2,    % +O, +Opts
    rdf_print_predicate//1, % +P
    rdf_print_predicate//2, % +P, +Opts
    rdf_print_quad/1,       % +Quad
    rdf_print_quad/2,       % +Quad, +Opts
    rdf_print_quad/3,       % ?S, ?P, ?O
    rdf_print_quad/4,       % ?S, ?P, ?O, ?G
    rdf_print_quad/5,       % ?S, ?P, ?O, ?G, +Opts
    rdf_print_quads/1,      % +Quads
    rdf_print_quads/2,      % +Quads, +Opts
    rdf_print_subject//1,   % +S
    rdf_print_subject//2,   % +S, +Opts
    rdf_print_term/1,       % +T
    rdf_print_term/2,       % +T, +Opts
    rdf_print_term//1,      % +T
    rdf_print_term//2       % +T, +Opts
    rdf_print_tuple/1,      % +Tuple
    rdf_print_tuple/2,      % +Tuple, +Opts
    rdf_print_tuple/5,      % +S, +P, +O, ?G, +Opts
    rdf_print_tuple//4,     % +S, +P, +O, ?G
    rdf_print_tuple//5,     % +S, +P, +O, ?G, +Opts
    rdf_print_tuples/1,     % +Tuples
    rdf_print_tuples/2,     % +Tuples, +Opts
    rdf_print_triple/1,     % +Triple
    rdf_print_triple/2,     % +Triple, +Opts
    rdf_print_triple/3,     % ?S, ?P, ?O
    rdf_print_triple/4,     % ?S, ?P, ?O, ?G
    rdf_print_triple/5,     % ?S, ?P, ?O, ?G, +Opts
    rdf_print_triples/1,    % +Triples
    rdf_print_triples/2     % +Triples, +Opts
  ]
).

/** <module> RDF: Printing

Printing of RDF statements to a text-based output stream.

@author Wouter Beek
@version 2015/07-2016/03
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(dcg/dcg_collection)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_pl)).
:- use_module(library(dcg/dcg_table)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(pair_ext)).
:- use_module(library(rdf/rdf_deref)).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(rdf/rdf_stats)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(string_ext)).
:- use_module(library(typecheck)).
:- use_module(library(yall)).

:- set_prolog_flag(toplevel_print_anon, false).

:- rdf_meta
   rdf_print_deref(r),
   rdf_print_deref(r, +),
   rdf_print_descr(r),
   rdf_print_descr(r, +),
   rdf_print_descr(r, r, +),
   rdf_print_graph(r),
   rdf_print_graph(r, +),
   rdf_print_graph(r, ?, ?),
   rdf_print_graph(r, +, ?, ?),
   rdf_print_literal(o, ?, ?),
   rdf_print_literal(o, +, ?, ?),
   rdf_print_object(o, +, ?, ?),
   rdf_print_predicate(r, +, ?, ?),
   rdf_print_quad(t),
   rdf_print_quad(t, +),
   rdf_print_quad(r, r, o),
   rdf_print_quad(r, r, o, ?),
   rdf_print_quad(r, r, o, ?, +),
   rdf_print_quads(t),
   rdf_print_quads(t, +),
   rdf_print_subject(r, +, ?, ?),
   rdf_print_term(o),
   rdf_print_term(o, +),
   rdf_print_term(o, ?, ?),
   rdf_print_term(o, +, ?, ?),
   rdf_print_tuple(t),
   rdf_print_tuple(t, +),
   rdf_print_tuple(t, r, r, ?, ?, ?),
   rdf_print_tuple(t, r, r, ?, +, ?, ?),
   rdf_print_tuples(t),
   rdf_print_tuples(t, +),
   rdf_print_triple(t),
   rdf_print_triple(t, +),
   rdf_print_triple(r, r, o),
   rdf_print_triple(r, r, o, ?),
   rdf_print_triple(r, r, o, ?, +),
   rdf_print_triples(t),
   rdf_print_triples(t, +),
   rdf_symbol_iri(r).

:- predicate_options(rdf_print_deref/2, 2, [
     pass_to(rdf_print_triple/5, 5)
   ]).
:- predicate_options(rdf_print_descr/2, 2, [
     pass_to(rdf_print_descr/3, 3)
   ]).
:- predicate_options(rdf_print_descr/3, 3, [
     pass_to(rdf_print_quad/5, 5)
   ]).
:- predicate_options(rdf_print_graph/2, 2, [
     pass_to(rdf_print_quad/5, 5),
     pass_to(rdf_print_triple/5, 5)
   ]).
:- predicate_options(rdf_print_graphs/1, 1, [
     pass_to(dcg_table//2, 2)
   ]).
:- predicate_options(rdf_print_quad/2, 2, [
     pass_to(rdf_print_tuple/5, 5)
   ]).
:- predicate_options(rdf_print_quad/5, 5, [
     pass_to(rdf_print_tuple/5, 5)
   ]).
:- predicate_options(rdf_print_quads/2, 2, [
     pass_to(rdf_print_tuple/5, 5)
   ]).
:- predicate_options(rdf_print_tuple/2, 2, [
     pass_to(rdf_print_tuple/5, 5)
   ]).
:- predicate_options(rdf_print_tuple/5, 5, [
     indent(+nonneg),
     pass_to(rdf_print_tuple//5, 5)
   ]).
:- predicate_options(rdf_print_tuple//5, 5, [
     pass_to(rdf_print_graph_maybe//2, 2),
     pass_to(rdf_print_object//2, 2),
     pass_to(rdf_print_predicate//2, 2),
     pass_to(rdf_print_subject//2, 2)
   ]).
:- predicate_options(rdf_print_tuples/2, 2, [
     pass_to(rdf_print_tuple/2, 2)
   ]).
:- predicate_options(rdf_print_triple/2, 2, [
     pass_to(rdf_print_tuple/5, 5)
   ]).
:- predicate_options(rdf_print_triple/5, 5, [
     pass_to(rdf_print_tuple/5, 5)
   ]).
:- predicate_options(rdf_print_triples/2, 2, [
     pass_to(rdf_print_tuple/5, 5)
   ]).
:- predicate_options(rdf_print_iri//2, 2, [
     abbr_iri(+boolean),
     abbr_list(+boolean),
     ellip_ln(+or([nonneg,oneof([inf])])),
     label_iri(+boolean),
     lrange(+list(atom)),
     symbol_iri(+boolean),
     pass_to(rdf_print_list//2, 2)
   ]).
:- predicate_options(rdf_print_lexical//2, 2, [
     ellip_lit(+or([nonneg,oneof([inf])]))
   ]).
:- predicate_options(rdf_print_list//2, 2, [
     pass_to(rdf_print_term//2, 2)
   ]).
:- predicate_options(rdf_print_literal//2, 2, [
     pass_to(rdf_print_datatype//2, 2),
     pass_to(rdf_print_language_tag//2, 2),
     pass_to(rdf_print_lexical//2, 2)
   ]).
:- predicate_options(rdf_print_object//2, 2, [
     pass_to(rdf_print_term//2, 2)
   ]).
:- predicate_options(rdf_print_predicate//2, 2, [
     pass_to(rdf_print_iri//2, 2)
   ]).
:- predicate_options(rdf_print_subject//2, 2, [
     pass_to(rdf_print_term//2, 2)
   ]).
:- predicate_options(rdf_print_term/2, 2, [
     pass_to(rdf_print_term//2, 2)
   ]).





%! rdf_print_datatype(+D, +Opts)// is det.
% Options are passed to rdf_print_iri//2.

rdf_print_datatype(D, Opts) -->
  rdf_print_iri(D, Opts).



%! rdf_print_deref(+Iri) is det.
%! rdf_print_deref(+Iri, +Opts) is det.

rdf_print_deref(Iri) :-
  rdf_print_deref(Iri, []).
rdf_print_deref(Iri, Opts) :-
  rdf_deref(Iri, Tuples),
  rdf_print_tuples(Tuples, Opts).



%! rdf_print_descr(+S) is det.
%! rdf_print_descr(+S, +Opts) is det.
%! rdf_print_descr(+S, ?G, +Opts) is det.
% The following options are supported:
%   * abbr_iri(+boolean)
%   * abbr_list(+boolean)
%   * ellip_lit(+or([nonneg,oneof([inf])]))
%   * id_closure(+boolean)
%   * indent(+nonneg)
%   * label_iri(+boolean)
%   * symbol_iri(+boolean)

rdf_print_descr(S) :-
  rdf_print_descr(S, none, []).

rdf_print_descr(S, Opts) :-
  rdf_print_descr(S, none, Opts).

rdf_print_descr(S, G, Opts) :-
  (   G == none
  ->  forall(rdf_print_triple(S, _, _, _, Opts), true)
  ;   var(G)
  ->  % No graph is given: display quads.
      forall(rdf_print_quad(S, _, _, G, Opts), true)
  ;   % A graph is given: display triples.
      forall(rdf_print_triple(S, _, _, G, Opts), true)
  ).



%! rdf_print_graph(?G) is det.
%! rdf_print_graph(?G, +Opts) is det.
% The following options are supported:
%   * abbr_iri(+boolean)
%   * abbr_list(+boolean)
%   * ellip_lit(+or([nonneg,oneof([inf])]))
%   * id_closure(+boolean)
%   * indent(+nonneg)
%   * label_iri(+boolean)
%   * symbol_iri(+boolean)
%
% @throws existence_error

rdf_print_graph(G) :-
  rdf_print_graph(G, []).

rdf_print_graph(G, Opts) :-
  (   var(G)
  ->  rdf_print_quad(_, _, _, _, Opts),
      fail
  ;   rdf_graph(G)
  ->  rdf_print_triple(_, _, _, G, Opts),
      fail
  ;   existence_error(rdf_graph, G)
  ).
rdf_print_graph(_, _).



%! rdf_print_graph(?G)// is det.
%! rdf_print_graph(?G, +Opts)// is det.

rdf_print_graph(G) -->
  rdf_print_graph(G, []).

rdf_print_graph(G, Opts) -->
  "@",
  (   {
        rdf_global_id(Prefix:Local, G),
        \+ option(abbr_iri(false), Opts)
      }
  ->  {
        option(ellip_ln(N), Opts, 20),
        atom_truncate(Local, N, Local0)
      },
      atom(Prefix),
      ":",
      atom(Local0)
  ;   atom(G)
  ).



%! rdf_print_iri(+Iri, +Opts)// is det.
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
%   * label_iri(+boolean)
%   * lrange(+list(atom))
%   * symbol_iri(+boolean)
%     Whether logic symbols should be used i.o. IRIs.
%     Default is `true`.

rdf_print_iri(Iri, Opts) -->
  {option(abbr_list(true), Opts)},
  rdf_print_list(Iri, Opts), !.
rdf_print_iri(Iri, Opts) -->
  {option(symbol_iri(true), Opts)},
  rdf_symbol_iri(Iri), !.
rdf_print_iri(Global, Opts) -->
  {
    option(label_iri(true), Opts),
    rdfs_label(Global, Lbl), !
  },
  atom(Lbl).
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
rdf_print_iri(Global, _) -->
  {is_http_iri(Global)},
  atom(Global).



%! rdf_print_language_tag(+LTag, +Opts)// is det.

rdf_print_language_tag(LTag, _) -->
  atom(LTag).



%! rdf_print_lexical(+Lex, +Opts)// is det.
% The following options are supported:
%   * ellip_lit(+or([nonneg,oneof([inf])]))
%     Elipses lexical forms so that they are assured to be at most
%     the given number of characters in length.
%     Default is `20` to ensure that every triple fits within
%     an 80 character wide terminal.

rdf_print_lexical(Lex, Opts) -->
  {
    option(ellip_lit(N), Opts, 20),
    string_truncate(Lex, N, Lex0)
  },
  "\"", atom(Lex0), "\"".



%! rdf_print_list(+RdfList, +Opts)// is semidet.
% The following options are supported:
%   * abbr_iri(+boolean)
%   * abbr_list(+boolean)
%     Whether or not RDF lists are displayed using Prolog list notation.
%     Default is `true`.
%   * ellip_lit(+or([nonneg,oneof([inf])]))
%   * ellip_ln(+or([nonneg,oneof([inf])]))
%   * label_iri(+boolean)
%   * lrange(+list(atom))
%   * symbol_iri(+boolean)

rdf_print_list(L1, Opts) -->
  {rdf_list(L1, L2)},
  list([T]>>rdf_print_term(T, Opts), L2).



%! rdf_print_literal(+Lit)// is det.
%! rdf_print_literal(+Lit, +Opts)// is det.
% The following options are supported:
%   * abbr_iri(+boolean)
%   * ellip_lit(+or([nonneg,oneof([inf])]))
%   * ellip_ln(+or([nonneg,oneof([inf])]))
%   * label_iri(+boolean)
%   * lrange(+list(atom))
%   * symbol_iri(+boolean)
%   * symbol_iri(+boolean)

rdf_print_literal(Lit) -->
  rdf_print_literal(Lit, []).
rdf_print_literal(V^^D, Opts) --> !,
  {rdf_lexical_form(V^^D, Lex^^_)},
  "〈", rdf_print_datatype(D, Opts), ", ", rdf_print_lexical(Lex, Opts), "〉".
rdf_print_literal(Lex@LTag, Opts) -->
  {rdf_equal(rdf:langString, D)},
  "〈", rdf_print_datatype(D, Opts), ", ",
  "〈", rdf_print_lexical(Lex, Opts), ", ",
  rdf_print_language_tag(LTag, Opts), "〉", "〉".


%! rdf_print_object(+O, +Opts)// is det.
%! rdf_print_object(+O, +Opts)// is det.
% The following options are supported:
%   * abbr_iri(+boolean)
%   * abbr_list(+boolean)
%   * ellip_lit(+or([nonneg,oneof([inf])]))
%   * ellip_ln(+or([nonneg,oneof([inf])]))
%   * label_iri(+boolean)
%   * lrange(+list(atom))
%   * symbol_iri(+boolean)

rdf_print_object(O) -->
  rdf_print_object(O, []).
rdf_print_object(O, Opts) -->
  rdf_print_term(O, Opts).



%! rdf_print_predicate(+P, +Opts)// is det.
%! rdf_print_predicate(+P, +Opts)// is det.
%   * abbr_iri(+boolean)
%   * ellip_ln(+or([nonneg,oneof([inf])]))
%   * label_iri(+boolean)
%   * lrange(+list(atom))
%   * symbol_iri(+boolean)

rdf_print_predicate(P) -->
  rdf_print_predicate(P, []).
rdf_print_predicate(P, Opts) -->
  rdf_print_iri(P, Opts).



%! rdf_print_graphs is det.
%! rdf_print_graphs(+Opts) is det.
% Options are passed to dcg_table//2.

rdf_print_graphs:-
  rdf_print_graphs([maximum_number_of_rows(50)]).

rdf_print_graphs(Opts) :-
  aggregate_all(set(N-G), rdf_number_of_triples(G, N), Pairs1),
  reverse(Pairs1, Pairs2),
  maplist(inverse_pair, Pairs2, Pairs3),
  maplist(pair_list, Pairs3, DataRows),
  dcg_with_output_to(
    user_output,
    dcg_table([head(['Graph','Number of statements'])|DataRows], Opts)
  ).



%! rdf_print_quad(+Quad) is det.
% Wrapper around rdf_print_quad/2.

rdf_print_quad(Q) :-
  rdf_print_quad(Q, []).


%! rdf_print_quad(+Quad, +Opts) is det.
% Deterministically prints the given ground Quad in plain text.
%
% Options are passed to rdf_print_tuple/5.

rdf_print_quad(rdf(S,P,O,G), Opts) :-
  rdf_print_tuple(S, P, O, G, Opts).


%! rdf_print_quad(?S, ?P, ?O) is det.
% Wrapper around rdf_print_quad/4 with uninstantiated graph.

rdf_print_quad(S, P, O) :-
  rdf_print_quad(S, P, O, _).


%! rdf_print_quad(?S, ?P, ?O, ?G) is det.
% Wrapper around rdf_print_quad/5 with default options.

rdf_print_quad(S, P, O, G) :-
  rdf_print_quad(S, P, O, G, []).


%! rdf_print_quad(+S, +P, +O, +G, +Opts) is det.
%! rdf_print_quad(?S, ?P, ?O, ?G, +Opts) is nondet.
% If the simple quad pattern 〈S,P,O,G〉 is ground
% this deterministically prints the quad in plain text.
%
% Otherwise, this non-deterministically matches quads from the RDF DB
% based on the given instantiation pattern and prints them in plain text.
%
% Options are passed to rdf_print_tuple/5.

% Allow ground tuples to be printed without being in the database.
rdf_print_quad(S, P, O, G, Opts) :-
  ground(rdf(S,P,O,G)), !,
  rdf_print_tuple(S, P, O, G, Opts).
rdf_print_quad(S, P, O, G, Opts) :-
  rdf(S, P, O, G),
  dcg_with_output_to(current_output, rdf_print_tuple(S, P, O, G, Opts)),
  nl.



%! rdf_print_quads(+Quads:list) is det.
% Wrapper around rdf_print_quads/2 with default options.

rdf_print_quads(Qs) :-
  rdf_print_quads(Qs, []).


%! rdf_print_quads(+Quads:list, +Opts) is det.
% Print the given collection of ground quads.
%
% Options are passed to rdf_print_tuple/5
% except for option abbr_list/1 which is set to `false`
% since RDF list abbreviation does generally not make sense
% when given an isolated collection of quads.

rdf_print_quads(Qs, Opts0) :-
  merge_options([abbr_list(false)], Opts0, Opts),
  forall(member(rdf(S,P,O,G), Qs), rdf_print_tuple(S, P, O, G, Opts)).



%! rdf_print_subject(+S)// is det.
%! rdf_print_subject(+S, +Opts)// is det.
% The following options are supported:
%   * abbr_iri(+boolean)
%   * abbr_list(+boolean)
%   * ellip_ln(+or([nonneg,oneof([inf])]))

rdf_print_subject(S) -->
  rdf_print_subject(S, []).
rdf_print_subject(S, Opts) -->
  rdf_print_term(S, Opts).



%! rdf_print_term(+T) is det.
%! rdf_print_term(+T, +Opts) is det.
% The following options are supported:
%   * abbr_iri(+boolean)
%   * abbr_list(+boolean)
%   * ellip_lit(+or([nonneg,oneof([inf])]))
%   * label_iri(+boolean)
%   * lrange(+list(atom))
%   * symbol_iri(+boolean)

rdf_print_term(T) :-
  rdf_print_term(T, []).
rdf_print_term(T, Opts) :-
  dcg_with_output_to(current_output, rdf_print_term(T, Opts)).



%! rdf_print_term(+T)// is det.
%! rdf_print_term(+T, +Opts)// is det.
% The following options are supported:
%   * abbr_iri(+boolean)
%   * abbr_list(+boolean)
%   * ellip_lit(+or([nonneg,oneof([inf])]))
%   * ellip_ln(+or([nonneg,oneof([inf])]))
%   * label_iri(+boolean)
%   * lrange(+list(atom))
%   * symbol_iri(+boolean)

rdf_print_term(T) -->
  rdf_print_term(T, []).
rdf_print_term(T, Opts) -->
  {rdf_is_literal(T)}, !,
  rdf_print_literal(T, Opts).
rdf_print_term(T, Opts) -->
  {rdf_is_bnode(T)}, !,
  rdf_print_bnode(T, Opts).
rdf_print_term(T, Opts) -->
  rdf_print_iri(T, Opts), !.
rdf_print_term(T, _) -->
  term(T).



%! rdf_print_tuple(+Tuple) is det.
% Wrapper around rdf_print_tuple/2 with default options.

rdf_print_tuple(T) :-
  rdf_print_tuple(T, []).


%! rdf_print_tuple(+Tuple, +Opts) is det.

rdf_print_tuple(T, Opts0) :-
  merge_options([abbr_list(false)], Opts0, Opts),
  (   % Tuple is a triple.
      T = rdf(_,_,_)
  ->  rdf_print_triple(T, Opts)
  ;   % Tuple is a quad.
      T = rdf(_,_,_,_)
  ->  rdf_print_quad(T, Opts)
  ).


%! rdf_print_tuple(+S, +P, +O, +G, +Opts) is det.
% The following options are supported:
%   * abbr_iri(+boolean)
%   * abbr_list(+boolean)
%   * ellip_lit(+or([nonneg,oneof([inf])]))
%   * indent(+nonneg)
%   * label_iri(+boolean)
%   * lrange(+list(atom))

rdf_print_tuple(S, P, O, G, Opts) :-
  option(indent(I), Opts, 0), tab(I),
  dcg_with_output_to(current_output, rdf_print_tuple(S, P, O, G, Opts)),
  nl.



%! rdf_print_tuple(+S, +P, +O, ?G)// is det.
%! rdf_print_tuple(+S, +P, +O, ?G, +Opts)// is det.
%   * abbr_iri(+boolean)
%   * abbr_list(+boolean)
%   * ellip_lit(+or([nonneg,oneof([inf])]))
%   * ellip_ln(+or([nonneg,oneof([inf])]))
%   * label_iri(+boolean)
%   * lrange(+list(atom))
%   * symbol_iri(+boolean)

rdf_print_tuple(S, P, O, G) -->
  rdf_print_tuple(S, P, O, G, []).

rdf_print_tuple(S, P, O, G, Opts) -->
  "〈",
  rdf_print_subject(S, Opts),
  ", ",
  rdf_print_predicate(P, Opts),
  ", ",
  rdf_print_object(O, Opts),
  "〉",
  ({var(G)} -> "" ; rdf_print_graph(G, Opts)).



%! rdf_print_tuples(+Tuples:list) is det.
% Wrapper around rdf_print_tuples/2 with default options.

rdf_print_tuples(L) :-
  rdf_print_tuples(L, []).


%! rdf_print_tuples(+Tuples:list, +Opts) is det.

rdf_print_tuples([], _) :- !.
rdf_print_tuples([H|T], Opts) :-
  rdf_print_tuple(H, Opts),
  rdf_print_tuples(T, Opts).



%! rdf_print_triple(+Triple) is det.
% Wrapper around rdf_print_triple/2 with default options.



%! rdf_print_triple(+Triple, +Opts) is det.
%! rdf_print_triple(?S, ?P, ?O) is nondet.
%! rdf_print_triple(?S, ?P, ?O, ?G) is nondet.
%! rdf_print_triple(?S, ?P, ?O, ?G, +Opts) is nondet.
% The following options are supported:
%   * abbr_iri(+boolean)
%   * abbr_list(+boolean)
%   * ellip_lit(+or([nonneg,oneof([inf])]))
%   * indent(+nonneg)
%   * label_iri(+boolean)
%   * lrange(+list(atom))
%   * symbol_iri(+boolean)

rdf_print_triple(T) :-
  rdf_print_triple(T, []).
rdf_print_triple(rdf(S,P,O), Opts) :-
  rdf_print_tuple(S, P, O, _, Opts).
rdf_print_triple(S, P, O) :-
  rdf_print_triple(S, P, O, _).
rdf_print_triple(S, P, O, G) :-
  rdf_print_triple(S, P, O, G, []).
% Allow ground tuples to be printed without being in the database.
rdf_print_triple(S, P, O, _, Opts) :-
  ground(rdf(S,P,O)), !,
  rdf_print_tuple(S, P, O, _, Opts).
rdf_print_triple(S, P, O, G, Opts) :-
  rdf(S, P, O, G),
  dcg_with_output_to(current_output, rdf_print_tuple(S, P, O, _, Opts)),
  nl.



%! rdf_print_triples(+Triples) is det.
%! rdf_print_triples(+Triples, +Opts) is det.

rdf_print_triples(Triples) :-
  rdf_print_triples(Triples, []).
rdf_print_triples(Triples, Opts0) :-
  merge_options([abbr_list(false)], Opts0, Opts),
  forall(member(rdf(S,P,O), Triples), rdf_print_tuple(S, P, O, _, Opts)).



%! rdf_symbol_iri(+Iri)// is det.
% Writes a symbolic representation for the given IRI.

rdf_symbol_iri(owl:equivalentClass)    --> "≡".
rdf_symbol_iri(owl:equivalentProperty) --> "≡".
rdf_symbol_iri(owl:sameAs)             --> "≡".
rdf_symbol_iri(rdfs:subClassOf)        --> "⊆".
rdf_symbol_iri(rdf:type)               --> "∈".
