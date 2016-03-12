:- module(
  rdf_print_stmt,
  [
    rdf_print_quad/1,    % +Quad
    rdf_print_quad/2,    % +Quad, +Opts
    rdf_print_quad/3,    % ?S, ?P, ?O
    rdf_print_quad/4,    % ?S, ?P, ?O, ?G
    rdf_print_quad/5,    % ?S, ?P, ?O, ?G, +Opts
    rdf_print_quads/1,   % +Quads
    rdf_print_quads/2,   % +Quads, +Opts
    rdf_print_tuple/1,   % +Tuple
    rdf_print_tuple/2,   % +Tuple, +Opts
    rdf_print_tuple/5,   % +S, +P, +O, ?G, +Opts
    rdf_print_tuple//4,  % +S, +P, +O, ?G
    rdf_print_tuple//5,  % +S, +P, +O, ?G, +Opts
    rdf_print_tuples/1,  % +Tuples
    rdf_print_tuples/2,  % +Tuples, +Opts
    rdf_print_triple/1,  % +Triple
    rdf_print_triple/2,  % +Triple, +Opts
    rdf_print_triple/3,  % ?S, ?P, ?O
    rdf_print_triple/4,  % ?S, ?P, ?O, ?G
    rdf_print_triple/5,  % ?S, ?P, ?O, ?G, +Opts
    rdf_print_triples/1, % +Triples
    rdf_print_triples/2  % +Triples, +Opts
  ]
).

/** <module> RDF tuple printing

@author Wouter Beek
@version 2015/07-2015/09, 2015/11-2016/03
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(rdf/rdf_print_term)).
:- use_module(library(semweb/rdf11)).

:- rdf_meta
   rdf_print_quad(t),
   rdf_print_quad(t, +),
   rdf_print_quad(r, r, o),
   rdf_print_quad(r, r, o, ?),
   rdf_print_quad(r, r, o, ?, +),
   rdf_print_quads(t),
   rdf_print_quads(t, +),
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
   rdf_print_triples(t, +).

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
