:- module(
  rdf_print_stmt,
  [
    rdf_print_quadruple/1, % +Quadruple
    rdf_print_quadruple/2, % +Quadruple:compound
                           % +Options:list(compound)
    rdf_print_quadruple/3, % ?Subject, ?Predicate, ?Object
    rdf_print_quadruple/4, % ?Subject, ?Predicate, ?Object, ?Graph
    rdf_print_quadruple/5, % ?Subject:rdf_term
                           % ?Predicate:iri
                           % ?Object:rdf_term
                           % ?Graph:atom
                           % +Options:list(compound)
    rdf_print_quadruples/1, % +Quadruples
    rdf_print_quadruples/2, % +Quadruples:list(compoud)
                            % +Options:list(compound)
    rdf_print_statement/1, % +Statement
    rdf_print_statement/2, % +Statement:compoud
                           % +Options:list(compound)
    rdf_print_statement//5, % +Subject:rdf_term,
                            % +Predicate:iri,
                            % +Object:rdf_term,
                            % ?Graph:atom,
                            % +Options:list(compound)
    rdf_print_statements/1, % +Statements
    rdf_print_statements/2, % +Statements:list(compoud)
                            % +Options:list(compound)
    rdf_print_triple/1, % +Triple
    rdf_print_triple/2, % +Triple:compound
                        % +Options:list(compound)
    rdf_print_triple/3, % ?Subject, ?Predicate, ?Object
    rdf_print_triple/4, % ?Subject, ?Predicate, ?Object, ?Graph
    rdf_print_triple/5, % ?Subject:rdf_term
                        % ?Predicate:iri
                        % ?Object:rdf_term
                        % ?Graph:atom
                        % +Options:list(compound)
    rdf_print_triples/1, % +Triples
    rdf_print_triples/2 % +Triples:list(compoud)
                        % +Options:list(compound)
  ]
).

/** <module> RDF statement printing

@author Wouter Beek
@version 2015/07-2015/09
*/

:- use_module(library(dcg/dcg_bracketed)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(rdf/rdf_read)).
:- use_module(library(semweb/rdf_db)).

:- rdf_meta(rdf_print_quadruple(t)).
:- rdf_meta(rdf_print_quadruple(t,+)).
:- rdf_meta(rdf_print_quadruple(o,r,o)).
:- rdf_meta(rdf_print_quadruple(o,r,o,?)).
:- rdf_meta(rdf_print_quadruple(o,r,o,?,+)).
:- rdf_meta(rdf_print_quadruples(t)).
:- rdf_meta(rdf_print_quadruples(t,+)).
:- rdf_meta(rdf_print_statement(t)).
:- rdf_meta(rdf_print_statement(t,+)).
:- rdf_meta(rdf_print_statements(t)).
:- rdf_meta(rdf_print_statements(t,+)).
:- rdf_meta(rdf_print_triple(t)).
:- rdf_meta(rdf_print_triple(t,+)).
:- rdf_meta(rdf_print_triple(o,r,o)).
:- rdf_meta(rdf_print_triple(o,r,o,?)).
:- rdf_meta(rdf_print_triple(o,r,o,?,+)).
:- rdf_meta(rdf_print_triples(t)).
:- rdf_meta(rdf_print_triples(t,+)).

:- predicate_options(rdf_print_quadruple/2, 2, [
     pass_to(rdf_print_statement/5, 5)
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
:- predicate_options(rdf_print_statement//5, 5, [
     style(+oneof([tuple,turtle])),
     pass_to(rdf_print_graph_maybe//2, 2),
     pass_to(rdf_print_object//2, 2),
     pass_to(rdf_print_predicate//2, 2),
     pass_to(rdf_print_subject//2, 2)
   ]).
:- predicate_options(rdf_print_statements/2, 2, [
     pass_to(rdf_print_statement/2, 2)
   ]).
:- predicate_options(rdf_print_triple/2, 2, [
     pass_to(rdf_print_statement/5, 5)
   ]).
:- predicate_options(rdf_print_triple/5, 5, [
     pass_to(rdf_print_statement/5, 5)
   ]).
:- predicate_options(rdf_print_triples/2, 2, [
     pass_to(rdf_print_statement/5, 5)
   ]).





%! rdf_print_quadruple(+Quadruple:compound) is det.
% Wrapper around rdf_print_quadruple/2.

rdf_print_quadruple(Q):-
  rdf_print_quadruple(Q, []).

%! rdf_print_quadruple(+Quadruple:compound, +Options:list(compound)) is det.
% Deterministically prints the given ground Quadruple in plain text.
%
% Options are passed to rdf_print_statement/5.

rdf_print_quadruple(rdf(S,P,O,G), Opts):-
  rdf_print_statement(S, P, O, G, Opts).

%! rdf_print_quadruple(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Object:rdf_term
%! ) is det.
% Wrapper around rdf_print_quadruple/4 with uninstantiated graph.

rdf_print_quadruple(S, P, O):-
  rdf_print_quadruple(S, P, O, _).

%! rdf_print_quadruple(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   ?Graph:atom
%! ) is det.
% Wrapper around rdf_print_quadruple/5 with default options.

rdf_print_quadruple(S, P, O, G):-
  rdf_print_quadruple(S, P, O, G, []).

%! rdf_print_quadruple(
%!   +Subject:rdf_term,
%!   +Predicate:iri,
%!   +Object:rdf_term,
%!   +Graph:atom,
%!   +Options:list(compound)
%! ) is det.
%! rdf_print_quadruple(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   ?Graph:atom,
%!   +Options:list(compound)
%! ) is nondet.
% If the simple quadruple pattern 〈Subject,Predicate,Object,Graph〉 is ground
% this deterministically prints the quadruple in plain text.
%
% Otherwise, this non-deterministically matches quadruples from the RDF DB
% based on the given instantiation pattern and prints them in plain text.
%
% Options are passed to rdf_print_statement/5.

rdf_print_quadruple(S, P, O, G, Opts):-
  (ground(rdf(S,P,O,G)) -> true ; rdf2(S, P, O, G)),
  rdf_print_statement(S, P, O, G, Opts).



%! rdf_print_quadruples(+Quadruples:list(compound)) is det.
% Wrapper around rdf_print_quadruples/2 with default options.

rdf_print_quadruples(Qs):-
  rdf_print_quadruples(Qs, []).

%! rdf_print_quadruples(
%!   +Quadruples:list(compound),
%!   +Options:list(compound)
%! ) is det.
% Print the given collection of ground quadruples.
%
% Options are passed to rdf_print_statement/5
% except for option abbr_list/1 which is set to `false`
% since RDF list abbreviation does generally not make sense
% when given an isolated collection of quadruples.

rdf_print_quadruples(Qs, Opts0):-
  merge_options([abbr_list(false)], Opts0, Opts),
  forall(member(rdf(S,P,O,G), Qs), rdf_print_statement(S, P, O, G, Opts)).



%! rdf_print_statement(+Statement:compound) is det.
% Wrapper around rdf_print_statement/2 with default options.

rdf_print_statement(T):-
  rdf_print_statement(T, []).

%! rdf_print_statement(+Statement:compound, +Options:list(compound)) is det.

rdf_print_statement(T, Opts0):-
  merge_options([abbr_list(false)], Opts0, Opts),
  (   % Statement is a triple.
      T = rdf(_,_,_)
  ->  rdf_print_triple(T, Opts)
  ;   % Statement is a quadruple.
      T = rdf(_,_,_,_)
  ->  rdf_print_quadruple(T, Opts)
  ).

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
%   * label_iri(+boolean)
%   * lang_pref(+atom)
%   * style(+oneof([tuple,turtle])

rdf_print_statement(S, P, O, G, Opts):-
  option(indent(I), Opts, 0),
  tab(I),
  dcg_with_output_to(current_output, rdf_print_statement(S, P, O, G, Opts)),
  nl.



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
%   * label_iri(+boolean)
%   * lang_pref(+atom)
%   * symbol_iri(+boolean)
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
  ({var(G)} -> "" ; rdf_print_graph(G, Opts)),
  " .".
rdf_print_statement(S, P, O, G, Opts) -->
  bracketed(langular, (
    rdf_print_subject(S, Opts),
    ", ",
    rdf_print_predicate(P, Opts),
    ", ",
    rdf_print_object(O, Opts)
  )),
  ({var(G)} -> "" ; rdf_print_graph(G, Opts)).



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



%! rdf_print_triple(+Triple:compound) is det.
% Wrapper around rdf_print_triple/2 with default options.

rdf_print_triple(T):-
  rdf_print_triple(T, []).

%! rdf_print_triple(+Triple:compound, +Options:list(compound)) is det.

rdf_print_triple(rdf(S,P,O), Opts):-
  rdf_print_statement(S, P, O, _, Opts).

%! rdf_print_triple(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Object:rdf_term
%! ) is nondet.
% Wrapper around rdf_print_triple/4 with uninstantiated graph.

rdf_print_triple(S, P, O):-
  rdf_print_triple(S, P, O, _).

%! rdf_print_triple(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   ?Graph:atom
%! ) is nondet.
% Wrapper around rdf_print_triple/5 with default options.

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
%   * label_iri(+boolean)
%   * lang_pref(+atom)
%   * style(+oneof([tuple,turtle])
%   * symbol_iri(+boolean)

rdf_print_triple(S, P, O, G, Opts):-
  (ground(rdf(S,P,O)) -> true ; rdf2(S, P, O, G)),
  rdf_print_statement(S, P, O, G, Opts).



%! rdf_print_triples(+Triples:list(compound)) is det.
% Wrapper around rdf_print_triples/2 with default options.

rdf_print_triples(Ts):-
  rdf_print_triples(Ts, []).

%! rdf_print_triples(+Triples:list(compound), +Options:list(compound)) is det.

rdf_print_triples(Ts, Opts0):-
  merge_options([abbr_list(false)], Opts0, Opts),
  forall(member(rdf(S,P,O), Ts), rdf_print_statement(S, P, O, _, Opts)).
