:- module(
  rdf_print,
  [
    rdf_print_deref/1,	% +S
    rdf_print_deref/2,	% +S, +Opts
    rdf_print_descr/1,	% +S
    rdf_print_descr/2,	% +S, +Opts
    rdf_print_descr/3,	% +S, ?G, +Opts
    rdf_print_graph/1,	% ?G
    rdf_print_graph/2,	% ?G, +Opts
    rdf_print_graphs/0,
    rdf_print_graphs/1	% +Opts
  ]
).
:- reexport(library(rdf/rdf_print_stmt)).
:- reexport(library(rdf/rdf_print_term)).

/** <module> RDF: Printing

Printing of RDF statements to a text-based output stream.

@author Wouter Beek
@version 2015/07-2015/09, 2015/12-2016/01
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_table)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(pair_ext)).
:- use_module(library(rdf/rdf_deref)).
:- use_module(library(rdf/rdf_stats)).
:- use_module(library(semweb/rdf11)).

:- set_prolog_flag(toplevel_print_anon, false).

:- rdf_meta
   rdf_print_deref(r),
   rdf_print_deref(r, +),
   rdf_print_descr(r),
   rdf_print_descr(r, +),
   rdf_print_descr(r, r, +),
   rdf_print_graph(r),
   rdf_print_graph(r, +).

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





%! rdf_print_deref(+S) is det.
%! rdf_print_deref(+S, +Opts) is det.

rdf_print_deref(S) :-
  rdf_print_deref(S, []).
rdf_print_deref(S, Opts) :-
  rdf_deref(S),
  rdf_print_triple(S, _, _, _, Opts).



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
