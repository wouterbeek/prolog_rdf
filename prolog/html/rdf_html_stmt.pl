:- module(
  rdf_html_stmt,
  [
    rdf_html_describe//1,	% +S
    rdf_html_describe//2,	% +S, +Opts
    rdf_html_describe//3,	% +S, ?G, +Opts
    rdf_html_quadruple//1,	% +Quad
    rdf_html_quadruple//2,	% +Quad, +Opts
    rdf_html_quadruple//4,	% ?S, ?P, ?O, ?G
    rdf_html_quadruple//5,	% ?S, ?P, ?O, ?G, +Opts
    rdf_html_quadruples//1,	% +Quads
    rdf_html_quadruples//2,	% +Quads, +Opts
    rdf_html_statement//1,	% +Stmt
    rdf_html_statement//2,	% +Stmt, +Opts
    rdf_html_statements//1,	% +Stmts
    rdf_html_statements//2,	% +Stmts, +Opts
    rdf_html_term//1,		% +T
    rdf_html_term//2,		% +T, +Opts
    rdf_html_triple//1,		% +Trip
    rdf_html_triple//2,		% +Trip, +Opts
    rdf_html_triple//3,		% ?S, ?P, ?O
    rdf_html_triple//4,		% ?S, ?P, ?O, ?G
    rdf_html_triple//5,		% ?S, ?P, ?O, ?G, +Opts
    rdf_html_triples//1,	% +Trips
    rdf_html_triples//2		% +Trips, +Opts
  ]
).

/** <module> Generate HTML representations of RDF statements

@author Wouter Beek
@version 2015/08-2015/09, 2015/12-2016/01
*/

:- use_module(library(error)).
:- use_module(library(html/rdf_html_term)).
:- use_module(library(http/html_write)).
:- use_module(library(option)).
:- use_module(library(rdf11/rdf11)).

:- rdf_meta
   rdf_html_describe(r, ?, ?),
   rdf_html_describe(r, +, ?, ?),
   rdf_html_describe(r, ?, r, ?, ?),
   rdf_html_quadruple(t, ?, ?),
   rdf_html_quadruple(t, +, ?, ?),
   rdf_html_quadruple(r, r, o, r, ?, ?),
   rdf_html_quadruple(r, r, o, r ,+, ?, ?),
   rdf_html_quadruples(t, ?, ?),
   rdf_html_quadruples(t, +, ?, ?),
   rdf_html_statement(t, ?, ?),
   rdf_html_statement(t, +, ?, ?),
   rdf_html_statements(t, ?, ?),
   rdf_html_statements(t, +, ?, ?),
   rdf_html_term(o, ?, ?),
   rdf_html_term(o, +, ?, ?),
   rdf_html_triple(t, ?, ?),
   rdf_html_triple(t, +, ?, ?),
   rdf_html_triple(r, r, o, ?, ?),
   rdf_html_triple(r, r, o, r, ?, ?),
   rdf_html_triple(r, r, o, r, +, ?, ?),
   rdf_html_triples(t, ?, ?),
   rdf_html_triples(t, +, ?, ?).

:- predicate_options(rdf_html_describe//2, 2, [
     pass_to(rdf_html_describe//3, 3)
   ]).
:- predicate_options(rdf_html_describe//3, 3, [
     pass_to(rdf_html_statement//5, 5)
   ]).
:- predicate_options(rdf_html_quadruple//2, 2, [
     pass_to(rdf_html_statement//5, 5)
   ]).
:- predicate_options(rdf_html_quadruple//5, 5, [
     pass_to(rdf_html_quadruple//2, 2),
     pass_to(rdf_html_statement//5, 5)
   ]).
:- predicate_options(rdf_html_quadruples//2, 2, [
     pass_to(rdf_html_quadruple//2, 2)
   ]).
:- predicate_options(rdf_html_statement//2, 2, [
     pass_to(rdf_html_quadruple//2, 2),
     pass_to(rdf_html_triple//2, 2)
   ]).
:- predicate_options(rdf_html_statement//5, 5, [
     pass_to(rdf_html_object//2, 2),
     pass_to(rdf_html_predicate//2, 2),
     pass_to(rdf_html_subject//2, 2)
   ]).
:- predicate_options(rdf_html_statements//2, 2, [
     pass_to(rdf_html_statement//2, 2)
   ]).
:- predicate_options(rdf_html_triple//2, 2, [
     pass_to(rdf_html_statement//5, 5)
   ]).
:- predicate_options(rdf_html_triple//4, 4, [
     pass_to(rdf_html_triple//5, 5)
   ]).
:- predicate_options(rdf_html_triple//5, 5, [
     pass_to(rdf_html_statement//5, 5),
     pass_to(rdf_html_triples//2, 2)
   ]).
:- predicate_options(rdf_html_triples//2, 2, [
     pass_to(rdf_html_statement//2, 2)
   ]).





%! rdf_html_describe(+S)// is det.
%! rdf_html_describe(+S, +Opts)// is det.
%! rdf_html_describe(+S, ?G, +Opts)// is det.
% The following options are supported:
%   * abbr_iri(+boolean)
%   * abbr_list(+boolean)
%   * ellip_lit(+or([nonneg,oneof([inf])]))
%   * page_size(+nonneg)
%   * symbol_iri(+boolean)

rdf_html_describe(S) -->
  rdf_html_describe(S, []).

rdf_html_describe(S, Opts) -->
  rdf_html_describe(S, _, Opts).

% No graph is given: display quadruples.
rdf_html_describe(S, G, Opts) -->
  {var(G)}, !,
  rdf_html_quadruple(S, _, _, G, Opts).
% A graph is given: display triples.
rdf_html_describe(S, G, Opts) -->
  rdf_html_triple(S, _, _, G, Opts).



%! rdf_html_quadruple(+Quad)// is det.
%! rdf_html_quadruple(+Quad, +Opts)// is det.
%! rdf_html_quadruple(?S, ?P, ?O, ?G)// is det.
%! rdf_html_quadruple(?S, ?P, ?O, ?G, +Opts)// is det.
% The following options are supported:
%   * abbr_iri(+boolean)
%   * abbr_list(+boolean)
%   * ellip_lit(+or([nonneg,oneof([inf])]))
%   * symbol_iri(+boolean)
%   * page_size(+nonneg)

rdf_html_quadruple(Q) -->
  rdf_html_quadruple(Q, []).

rdf_html_quadruple(rdf(S,P,O,G), Opts) -->
  rdf_html_statement(S, P, O, G, Opts).

rdf_html_quadruple(S, P, O, G) -->
  rdf_html_quadruple(S, P, O, G, []).

% Ground quadruples are printed without them having to be present
% in the RDF DB.
rdf_html_quadruple(S, P, O, G, Opts) -->
  {ground(rdf(S,P,O,G))}, !,
  rdf_html_statement(S, P, O, G, Opts).
% Non-ground quadruples are non-deterministically matched
% against the RDF DB.
rdf_html_quadruple(S, P, O, G, Opts) -->
  {option(page_size(N), Opts, 10),
   findnsols(N, rdf(S,P,O,G), rdf(S, P, O, G), Ts)},
  'rdf_html_quadruple*'(Ts, Opts).



%! rdf_html_quadruples(+Quads)// is det.
%! rdf_html_quadruples(+Quads, +Opts)// is det.

rdf_html_quadruples(Qs) -->
  rdf_html_quadruples(Qs, []).

rdf_html_quadruples(Qs, Opts0) -->
  {merge_options([abbr_list(false)], Opts0, Opts)},
  'rdf_html_quadruple*'(Qs, Opts).

'rdf_html_quadruple*'([], _) --> [].
'rdf_html_quadruple*'([H|T], Opts) -->
  rdf_html_quadruple(H, Opts),
  'rdf_html_quadruple*'(T, Opts).



%! rdf_html_statement(+Stmt)// is det.
%! rdf_html_statement(+Stmt, +Opts)// is det.
%! rdf_html_statement(+S, +P, +O, ?G, +Opts)// is det.
%   * abbr_iri(+boolean)
%   * abbr_list(+boolean)
%   * ellip_lit(+or([nonneg,oneof([inf])]))
%   * ellip_ln(+or([nonneg,oneof([inf])]))
%   * label_iri(+boolean)
%   * page_size(+nonneg)
%   * symbol_iri(+boolean)

rdf_html_statement(T) -->
  rdf_html_statement(T, []).

rdf_html_statement(T, Opts0) -->
  {merge_options([abbr_list(false)], Opts0, Opts)},
  (   % Statement is a triple.
      {T = rdf(_,_,_)}
  ->  rdf_html_triple(T, Opts)
  ;   % Statement is a quadruple.
      {T = rdf(_,_,_,_)}
  ->  rdf_html_quadruple(T, Opts)
  ).

rdf_html_statement(S, P, O, G, Opts) -->
  html(
    span(class='rdf-stmt', [
      &(lang),
      \rdf_html_subject(S, Opts),
      ', ',
      \rdf_html_predicate(P, Opts),
      ', ',
      \rdf_html_object(O, Opts),
      \rdf_html_graph(G, Opts),
      &(rang)
    ])
  ).


%! rdf_html_statements(+Stmts)// is det.
%! rdf_html_statements(+Stmts, +Opts)// is det.

rdf_html_statements(Ss) -->
  rdf_html_statement(Ss, []).

rdf_html_statements(Ss, Opts0) -->
  {merge_options([abbr_list(false)], Opts0, Opts)},
  'rdf_html_statement*'(Ss, Opts).

'rdf_html_statement*'([], _) --> [].
'rdf_html_statement*'([H|T], Opts) -->
  rdf_html_statement(H, Opts),
  'rdf_html_statement*'(T, Opts).



%! rdf_html_triple(+Trip)// is det.
%! rdf_html_triple(+Trip, +Opts)// is det.

rdf_html_triple(T) -->
  rdf_html_triple(T, []).

rdf_html_triple(rdf(S,P,O), Opts) -->
  rdf_html_statement(S, P, O, _, Opts).


%! rdf_html_triple(?S, ?P, ?O)// is nondet.
%! rdf_html_triple(?S, ?P, ?O, ?G)// is nondet.
%! rdf_html_triple(?S, ?P, ?O, ?G, +Opts)// is det.
% The following options are supported:
%   * abbr_iri(+boolean)
%   * abbr_list(+boolean)
%   * ellip_lit(+or([nonneg,oneof([inf])]))
%   * label_iri(+boolean)
%   * page_size(+nonneg)
%   * symbol_iri(+boolean)

rdf_html_triple(S, P, O) -->
  rdf_html_triple(S, P, O, _).

rdf_html_triple(S, P, O, G) -->
  rdf_html_triple(S, P, O, G, []).

% Ground triples are printing without them having to be present
% in the RDF DB.
rdf_html_triple(S, P, O, G, Opts) -->
  {ground(rdf(S,P,O))}, !,
  rdf_html_statement(S, P, O, G, Opts).
% Non-ground triples are non-deterministically matched
% against the RDF DB.
rdf_html_triple(S, P, O, G, Opts) -->
  {option(page_size(N), Opts, 10),
   findnsols(N, rdf(S,P,O), rdf(S, P, O, G), Ts)},
  rdf_html_triples(Ts, Opts).



%! rdf_html_triples(+Trips)// is det.
%! rdf_html_triples(+Trips, +Opts)// is det.

rdf_html_triples(Ts) -->
  rdf_html_triples(Ts, []).

rdf_html_triples(Ts, Opts0) -->
  {merge_options([abbr_list(false)], Opts0, Opts)},
  'rdf_html_statement*'(Ts, Opts).
