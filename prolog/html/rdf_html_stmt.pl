:- module(
  rdf_html_stmt,
  [
    rdf_html_describe//1, % +Subject
    rdf_html_describe//2, % +Subject, +Options
    rdf_html_describe//3, % +Subject:rdf_term
                          % ?Graph:atom
                          % +Options:list(compound)
    rdf_html_quadruple//1, % +Quadruple
    rdf_html_quadruple//2, % +Quadruple:compound
                           % +Options:list(compound)
    rdf_html_quadruple//4, % ?Subject, ?Predicate, ?Object, ?Graph
    rdf_html_quadruple//5, % ?Subject:rdf_term
                           % ?Predicate:iri
                           % ?Object:rdf_term
                           % ?Graph:atom
                           % +Options:list(compound)
    rdf_html_quadruples//1, % +Quadruples:list(compoud)
    rdf_html_quadruples//2, % +Quadruples:list(compoud)
                            % +Options:list(compound)
    rdf_html_statement//1, % +Statement:compoud
    rdf_html_statement//2, % +Statement:compoud
                           % +Options:list(compound)
    rdf_html_statements//1, % +Statements:list(compoud)
    rdf_html_statements//2, % +Statements:list(compoud)
                            % +Options:list(compound)
    rdf_html_term//1, % +Term
    rdf_html_term//2, % +Term:rdf_term
                      % +Options:list(compound)
    rdf_html_triple//1, % +Triple
    rdf_html_triple//2, % +Triple:compound
                        % +Options:list(compound)
    rdf_html_triple//4, % ?Subject, ?Predicate, ?Object, ?Graph
    rdf_html_triple//5, % ?Subject:rdf_term
                        % ?Predicate:iri
                        % ?Object:rdf_term
                        % ?Graph:atom
                        % +Options:list(compound)
    rdf_html_triples//1, % +Triples:list(compoud)
    rdf_html_triples//2 % +Triples:list(compoud)
                        % +Options:list(compound)
  ]
).

/** <module> Generate HTML representations of RDF statements

@author Wouter Beek
@version 2015/08-2015/09, 2015/12
*/

:- use_module(library(error)).
:- use_module(library(html/rdf_html_term)).
:- use_module(library(http/html_write)).
:- use_module(library(option)).
:- use_module(library(rdf/rdf_graph)).
:- use_module(library(rdf/rdf_read)).

:- rdf_meta(rdf_html_describe(o,?,?)).
:- rdf_meta(rdf_html_describe(o,+,?,?)).
:- rdf_meta(rdf_html_describe(o,?,+,?,?)).
:- rdf_meta(rdf_html_quadruple(t,?,?)).
:- rdf_meta(rdf_html_quadruple(t,+,?,?)).
:- rdf_meta(rdf_html_quadruple(o,r,o,?,?,?)).
:- rdf_meta(rdf_html_quadruple(o,r,o,?,+,?,?)).
:- rdf_meta(rdf_html_quadruples(t,?,?)).
:- rdf_meta(rdf_html_quadruples(t,+,?,?)).
:- rdf_meta(rdf_html_statement(t,?,?)).
:- rdf_meta(rdf_html_statement(t,+,?,?)).
:- rdf_meta(rdf_html_statements(t,?,?)).
:- rdf_meta(rdf_html_statements(t,+,?,?)).
:- rdf_meta(rdf_html_term(o,?,?)).
:- rdf_meta(rdf_html_term(o,+,?,?)).
:- rdf_meta(rdf_html_triple(t,?,?)).
:- rdf_meta(rdf_html_triple(t,+,?,?)).
:- rdf_meta(rdf_html_triple(o,r,o,?,?,?)).
:- rdf_meta(rdf_html_triple(o,r,o,?,+,?,?)).
:- rdf_meta(rdf_html_triples(t,?,?)).
:- rdf_meta(rdf_html_triples(t,+,?,?)).

:- predicate_options(rdf_html_describe//2, 2, [
     pass_to(rdf_html_describe//3, 3)
   ]).
:- predicate_options(rdf_html_describe//3, 3, [
     pass_to(rdf_html_statement//5, 5)
   ]).
:- predicate_options(rdf_html_graph_maybe//2, 2, [
     style(+oneof([tuple,turtle]))
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
     style(+oneof([tuple,turtle])),
     pass_to(rdf_html_graph_maybe//2, 2),
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





%! rdf_html_describe(+Subject:rdf_term)// is det.
% Wrapper around rdf_html_describe//2 with default options.

rdf_html_describe(S) -->
  rdf_html_describe(S, []).

%! rdf_html_describe(+Subject:rdf_term, +Options:list(compound))// is det.
% Wrapper around rdf_html_describe//3 with uninstantiated graph.

rdf_html_describe(S, Opts) -->
  rdf_html_describe(S, _, Opts).

%! rdf_html_describe(
%!   +Subject:rdf_term,
%!   ?Graph:atom,
%!   +Options:list(compound)
%! )// is det.
% The following options are supported:
%   * abbr_iri(+boolean)
%   * abbr_list(+boolean)
%   * ellip_lit(+or([nonneg,oneof([inf])]))
%   * page_size(+nonneg)
%   * language_priority_list(+list(atom))
%   * symbol_iri(+boolean)
%   * style(+oneof([tuple,turtle])

% No graph is given: display quadruples.
rdf_html_describe(S, G, Opts) -->
  {var(G)}, !,
  rdf_html_quadruple(S, _, _, G, Opts).
% A graph is given: display triples.
rdf_html_describe(S, G, Opts) -->
  rdf_html_triple(S, _, _, G, Opts).



%! rdf_html_quadruple(+Quadruple:compound)// is det.
% Wrapper around rdf_html_quadruple//2 with default options.

rdf_html_quadruple(Q) -->
  rdf_html_quadruple(Q, []).

%! rdf_html_quadruple(+Quadruple:compound, +Options:list(compound))// is det.

rdf_html_quadruple(rdf(S,P,O,G), Opts) -->
  rdf_html_statement(S, P, O, G, Opts).

%! rdf_html_quadruple(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   ?Graph:atom
%! )// is det.
% Wrapper around rdf_html_quadruples//5 with default options.

rdf_html_quadruple(S, P, O, G) -->
  rdf_html_quadruple(S, P, O, G, []).

%! rdf_html_quadruple(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   ?Graph:atom,
%!   +Options:list(compound)
%! )// is det.
% The following options are supported:
%   * abbr_iri(+boolean)
%   * abbr_list(+boolean)
%   * ellip_lit(+or([nonneg,oneof([inf])]))
%   * language_priority_list(+list(atom))
%   * symbol_iri(+boolean)
%   * page_size(+nonneg)
%   * style(+oneof([tuple,turtle])

% Ground quadruples are printed without them having to be present
% in the RDF DB.
rdf_html_quadruple(S, P, O, G, Opts) -->
  {ground(rdf(S,P,O,G))}, !,
  rdf_html_statement(S, P, O, G, Opts).
% Non-ground quadruples are non-deterministically matched
% against the RDF DB.
rdf_html_quadruple(S, P, O, G, Opts) -->
  {option(page_size(N), Opts, 10),
   findnsols(N, rdf(S,P,O,G), user:rdf(S, P, O, G), Ts)},
  'rdf_html_quadruple*'(Ts, Opts).



%! rdf_html_quadruples(+Quadruples:list(compound))// is det.
% Wrapper around rdf_html_quadruples//2 with default options.

rdf_html_quadruples(Qs) -->
  rdf_html_quadruples(Qs, []).

%! rdf_html_quadruples(
%!   +Quadruples:list(compound),
%!   +Options:list(compound)
%! )// is det.

rdf_html_quadruples(Qs, Opts0) -->
  {merge_options([abbr_list(false)], Opts0, Opts)},
  'rdf_html_quadruple*'(Qs, Opts).

'rdf_html_quadruple*'([], _) --> !, html([]).
'rdf_html_quadruple*'([H|T], Opts) -->
  rdf_html_quadruple(H, Opts),
  'rdf_html_quadruple*'(T, Opts).



%! rdf_html_statement(+Statement:compound)// is det.
% Wrapper around rdf_html_statement//2 with default options.

rdf_html_statement(T) -->
  rdf_html_statement(T, []).

%! rdf_html_statement(+Statement:compound, +Options:list(compound))// is det.

rdf_html_statement(T, Opts0) -->
  {merge_options([abbr_list(false)], Opts0, Opts)},
  (   % Statement is a triple.
      {T = rdf(_,_,_)}
  ->  rdf_html_triple(T, Opts)
  ;   % Statement is a quadruple.
      {T = rdf(_,_,_,_)}
  ->  rdf_html_quadruple(T, Opts)
  ).



%! rdf_html_statement(
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
%   * language_priority_list(+list(atom))
%   * page_size(+nonneg)
%   * style(+oneof([tuple,turtle])
%     The style that is used for printing the statement.
%     Style `turtle` is appropriate for enumerating multiple triples.
%     Style `tuple` is appropriate for singular triples.
%     Default is `tuple`.
%   * symbol_iri(+boolean)

rdf_html_statement(S, P, O, G, Opts) -->
  {option(style(turtle), Opts)}, !,
  html(
    span(class=statement, [
      \rdf_html_subject(S, Opts),
      ' ',
      \rdf_html_predicate(P, Opts),
      ' ',
      \rdf_html_object(O, Opts),
      \rdf_html_graph_maybe(G, Opts),
      ' .'
    ])
  ).
rdf_html_statement(S, P, O, G, Opts) -->
  html(
    span(class=statement, [
      &(lang),
      \rdf_html_subject(S, Opts),
      ', ',
      \rdf_html_predicate(P, Opts),
      ', ',
      \rdf_html_object(O, Opts),
      \rdf_html_graph_maybe(G, Opts),
      &(rang)
    ])
  ).



%! rdf_html_statements(+Statements:list(compound))// is det.
% Wrapper around rdf_html_statements//2 with default options.

rdf_html_statements(Ss) -->
  rdf_html_statement(Ss, []).


%! rdf_html_statements(
%!   +Statements:list(compound),
%!   +Options:list(compound)
%! )// is det.

rdf_html_statements(Ss, Opts0) -->
  {merge_options([abbr_list(false)], Opts0, Opts)},
  'rdf_html_statement*'(Ss, Opts).

'rdf_html_statement*'([], _) --> !, html([]).
'rdf_html_statement*'([H|T], Opts) -->
  rdf_html_statement(H, Opts),
  'rdf_html_statement*'(T, Opts).



%! rdf_html_triple(+Triple:compound)// is det.
% Wrapper around rdf_html_triple//2 with default options.

rdf_html_triple(T) -->
  rdf_html_triple(T, []).


%! rdf_html_triple(+Triple:compound, +Options:list(compound))// is det.

rdf_html_triple(rdf(S,P,O), Opts) -->
  rdf_html_statement(S, P, O, _, Opts).


%! rdf_html_triple(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   +Options:list(compound)
%! )// is nondet.
% Wrapper around rdf_html_triple//5 with default options.

rdf_html_triple(S, P, O, G) -->
  rdf_html_triple(S, P, O, G, []).


%! rdf_html_triple(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   ?Graph:atom,
%!   +Options:list(compound)
%! )// is det.
% The following options are supported:
%   * abbr_iri(+boolean)
%   * abbr_list(+boolean)
%   * ellip_lit(+or([nonneg,oneof([inf])]))
%   * label_iri(+boolean)
%   * language_priority_list(+list(atom))
%   * page_size(+nonneg)
%   * style(+oneof([tuple,turtle])
%   * symbol_iri(+boolean)

% Ground triples are printing without them having to be present
% in the RDF DB.
rdf_html_triple(S, P, O, G, Opts) -->
  {ground(rdf(S,P,O))}, !,
  rdf_html_statement(S, P, O, G, Opts).
% Non-ground triples are non-deterministically matched
% against the RDF DB.
rdf_html_triple(S, P, O, G, Opts) -->
  {option(page_size(N), Opts, 10),
   findnsols(N, rdf(S,P,O), user:rdf(S, P, O, G), Ts)},
  rdf_html_triples(Ts, Opts).



%! rdf_html_triples(+Triples:list(compound))// is det.
% Wrapper around rdf_html_triples//2 with default options.

rdf_html_triples(Ts) -->
  rdf_html_triples(Ts, []).


%! rdf_html_triples(+Triples:list(compound), +Options:list(compound))// is det.

rdf_html_triples(Ts, Opts0) -->
  {merge_options([abbr_list(false)], Opts0, Opts)},
  'rdf_html_statement*'(Ts, Opts).





% HELPERS %

%! rdf_html_graph_maybe(+Graph:atom, +Options:list(compound))// is det.

rdf_html_graph_maybe(G, Opts) -->
  {ground(G)}, !,
  (   {option(style(turtle), Opts)}
  ->  html([' ',span(class=graph, G)])
  ;   html(['@',span(class=graph, G)])
  ).
rdf_html_graph_maybe(_, _) --> html([]).
