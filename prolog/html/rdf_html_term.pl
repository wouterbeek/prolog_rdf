:- module(
  rdf_html_term,
  [
    rdf_html_describe//1, % +Subject
    rdf_html_describe//2, % +Subject, +Options
    rdf_html_describe//3, % +Subject:rdf_term
                          % ?Graph:atom
                          % +Options:list(compound)
    rdf_html_graph//1, % ?Graph
    rdf_html_graph//2, % ?Graph:atom
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

/** <module> RDF HTML term

Generates HTML representations of RDF data.

---

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(atom_ext)).
:- use_module(library(error)).
:- use_module(library(html/content/html_collection)).
:- use_module(library(html/content/html_symbol)).
:- use_module(library(http/html_write)).
:- use_module(library(lambda)).
:- use_module(library(option)).
:- use_module(library(owl/id_store)).
:- use_module(library(rdf/rdf_bnode_name)).
:- use_module(library(rdf/rdf_graph)).
:- use_module(library(rdf/rdf_list)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_read)).
:- use_module(library(rdfs/rdfs_read)).
:- use_module(library(semweb/rdf_db)).

:- set_prolog_flag(toplevel_print_anon, false).

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

:- predicate_options(rdf_html_bnode//2, 2, [
     abbr_list(+boolean),
     pass_to(rdf_html_list//2)
   ]).
:- predicate_options(rdf_html_datatype//2, 2, [
     pass_to(rdf_html_iri//2, 2)
   ]).
:- predicate_options(rdf_html_describe//2, 2, [
     pass_to(rdf_html_describe//3, 3)
   ]).
:- predicate_options(rdf_html_describe//3, 3, [
     pass_to(rdf_html_statement//5, 5)
   ]).
:- predicate_options(rdf_html_graph//2, 2, [
     pass_to(rdf_html_quadruple//5, 5),
     pass_to(rdf_html_triple//5, 5)
   ]).
:- predicate_options(rdf_html_graph_maybe//2, 2, [
     style(+oneof([tuple,turtle]))
   ]).
:- predicate_options(rdf_html_iri//2, 2, [
     abbr_iri(+boolean),
     abbr_list(+boolean),
     ellip_ln(+or([nonneg,oneof([inf])])),
     label_iri(+boolean),
     lang_pref(+atom),
     symbol_iri(+boolean),
     pass_to(rdf_html_list//2, 2)
   ]).
:- predicate_options(rdf_html_language_tag//2, 2, [
     pass_to(rdf_html_language_subtags//2, 2)
   ]).
:- predicate_options(rdf_html_lexical//2, 2, [
     ellip_lit(+or([nonneg,oneof([inf])]))
   ]).
:- predicate_options(rdf_html_list//2, 2, [
     pass_to(rdf_html_term//2, 2)
   ]).
:- predicate_options(rdf_html_literal//2, 2, [
     style(+oneof([tuple,turtle])),
     pass_to(rdf_html_datatype//2, 2),
     pass_to(rdf_html_language_tag//2, 2),
     pass_to(rdf_html_lexical//2, 2)
   ]).
:- predicate_options(rdf_html_object//2, 2, [
     pass_to(rdf_html_term//2, 2)
   ]).
:- predicate_options(rdf_html_predicate//2, 2, [
     pass_to(rdf_html_iri//2, 2)
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
:- predicate_options(rdf_html_subject//2, 2, [
     pass_to(rdf_html_term//2, 2)
   ]).
:- predicate_options(rdf_html_term//2, 2, [
     pass_to(rdf_html_bnode//2, 2),
     pass_to(rdf_html_iri//2, 2),
     pass_to(rdf_html_literal//2, 2)
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





%! rdf_html_bnode(+BNode:bnode, +Options:list(compound))// is det.
% The following options are supported:
%   * abbr_list(+boolean)
%     Whether RDF lists are shown in Prolog list notation.
%     Default is `true` for statements in the RDF DB
%     but `false` for RDF compound terms (since in the latter case
%     we cannot check whether something is an RDF list or not).

rdf_html_bnode(B, Opts) -->
  {option(abbr_list(true), Opts)},
  rdf_html_list(B, Opts), !.
rdf_html_bnode(B, _) -->
  {rdf_bnode_name(B, BName)},
  html(span(class='blank-node', BName)).



%! rdf_html_datatype(+Datatype:iri, +Options:list(compound))// is det.

rdf_html_datatype(D, Opts) -->
  html(span(class=datatype, \rdf_html_iri(D, Opts))).



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
%   * lang_pref(+atom)
%   * symbol_iri(+boolean)
%   * style(+oneof([tuple,turtle])

% No graph is given: display quadruples.
rdf_html_describe(S, G, Opts) -->
  {var(G)}, !,
  rdf_html_quadruple(S, _, _, G, Opts).
% A graph is given: display triples.
rdf_html_describe(S, G, Opts) -->
  rdf_html_triple(S, _, _, G, Opts).



%! rdf_html_graph(?Graph:atom)// is det.
% Wrapper around rdf_html_graph//2 with default options.

rdf_html_graph(G) -->
  rdf_html_graph(G, []).

%! rdf_html_graph(?Graph:atom, +Options:list(compound))// is det.
% The following options are supported:
%   * abbr_iri(+boolean)
%   * abbr_list(+boolean)
%   * ellip_lit(+or([nonneg,oneof([inf])]))
%   * lang_pref(+atom)
%   * page_size(+nonneg)
%   * symbol_iri(+boolean)
%   * style(+oneof([tuple,turtle])
%
% @throws existence_error

rdf_html_graph(G, Opts) -->
  {var(G)}, !,
  rdf_html_quadruple(_, _, _, _, Opts).
rdf_html_graph(G, Opts) -->
  {rdf_is_graph(G)}, !,
  rdf_html_triple(_, _, _, G, Opts).
rdf_html_graph(G, _) -->
  {existence_error(rdf_graph, G)}.



%! rdf_html_graph_maybe(+Graph:atom, +Options:list(compound))// is det.

rdf_html_graph_maybe(G, Opts) -->
  {ground(G)}, !,
  (   {option(style(turtle), Opts)}
  ->  html([' ',span(class=graph, G)])
  ;   html(['@',span(class=graph, G)])
  ).
rdf_html_graph_maybe(_, _) --> html([]).



%! rdf_html_iri(+Iri:atom, +Options:list(compound))// is det.
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
%     Ellipses IRI local names so that they are assued to be at most
%     the given number of characters in length.
%     This is only used when `abbr_iri=true`.
%     Default is `20` to ensure that every triple fits within
%     an 80 character wide terminal.
%   * label_iri(+boolean)
%     Whether RDFS labels should be used i.o. IRIs.
%     Default is `false`.
%   * lang_pref(+atom)
%     Default is `en-US`.
%   * symbol_iri(+boolean)
%     Whether logic symbols should be used i.o. IRIs.
%     Default is `true`.

rdf_html_iri(Iri, Opts) -->
  {option(abbr_list(true), Opts)},
  rdf_html_list(Iri, Opts), !.
rdf_html_iri(Iri, Opts) -->
  {option(symbol_iri(true), Opts, true),
   Class = [iri,'symbol-iri']},
  (   {rdf_memberchk(Iri, [owl:equivalentClass,owl:sameAs])}
  ->  html(span(class=[equiv|Class], \equivalence))
  ;   {rdf_global_id(rdfs:subClassOf, Iri)}
  ->  html(span(class=[subclass|Class], \subclass))
  ;   {rdf_global_id(rdf:type, Iri)}
  ->  html(span(class=[in|Class], \set_membership))
  ).
rdf_html_iri(Global, Opts) -->
  {option(label_iri(true), Opts),
   option(lang_pref(Pref), Opts, 'en-US'),
   once(rdfs_label(Global, Pref, Lang, Lbl))}, !,
  (   {var(Lang)}
  ->  html(span(class=[iri,'label-iri'], Lbl))
  ;   html(span([class=[iri,'label-iri'],lang=Lang], Lbl))
  ).
rdf_html_iri(Global, Opts) -->
  {\+ option(abbr_iri(false), Opts),
   rdf_global_id(Prefix:Local, Global), !,
   option(ellip_ln(N), Opts, 20),
   atom_truncate(Local, N, Local0)},
  html(
    span(class=['compact-iri',iri],[
      span(class='iri-prefix',Prefix),
      ':',
      span(class='iri-local',Local0)
    ])
  ).
rdf_html_iri(Global, Opts) -->
  {option(style(turtle), Opts)}, !,
  html(span(class=iri, [&(lang),Global,&(rang)])).
rdf_html_iri(Global, _) -->
  html(span(class=iri, Global)).



%! rdf_html_language_tag(+LanguageTag:atom, +Options:list(compound))// is det.
% Options are passed to rdf_html_language_subtags//2.

rdf_html_language_tag(Lang, Opts) -->
  {atomic_list_concat(Subtags, -, Lang)},
  html(span(class='language-tag', \rdf_html_language_subtags(Subtags, Opts))).

rdf_html_language_subtags([], _) --> !, html([]).
rdf_html_language_subtags([H|T], Opts) -->
  html(span(class='language-subtag', H)),
  rdf_html_language_subtags(T, Opts).



%! rdf_html_lexical(+LexicalForm:atom, +Options:list(compound))// is det.
% The following options are supported:
%   * ellip_lit(+or([nonneg,oneof([inf])]))
%     Elipses lexical forms so that they are assured to be at most
%     the given number of characters in length.
%     Default is `20` to ensure that every triple fits within
%     an 80 character wide terminal.

rdf_html_lexical(Lex, Opts) -->
 {option(ellip_lit(N), Opts, 20),
  atom_truncate(Lex, N, Lex0)},
  html(span(class='lexical-form', ['"',Lex0,'"'])).



%! rdf_html_list(+Term:rdf_term, +Options:list(compound))// is semidet.
% The following options are supported:
%   * abbr_list(+boolean)
%     Whether or not RDF lists are displayed using Prolog list notation.
%     Default is `true`.

rdf_html_list(L0, Opts) -->
  {rdf_is_list(L0),
   rdf_list_raw(L0, L)},
  html(class=list, \list(rdf_html_term0(Opts), L)).
rdf_html_term0(Opts, T) --> rdf_html_term(T, Opts).



%! rdf_html_literal(+Literal:comound, +Options:list(compound))// is det.
% The following options are supported:
%   * abbr_iri(+boolean)
%   * ellip_lit(+or([nonneg,oneof([inf])]))
%   * ellip_ln(+or([nonneg,oneof([inf])]))
%   * lang_pref(+atom)
%   * symbol_iri(+boolean)

rdf_html_literal(literal(type(D,Lex)), Opts) --> !,
  (   {option(style(turtle), Opts)}
  ->  html(
        span(class=literal, [
          \rdf_html_lexical(Lex, Opts),
          "^^",
          \rdf_html_datatype(D, Opts)
        ])
      )
  ;   html(
        span(class=literal, [
          &(lang),
          \rdf_html_datatype(D, Opts),
          ',',
          \rdf_html_lexical(Lex, Opts),
          &(rang)
        ])
      )
  ).
rdf_html_literal(literal(lang(Lang,Lex)), Opts) --> !,
  (   {option(style(turtle), Opts)}
  ->  html(
        span(class=['language-tagged-string',literal], [
          \rdf_html_lexical(Lex, Opts),
          '@',
          \rdf_html_language_tag(Lang, Opts)
        ])
      )
  ;   {rdf_global_id(rdf:langString, D)},
      html(
        span(class=['language-tagged-string',literal], [
          &(lang),
          \rdf_html_datatype(D, Opts),
          ',',
          &(lang),
          \rdf_html_language_tag(Lang, Opts),
          ',',
          \rdf_html_lexical(Lex, Opts),
          &(rang),
          &(rang)
        ])
      )
  ).
rdf_html_literal(literal(Lex), Opts) -->
  {rdf_global_id(xsd:string, D)},
  rdf_html_literal(literal(type(D,Lex)), Opts).



%! rdf_html_object(+Object:rdf_term, +Options:list(compound))// is det.
% The following options are supported:
%   * abbr_iri(+boolean)
%   * abbr_list(+boolean)
%   * label_iri(+boolean)
%   * ellip_lit(+or([nonneg,oneof([inf])]))
%   * ellip_ln(+or([nonneg,oneof([inf])]))

rdf_html_object(O, Opts) -->
  html(span(class=object, \rdf_html_term(O, Opts))).



%! rdf_html_predicate(+Predicate:iri, +Options:list(compound))// is det.
%   * abbr_iri(+boolean)
%   * ellip_ln(+or([nonneg,oneof([inf])]))
%   * label_iri(+boolean)
%   * lang_pref(+atom)
%   * symbol_iri(+boolean)

rdf_html_predicate(P, Opts) -->
  html(span(class=predicate, \rdf_html_iri(P, Opts))).



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
%   * lang_pref(+atom)
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
   findnsols(N, rdf(S,P,O,G), rdf2(S, P, O, G), Ts)},
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
%   * lang_pref(+atom)
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



%! rdf_html_subject(+Subject:rdf_term, +Options:list(compound))// is det.
% The following options are supported:
%   * abbr_iri(+boolean)
%   * abbr_list(+boolean)
%   * ellip_ln(+or([nonneg,oneof([inf])]))
%   * label_iri(+boolean)

rdf_html_subject(S, Opts) -->
  html(span(class=subject, \rdf_html_term(S, Opts))).



%! rdf_html_term(+Term:rdf_term)// is det.
% Wrapper around rdf_html_term//2 with default options.

rdf_html_term(T) -->
  rdf_html_term(T, []).

%! rdf_html_term(+Term:rdf_term, +Options:list(compound))// is det.
% The following options are supported:
%   * abbr_iri(+boolean)
%   * abbr_list(+boolean)
%   * ellip_lit(+or([nonneg,oneof([inf])]))
%   * ellip_ln(+or([nonneg,oneof([inf])]))
%   * label_iri(+boolean)
%   * lang_pref(+atom)
%   * symbol_iri(+boolean)

rdf_html_term(T, Opts) -->
  {rdf_is_literal(T)}, !,
  html(span(class=term, \rdf_html_literal(T, Opts))).
rdf_html_term(T, Opts) -->
  {rdf_is_bnode(T)}, !,
  rdf_html_bnode(T, Opts).
rdf_html_term(T, Opts) -->
  rdf_html_iri(T, Opts).



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
%   * lang_pref(+atom)
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
   findnsols(N, rdf(S,P,O), rdf2(S, P, O, G), Ts)},
  rdf_html_triples(Ts, Opts).



%! rdf_html_triples(+Triples:list(compound))// is det.
% Wrapper around rdf_html_triples//2 with default options.

rdf_html_triples(Ts) -->
  rdf_html_triples(Ts, []).

%! rdf_html_triples(+Triples:list(compound), +Options:list(compound))// is det.

rdf_html_triples(Ts, Opts0) -->
  {merge_options([abbr_list(false)], Opts0, Opts)},
  'rdf_html_statement*'(Ts, Opts).
