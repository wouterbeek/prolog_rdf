:- module(
  rdf_html_term,
  [
    rdf_html_bnode//2,		% +B, +Opts
    rdf_html_datatype//2,	% +D, +Opts
    rdf_html_graph//2,		% +G, +Opts
    rdf_html_iri//2,		% +Iri, +Opts
    rdf_html_language_tag//2,	% +LTag, +Opts
    rdf_html_lexical_form//2,	% +Lex, +Opts
    rdf_html_list//2,		% +T, +Opts
    rdf_html_literal//2,	% +Lit, +Opts
    rdf_html_object//2,		% +O, +Opts
    rdf_html_predicate//2,	% +P, +Opts
    rdf_html_subject//2,	% +S, +Opts
    rdf_html_term//1,		% +T
    rdf_html_term//2,		% +T, +Opts
    rdf_html_term_in_graph//3	% +T, ?G, +Opts
  ]
).

/** <module> RDF HTML term

Generates HTML representations of RDF data.

@author Wouter Beek
@version 2015/08-2015/09, 2015/12-2016/02
*/

:- use_module(library(atom_ext)).
:- use_module(library(html/html_collection)).
:- use_module(library(html/html_link)).
:- use_module(library(html/html_pl)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(option)).
:- use_module(library(rdf/rdf_api)).
:- use_module(library(rdf/rdf_bnode_name)).
:- use_module(library(rdf11/rdf11)).
:- use_module(library(typecheck)).
:- use_module(library(yall)).

:- rdf_meta
   html_entry(r, -),
   rdf_html_datatype(r, +, ?, ?),
   rdf_html_graph(r, +, ?, ?),
   rdf_html_iri(r, +, ?, ?),
   rdf_html_language_tag(+, +, ?, ?),
   rdf_html_lexical_form(+, +, ?, ?),
   rdf_html_list(o, +, ?, ?),
   rdf_html_literal(o, +, ?, ?),
   rdf_html_object(o, +, ?, ?),
   rdf_html_predicate(r, +, ?, ?),
   rdf_html_subject(r, +, ?, ?),
   rdf_html_term(o, ?, ?),
   rdf_html_term(o, +, ?, ?),
   rdf_html_term_in_graph(o, ?, +, ?, ?).

:- predicate_options(rdf_html_bnode//2, 2, [
     abbr_list(+boolean),
     pass_to(rdf_html_list//2)
   ]).
:- predicate_options(rdf_html_datatype//2, 2, [
     pass_to(rdf_html_iri//2, 2)
   ]).
:- predicate_options(rdf_html_graph//2, 2, [
     pass_to(html_link//3, 3)
   ]).
:- predicate_options(rdf_html_iri//2, 2, [
     abbr_iri(+boolean),
     abbr_list(+boolean),
     ellip_ln(+or([nonneg,oneof([inf])])),
     label_iri(+boolean),
     language_priority_list(+list(atom)),
     symbol_iri(+boolean),
     pass_to(rdf_html_list//2, 2)
   ]).
:- predicate_options(rdf_html_language_tag//2, 2, [
     pass_to(rdf_html_language_subtags//2, 2)
   ]).
:- predicate_options(rdf_html_lexical_form//2, 2, [
     ellip_lit(+or([nonneg,oneof([inf])]))
   ]).
:- predicate_options(rdf_html_list//2, 2, [
     pass_to(rdf_html_term//2, 2)
   ]).
:- predicate_options(rdf_html_literal//2, 2, [
     location(+atom),
     pass_to(rdf_html_datatype//2, 2),
     pass_to(rdf_html_language_tag//2, 2),
     pass_to(rdf_html_lexical_form//2, 2)
   ]).
:- predicate_options(rdf_html_object//2, 2, [
     pass_to(rdf_html_term//2, 2)
   ]).
:- predicate_options(rdf_html_predicate//2, 2, [
     pass_to(rdf_html_iri//2, 2)
   ]).
:- predicate_options(rdf_html_subject//2, 2, [
     pass_to(rdf_html_term//2, 2)
   ]).
:- predicate_options(rdf_html_term//2, 2, [
     pass_to(rdf_html_bnode//2, 2),
     pass_to(rdf_html_iri//2, 2),
     pass_to(rdf_html_literal//2, 2)
   ]).
:- predicate_options(rdf_html_term_in_graph//3, 3, [
     pass_to(rdf_html_graph//2, 2),
     pass_to(rdf_html_term//2, 2)
   ]).





%! rdf_html_bnode(+B, +Opts)// is det.
% The following options are supported:
%   * abbr_list(+boolean)
%     Whether RDF lists are shown in Prolog list notation.
%     Default is `true` for statements in the RDF DB
%     but `false` for RDF compound terms (since in the latter case
%     we cannot check whether something is an RDF list or not).

rdf_html_bnode(B, Opts) -->
  {option(abbr_list(true), Opts)},
  rdf_html_list(B, Opts), !.
rdf_html_bnode(B, Opts) -->
  {rdf_bnode_name(B, BName)},
  html(span(class='rdf-bnode',
    \html_link(BName, html([BName]), [query(bnode)|Opts]))
  ).



%! rdf_html_datatype(+Datatype:iri, +Opts)// is det.

rdf_html_datatype(D, Opts) -->
  html(span(class='datatype-iri', \rdf_html_iri(D, Opts))).



%! rdf_html_graph(+G, +Opts)// is det.

rdf_html_graph(G, _) --> {var(G)}, !, [].
rdf_html_graph(G, Opts) -->
  html(
    span(class='rdf-graph',
      \html_link(G, rdf_html_graph0(G, Opts), [query(graph)|Opts])
    )
  ).

rdf_html_graph0(G, Opts) -->
  html_global_iri(G, Opts), !.
rdf_html_graph0(G, Opts) -->
  html_pl_term(G, Opts).



%! rdf_html_iri(+Iri:atom, +Opts)// is det.
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
%   * language_priority_list(+list(atom))
%     Default is `en-US`.
%   * symbol_iri(+boolean)
%     Whether logic symbols should be used i.o. IRIs.
%     Default is `true`.

rdf_html_iri(Iri, Opts) -->
  {option(abbr_list(true), Opts)},
  rdf_html_list(Iri, Opts), !.
rdf_html_iri(Iri, Opts) -->
  {
    option(symbol_iri(true), Opts),
    html_entry(Iri, Name),
    ClassT = [iri,'symbol-iri']
  },
  html(span(class=[Name|ClassT], &(Name))).

rdf_html_iri(Global, Opts) -->
  {
    option(label_iri(true), Opts),
    option(lang_priority_list(LRanges), Opts, ['en-US']),
    once(rdfs_label(Global, LRanges, LTag, Lbl))
  }, !,
  (   {var(LTag)}
  ->  html(span(class=[iri,'label-iri'], Lbl))
  ;   html(span([class=[iri,'label-iri'],lang=LTag], Lbl))
  ).
rdf_html_iri(Global, Opts) -->
  {rdf_is_iri(Global)}, !,
  html_link(Global, html_global_iri(Global, Opts), [query(iri)|Opts]).



%! rdf_html_language_tag(+LanguageTag:atom, +Opts)// is det.
% Options are passed to rdf_html_language_subtags//2.

rdf_html_language_tag(LTag, Opts) -->
  {atomic_list_concat(Subtags, -, LTag)},
  html(span(class='language-tag', \rdf_html_language_subtags(Subtags, Opts))).

rdf_html_language_subtags([], _) --> !, html([]).
rdf_html_language_subtags([H|T], Opts) -->
  html(span(class='language-subtag', H)),
  rdf_html_language_subtags(T, Opts).



%! rdf_html_lexical_form(+Value, +Opts)// is det.
% The following options are supported:
%   * ellip_lit(+or([nonneg,oneof([inf])]))
%     Elipses lexical forms so that they are assured to be at most
%     the given number of characters in length.
%     Default is `20` to ensure that every triple fits within
%     an 80 character wide terminal.

rdf_html_lexical_form(V, Opts) -->
  {
    rdf_lexical_form(V, Lex),
    option(ellip_lit(N), Opts, 20),
    atom_truncate(Lex, N, Lex0)
  },
  html(span(class='lexical-form', ['"',Lex0,'"'])).



%! rdf_html_list(+List:rdf_term, +Opts)// is semidet.
% The following options are supported:
%   * abbr_list(+boolean)
%     Whether or not RDF lists are displayed using Prolog list notation.
%     Default is `true`.

rdf_html_list(L1, Opts) -->
  {rdf_list(L1, L2)},
  html(class='rdf-list', \list([T]>>rdf_html_term(T, Opts), L2)).



%! rdf_html_literal(+Literal:comound, +Opts)// is det.
% The following options are supported:
%   * abbr_iri(+boolean)
%   * ellip_lit(+or([nonneg,oneof([inf])]))
%   * ellip_ln(+or([nonneg,oneof([inf])]))
%   * language_priority_list(+list(atom))
%   * location(+atom)
%   * symbol_iri(+boolean)

rdf_html_literal(Lit, Opts) -->
  {option(location(_), Opts)}, !,
  html_link(Lit, rdf_html_literal0(Lit, Opts), [query(literal)|Opts]).
rdf_html_literal(Lit, Opts) -->
  rdf_html_literal0(Lit, Opts).

rdf_html_literal0(V^^D, Opts) --> !,
  html(
    span(class='rdf-literal', [
      &(lang),
      \rdf_html_datatype(D, Opts),
      ',',
      \rdf_html_lexical_form(V, Opts),
      &(rang)
    ])
  ).
rdf_html_literal0(V@LTag, Opts) --> !,
  {rdf_equal(rdf:langString, D)},
  html(
    span(class=['language-tagged-string','rdf-literal'], [
      &(lang),
      \rdf_html_datatype(D, Opts),
      ',',
      &(lang),
      \rdf_html_language_tag(LTag, Opts),
      ',',
      \rdf_html_lexical_form(V, Opts),
      &(rang),
      &(rang)
    ])
  ).


%! rdf_html_object(+Object:rdf_term, +Opts)// is det.
% The following options are supported:
%   * abbr_iri(+boolean)
%   * abbr_list(+boolean)
%   * label_iri(+boolean)
%   * ellip_lit(+or([nonneg,oneof([inf])]))
%   * ellip_ln(+or([nonneg,oneof([inf])]))

rdf_html_object(O, Opts) -->
  html(span(class='rdf-object', \rdf_html_term(O, Opts))).



%! rdf_html_predicate(+Predicate:iri, +Opts)// is det.
%   * abbr_iri(+boolean)
%   * ellip_ln(+or([nonneg,oneof([inf])]))
%   * label_iri(+boolean)
%   * language_priority_list(+list(atom))
%   * symbol_iri(+boolean)

rdf_html_predicate(P, Opts) -->
  html(span(class='rdf-predicate', \rdf_html_iri(P, Opts))).



%! rdf_html_subject(+Subject:rdf_term, +Opts)// is det.
% The following options are supported:
%   * abbr_iri(+boolean)
%   * abbr_list(+boolean)
%   * ellip_ln(+or([nonneg,oneof([inf])]))
%   * label_iri(+boolean)

rdf_html_subject(S, Opts) -->
  html(span(class='rdf-subject', \rdf_html_term(S, Opts))).



%! rdf_html_term(+T)// is det.
% Wrapper around rdf_html_term//2 with default options.

rdf_html_term(T) -->
  rdf_html_term(T, []).


%! rdf_html_term(+T, +Opts)// is det.
% The following options are supported:
%   * abbr_iri(+boolean)
%   * abbr_list(+boolean)
%   * ellip_lit(+or([nonneg,oneof([inf])]))
%   * ellip_ln(+or([nonneg,oneof([inf])]))
%   * label_iri(+boolean)
%   * language_priority_list(+list(atom))
%   * symbol_iri(+boolean)

rdf_html_term(graph(G), Opts) --> !,
  rdf_html_graph(G, Opts).
rdf_html_term(link(Link,Label), _) --> !,
  html_link(Link, html(Label)).
rdf_html_term(T, Opts) -->
  {rdf_is_literal(T)}, !,
  rdf_html_literal(T, Opts).
rdf_html_term(T, Opts) -->
  {rdf_is_bnode(T)}, !,
  rdf_html_bnode(T, Opts).
rdf_html_term(T, Opts) -->
  rdf_html_iri(T, Opts), !.
rdf_html_term(T, Opts) -->
  html_pl_term(T, Opts).



%! rdf_html_term_in_graph(+T, ?G, +Opts)// is det.

rdf_html_term_in_graph(T, G, Opts) -->
  {var(G)}, !,
  rdf_html_term(T, Opts).
rdf_html_term_in_graph(T, G, Opts) -->
  rdf_html_term(T, Opts),
  rdf_html_graph(G, Opts).





% HELPERS %

html_compact_iri(Global, Opts) -->
  {
    \+ option(abbr_iri(false), Opts),
    rdf_global_id(Prefix:Local, Global)
  }, !,
  {
    option(ellip_ln(N), Opts, 20),
    atom_truncate(Local, N, Local0)
  },
  html([
    span(class='iri-prefix', Prefix),
    ':',
    span(class='iri-local',Local0)
  ]).



html_entry(owl:equivalentClass, equiv).
html_entry(owl:sameAs,          equiv).
html_entry(rdf:type,            isin).
html_entry(rdfs:subClassOf,     sube).



html_full_iri(Global, _) -->
  html(span(class=iri, Global)).



html_global_iri(Global, Opts) -->
  html_compact_iri(Global, Opts), !.
html_global_iri(Global, Opts) -->
  html_full_iri(Global, Opts).
