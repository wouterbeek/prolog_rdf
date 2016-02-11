:- module(
  rdf_print_term,
  [
    rdf_print_graph//1,		% ?G
    rdf_print_graph//2,		% ?G, +Opts
    rdf_print_literal//1,	% +Lit
    rdf_print_literal//2,	% +Lit, +Opts
    rdf_print_object//1,	% +O
    rdf_print_object//2,	% +O, +Opts
    rdf_print_predicate//1,	% +P
    rdf_print_predicate//2,	% +P, +Opts
    rdf_print_subject//1,	% +S
    rdf_print_subject//2,	% +S, +Opts
    rdf_print_term/1,		% +T
    rdf_print_term/2,		% +T, +Opts
    rdf_print_term//1,		% +T
    rdf_print_term//2		% +T, +Opts
  ]
).

/** <module> RDF print term

@author Wouter Beek
@version 2015/07-2016/01
*/

:- use_module(library(atom_ext)).
:- use_module(library(dcg/dcg_collection)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(dcg/dcg_pl)).
:- use_module(library(option)).
:- use_module(library(rdf/rdf_api)).
:- use_module(library(rdf/rdf_bnode_name)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(typecheck)).
:- use_module(library(yall)).

:- rdf_meta
   rdf_print_graph(r, ?, ?),
   rdf_print_graph(r, +, ?, ?),
   rdf_print_literal(o, ?, ?),
   rdf_print_literal(o, +, ?, ?),
   rdf_print_object(o, +, ?, ?),
   rdf_print_predicate(r, +, ?, ?),
   rdf_print_subject(r, +, ?, ?),
   rdf_print_term(o),
   rdf_print_term(o, +),
   rdf_print_term(o, ?, ?),
   rdf_print_term(o, +, ?, ?),
   rdf_symbol_iri(r).

:- predicate_options(rdf_print_bnode//2, 2, [
     abbr_list(+boolean),
     pass_to(rdf_print_list//2, 2)
   ]).
:- predicate_options(rdf_print_iri//2, 2, [
     abbr_iri(+boolean),
     abbr_list(+boolean),
     ellip_ln(+or([nonneg,oneof([inf])])),
     label_iri(+boolean),
     language_priority_list(+list(atom)),
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
:- predicate_options(rdf_print_term//2, 2, [
     pass_to(rdf_print_bnode//2, 2),
     pass_to(rdf_print_iri//2, 2),
     pass_to(rdf_print_literal//2, 2)
   ]).





%! rdf_print_bnode(+B, +Opts)// is det.
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



%! rdf_print_datatype(+D, +Opts)// is det.
% Options are passed to rdf_print_iri//2.

rdf_print_datatype(D, Opts) -->
  rdf_print_iri(D, Opts).



%! rdf_print_graph(?G)// is det.
%! rdf_print_graph(?G, +Opts)// is det.

rdf_print_graph(G) -->
  rdf_print_graph(G, []).
rdf_print_graph(G, Opts) -->
  "@",
  ({  rdf_global_id(Prefix:Local, G),
      \+ option(abbr_iri(false), Opts)}
  -> {option(ellip_ln(N), Opts, 20),
      atom_truncate(Local, N, Local0)},
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
%   * language_priority_list(+list(atom))
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
    atom_truncate(Lex, N, Lex0)
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
%   * language_priority_list(+list(atom))
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
%   * language_priority_list(+list(atom))
%   * symbol_iri(+boolean)
%   * symbol_iri(+boolean)

rdf_print_literal(Lit) -->
  rdf_print_literal(Lit, []).
rdf_print_literal(V^^D, Opts) --> !,
  {format(atom(Lex), "~w", [V])},
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
%   * language_priority_list(+list(atom))
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
%   * language_priority_list(+list(atom))
%   * symbol_iri(+boolean)

rdf_print_predicate(P) -->
  rdf_print_predicate(P, []).
rdf_print_predicate(P, Opts) -->
  rdf_print_iri(P, Opts).



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
%   * language_priority_list(+list(atom))
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
%   * language_priority_list(+list(atom))
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
  pl_term(T).



%! rdf_symbol_iri(+Iri)// is det.
% Writes a symbolic representation for the given IRI.

rdf_symbol_iri(owl:equivalentClass)    --> "≡".
rdf_symbol_iri(owl:equivalentProperty) --> "≡".
rdf_symbol_iri(owl:sameAs)             --> "≡".
rdf_symbol_iri(rdfs:subClassOf)        --> "⊆".
rdf_symbol_iri(rdf:type)               --> "∈".
