:- module(
  rdf_print_term,
  [
    rdf_print_graph//2, % ?Graph:atom
                        % +Options:list(compound)
    rdf_print_literal//2, % +Literal:comound
                          % +Options:list(compound)
    rdf_print_object//2, % +Object:rdf_term
                         % +Options:list(compound)
    rdf_print_predicate//2, % +Predicate:iri
                            % +Options:list(compound)
    rdf_print_subject//2, % +Subject:rdf_term
                          % +Options:list(compound)
    rdf_print_term/1, % +Term
    rdf_print_term/2, % +Term:rdf_term
                      % +Options:list(compound)
    rdf_print_term//1, % +Term:rdf_term
    rdf_print_term//2 % +Term:rdf_term
                      % +Options:list(compound)
  ]
).

/** <module> RDF term printing

@author Wouter Beek
@license MIT license
@version 2015/07-2015/11
*/

:- use_module(library(atom_ext)).
:- use_module(library(dcg/dcg_collection)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(dcg/dcg_pl)).
:- use_module(library(lambda)).
:- use_module(library(option)).
:- use_module(library(rdf/rdf_bnode_name)).
:- use_module(library(rdf/rdf_list)).
:- use_module(library(rdfs/rdfs_read)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(typecheck)).

:- rdf_meta(rdf_print_graph(r,+,?,?)).
:- rdf_meta(rdf_print_literal(o,+,?,?)).
:- rdf_meta(rdf_print_object(o,+,?,?)).
:- rdf_meta(rdf_print_predicate(r,+,?,?)).
:- rdf_meta(rdf_print_subject(o,+,?,?)).
:- rdf_meta(rdf_print_term(o)).
:- rdf_meta(rdf_print_term(o,+)).
:- rdf_meta(rdf_print_term(o,?,?)).
:- rdf_meta(rdf_print_term(o,+,?,?)).
:- rdf_meta(rdf_symbol_iri(r)).

:- predicate_options(rdf_print_bnode//2, 2, [
     abbr_list(+boolean),
     pass_to(rdf_print_list//2, 2)
   ]).
:- predicate_options(rdf_print_graph//2, 2, [
     style(+oneof([tuple,turtle]))
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
     style(+oneof([tuple,turtle])),
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





%! rdf_print_term(+Term:rdf_term) is det.
% Wrapper around rdf_print_term/2 with default options.

rdf_print_term(T):- rdf_print_term(T, []).


%! rdf_print_term(+Term:rdf_term, +Options:list(compound)) is det.
% The following options are supported:
%   * abbr_iri(+boolean)
%   * abbr_list(+boolean)
%   * ellip_lit(+or([nonneg,oneof([inf])]))
%   * label_iri(+boolean)
%   * language_priority_list(+list(atom))
%   * symbol_iri(+boolean)

rdf_print_term(T, Opts):-
  dcg_with_output_to(current_output, rdf_print_term(T, Opts)).



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



%! rdf_print_datatype(+Datatype:iri, +Options:list(compound))// is det.
% Options are passed to rdf_print_iri//2.

rdf_print_datatype(D, Opts) --> rdf_print_iri(D, Opts).



%! rdf_print_graph(?Graph:atom, +Options:list(compound))// is det.

rdf_print_graph(G, Opts) -->
  ({option(style(turtle), Opts)} -> " " ; "@"),
  ({  rdf_global_id(Prefix:Local, G),
      \+ option(abbr_iri(false), Opts)}
  -> {option(ellip_ln(N), Opts, 20),
      atom_truncate(Local, N, Local0)},
      atom(Prefix),
      ":",
      atom(Local0)
  ;   atom(G)
  ).



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
%   * label_iri(+boolean)
%   * language_priority_list(+list(atom))
%   * symbol_iri(+boolean)
%     Whether logic symbols should be used i.o. IRIs.
%     Default is `true`.

rdf_print_iri(Iri, Opts) -->
  {option(abbr_list(true), Opts)},
  rdf_print_list(Iri, Opts), !.
rdf_print_iri(Iri, Opts) -->
  {option(symbol_iri(true), Opts, true)},
  rdf_symbol_iri(Iri).
rdf_print_iri(Global, Opts) -->
  {
    option(label_iri(true), Opts), !,
    option(language_priority_list(LRanges), Opts, ['en-US']),
    once(rdfs_label(Global, LRanges, _, Lbl))
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
rdf_print_iri(Global, Opts) -->
  {option(style(turtle), Opts)}, !,
  "〈", atom(Global), "〉".
rdf_print_iri(Global, _) -->
  {is_http_iri(Global)},
  atom(Global).



%! rdf_print_language_tag(+LanguageTag:atom, +Options:list(compound))// is det.

rdf_print_language_tag(LTag, _) --> atom(LTag).



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
  "\"", atom(Lex0), "\"".



%! rdf_print_list(+Term:rdf_term, +Options:list(compound))// is semidet.
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

rdf_print_list(L0, Opts) -->
  {rdf_is_list(L0), rdf_list_raw(L0, L)},
  list(\T^rdf_print_term(T, Opts), L).



%! rdf_print_literal(+Literal:comound, +Options:list(compound))// is det.
% The following options are supported:
%   * abbr_iri(+boolean)
%   * ellip_lit(+or([nonneg,oneof([inf])]))
%   * ellip_ln(+or([nonneg,oneof([inf])]))
%   * label_iri(+boolean)
%   * language_priority_list(+list(atom))
%   * symbol_iri(+boolean)
%   * symbol_iri(+boolean)

rdf_print_literal(literal(type(D,Lex)), Opts) --> !,
  (   {option(style(turtle), Opts)}
  ->  rdf_print_lexical(Lex, Opts), "^^", rdf_print_datatype(D, Opts)
  ;   "〈", rdf_print_datatype(D, Opts), ", ", rdf_print_lexical(Lex, Opts), "〉"
  ).
rdf_print_literal(literal(lang(LTag,Lex)), Opts) --> !,
  (   {option(style(turtle), Opts)}
  ->  rdf_print_lexical(Lex, Opts), "@", rdf_print_language_tag(LTag, Opts)
  ;   {rdf_global_id(rdf:langString, D)},
      "〈", rdf_print_datatype(D, Opts), ", ",
      "〈", rdf_print_lexical(Lex, Opts), ", ",
      rdf_print_language_tag(LTag, Opts), "〉", "〉"
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
%   * label_iri(+boolean)
%   * language_priority_list(+list(atom))
%   * symbol_iri(+boolean)

rdf_print_object(O, Opts) --> rdf_print_term(O, Opts).



%! rdf_print_predicate(+Predicate:iri, +Options:list(compound))// is det.
%   * abbr_iri(+boolean)
%   * ellip_ln(+or([nonneg,oneof([inf])]))
%   * label_iri(+boolean)
%   * language_priority_list(+list(atom))
%   * symbol_iri(+boolean)

rdf_print_predicate(P, Opts) --> rdf_print_iri(P, Opts).



%! rdf_print_subject(+Subject:rdf_term, +Options:list(compound))// is det.
% The following options are supported:
%   * abbr_iri(+boolean)
%   * abbr_list(+boolean)
%   * ellip_ln(+or([nonneg,oneof([inf])]))

rdf_print_subject(S, Opts) --> rdf_print_term(S, Opts).



%! rdf_print_term(+Term:rdf_term)// is det.
% Wrapper around rdf_print_term//2 with default options.

rdf_print_term(T) --> rdf_print_term(T, []).


%! rdf_print_term(+Term:rdf_term, +Options:list(compound))// is det.
% The following options are supported:
%   * abbr_iri(+boolean)
%   * abbr_list(+boolean)
%   * ellip_lit(+or([nonneg,oneof([inf])]))
%   * ellip_ln(+or([nonneg,oneof([inf])]))
%   * label_iri(+boolean)
%   * language_priority_list(+list(atom))
%   * symbol_iri(+boolean)

rdf_print_term(T, Opts) --> {rdf_is_literal(T)}, !, rdf_print_literal(T, Opts).
rdf_print_term(T, Opts) --> {rdf_is_bnode(T)},   !, rdf_print_bnode(T, Opts).
rdf_print_term(T, Opts) --> rdf_print_iri(T, Opts), !.
rdf_print_term(T, _)    --> pl_term(T).



%! rdf_symbol_iri(+Iri:iri)// is det.
% Writes a symbolic representation for the given IRI.

rdf_symbol_iri(owl:equivalentClass)    --> "≡".
rdf_symbol_iri(owl:equivalentProperty) --> "≡".
rdf_symbol_iri(owl:sameAs)             --> "≡".
rdf_symbol_iri(rdfs:subClassOf)        --> "⊆".
rdf_symbol_iri(rdf:type)               --> "∈".
