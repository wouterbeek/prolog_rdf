:- module(
  rdf_print,
  [
    rdf_print_graph/1, % +Graph:atom
    rdf_print_quadruple/4, % ?Subject:or([bnode,iri])
                           % ?Predicate:iri
                           % ?Object:rdf_term
                           % ?Graph:atom
    rdf_print_term/1, % +Term
    rdf_print_term/2, % +Term:rdf_term
                      % +Options:list(compound)
    rdf_print_term//1, % +Term
    rdf_print_term//2, % +Term:rdf_term
                       % +Options:list(compound)
    rdf_print_triple/4 % ?Subject:or([bnode,iri])
                       % ?Predicate:iri
                       % ?Object:rdf_term
                       % ?Graph:atom
  ]
).

/** <module> RDF print

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(atom_ext)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/dcg_bracketed)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(dcg/dcg_quoted)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).

:- predicate_options(rdf_print_term/2, 2, [
     pass_to(rdf_print_term//2, 2)
   ]).
:- predicate_options(rdf_print_term//2, 2, [
     pass_to(rdf_print_literal//2, 2)
   ]).
:- predicate_options(rdf_print_literal//2, 2, [
     ellipsis(+nonneg)
   ]).

:- rdf_meta(rdf_print_quadruple(r,r,o,?)).
:- rdf_meta(rdf_print_term(r)).
:- rdf_meta(rdf_print_term(r,+)).
:- rdf_meta(rdf_print_term(r,?,?)).
:- rdf_meta(rdf_print_term(r,+,?,?)).
:- rdf_meta(rdf_print_triple(r,r,o,?)).





%! rdf_print_graph(+Graph:atom) is det.

rdf_print_graph(G):-
  rdf_print_triple(_, _, _, G),
  fail.
rdf_print_graph(_).



%! rdf_print_quadruple(?Subject, ?Predicate, ?Object, ?Graph) is nondet.

rdf_print_quadruple(S, P, O, G):-
  rdf(S, P, O, G),
  rdf_print_statement(S, P, O, G).



%! rdf_print_term(+Term:rdf_term) is det.

rdf_print_term(T):-
  rdf_print_term(T, []).


%! rdf_print_term(+Term:rdf_term, +Options:list(compound)) is det.

rdf_print_term(T, Opts):-
  string_phrase(rdf_print_term(T, Opts), S),
  writeln(S).



%! rdf_print_triple(?Subject, ?Predicate, ?Object, ?Graph) is nondet.

rdf_print_triple(S, P, O, G):-
  rdf(S, P, O, G),
  rdf_print_statement(S, P, O, _).



%! rdf_print_statement(+Subject, +Predicate, +Object, +Graph) is det.

rdf_print_statement(S, P, O, G):-
  string_phrase(rdf_print_statement(S, P, O, G), S),
  writeln(S).

%! rdf_print_statement(+Subject, +Predicate, +Object, +Graph)// is det.

rdf_print_statement(S, P, O, G) -->
  rdf_print_subject(S),
  " ",
  rdf_print_iri(P),
  " ",
  rdf_print_term(O),
  (   {ground(G)}
  ->  rdf_print_graph(G)
  ;   ""
  ),
  " .".



%! rdf_print_bnode(+BNode:bnode)// is det.

rdf_print_bnode(B) -->
  {atom_concat('__bnode', N, B)},
  "_:",
  atom(N).



%! rdf_print_graph(+Graph:atom)// is det.

rdf_print_graph(G) -->
  atom(G).



%! rdf_print_iri(+Iri:atom)// is det.

rdf_print_iri(Global) -->
  {rdf_global_id(Prefix:Local, Global)}, !,
  atom(Prefix),
  ":",
  atom(Local).
rdf_print_iri(Global) -->
  bracketed(angular, atom(Global)).



%! rdf_print_lexical(+LexicalForm:atom, +Options:list(compound))// is det.

rdf_print_lexical(Lex, Opts) -->
  {
    option(elipsis(N), Opts, inf),
    atom_truncate(Lex, N, Lex0)
  },
  quoted(atom(Lex0)).



%! rdf_print_literal(+Literal:comound, +Options:list(compound))// is det.

rdf_print_literal(literal(type(D,Lex)), Opts) --> !,
  rdf_print_lexical(Lex, Opts),
  "^^",
  rdf_print_iri(D).
rdf_print_literal(literal(lang(LangTag,Lex)), Opts) --> !,
  rdf_print_lexical(Lex, Opts),
  "@",
  atom(LangTag).
rdf_print_literal(literal(Lex), Opts) -->
  rdf_print_lexical(Lex, Opts).



%! rdf_print_subject(+Subject:or([bnode,iri]))// is det.

rdf_print_subject(S) -->
  {rdf_is_bnode(S)}, !,
  rdf_print_bnode(S).
rdf_print_subject(S) -->
  rdf_print_iri(S).



%! rdf_print_term(+Term:rdf_term)// is det.

rdf_print_term(T) -->
  rdf_print_term(T, []).


%! rdf_print_term(+Term:rdf_term, +Options:list(compound))// is det.

rdf_print_term(T, Opts) -->
  {rdf_is_literal(T)}, !,
  rdf_print_literal(T, Opts).
rdf_print_term(T, _) -->
  {rdf_is_bnode(T)}, !,
  rdf_print_bnode(T).
rdf_print_term(T, _) -->
  rdf_print_iri(T).
