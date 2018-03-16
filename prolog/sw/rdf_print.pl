:- encoding(utf8).
:- module(
  rdf_print,
  [
    rdf_dcg_term//1, % +Term
    rdf_dcg_term//2  % +Term, +Options
  ]
).

/** <module> RDF printing

@author Wouter Beek
@version 2018
*/

:- use_module(library(lists)).

:- use_module(library(dcg)).
:- use_module(library(dict)).
:- use_module(library(sw/rdf_prefix)).
:- use_module(library(sw/rdf_term)).





%! rdf_dcg_iri(+Iri:rdf_iri, +Options:dict)// is det.

rdf_dcg_iri(Iri, Options) -->
  {
    dict_get(iri_abbr, Options, true, true),
    (   dict_get(prefix_map, Options, Prefix2Alias)
    ->  % Abbreviated based on the prefix map specified in options.
        (   gen_assoc(Prefix, Prefix2Alias, Alias),
            atom_prefix(Iri, Prefix)
        ->  atom_concat(Prefix, Local, Iri)
        )
    ;   % Abbreviated based on the global prefix declarations.
        rdf_global_id(Alias:Local, Iri)
    ), !,
    atom_length(Alias, AliasLength),
    Minus is AliasLength + 1,
    dict_get(max_iri_len, Options, ∞, Length),
    inf_minus(Length, Minus, Max)
  },
  atom(Alias),
  ":",
  ellipsis(Local, Max).
rdf_dcg_iri(Iri, Options) -->
  {dict_get(max_iri_len, Options, ∞, Length)},
  "<",
  ellipsis(Iri, Length),
  ">".

inf_minus(∞, _, ∞) :- !.
inf_minus(X, Y, X) :-
  X =< Y, !.
inf_minus(X, Y, Z) :-
  Z is X - Y.



%! rdf_dcg_lexical_form(+Lex:atom, +Options:dict)// is det.

rdf_dcg_lexical_form(Lex, Options) -->
  {dict_get(max_lit_len, Options, ∞, Len)},
  "\"",
  ellipsis(Lex, Len),
  "\"".



%! rdf_dcg_literal(+Literal:rdf_literal, +Options:dict)// is det.

% language-tagged string
rdf_dcg_literal(literal(lang(LTag,Lex)), Options) --> !,
  rdf_dcg_lexical_form(Lex, Options),
  "@",
  atom(LTag).
% typed literal
rdf_dcg_literal(literal(type(D,Lex)), Options) -->
  rdf_dcg_lexical_form(Lex, Options),
  "^^",
  rdf_dcg_iri(D, Options).



%! rdf_dcg_term(+Term:rdf_term)// is det.
%! rdf_dcg_term(+Term:rdf_term, +Options:dict)// is det.

rdf_dcg_term(Term) -->
  rdf_dcg_term(Term, _{}).


rdf_dcg_term(BNode, _) -->
  {rdf_is_bnode(BNode)}, !,
  atom(BNode).
rdf_dcg_term(Iri, Options) -->
  {rdf_is_iri(Iri)}, !,
  rdf_dcg_iri(Iri, Options).
rdf_dcg_term(Lit, Options) -->
  rdf_dcg_literal(Lit, Options), !.
