:- encoding(utf8).
:- module(
  rdf_print,
  [
    rdf_dcg_generalized_triple_pattern//1, % +TriplePattern
    rdf_dcg_generalized_triple_pattern//2, % +TriplePattern, +Options
    rdf_dcg_generalized_triple_pattern//3, % ?S, ?P, ?O
    rdf_dcg_generalized_triple_pattern//4, % ?S, ?P, ?O, +Options
    rdf_dcg_term//1,                       % +Term
    rdf_dcg_term//2,                       % +Term, +Options
    rdf_dcg_term_or_var//1,                % ?Term
    rdf_dcg_term_or_var//2,                % ?Term, +Options
    rdf_dcg_triple//1,                     % +Triple
    rdf_dcg_triple//2,                     % +Triple, +Options
    rdf_dcg_triple//3,                     % +S, +P, +O
    rdf_dcg_triple//4,                     % +S, +P, +O, +Options
    rdf_dcg_triple_pattern//1,             % +TriplePattern
    rdf_dcg_triple_pattern//2,             % +TriplePattern, +Options
    rdf_dcg_triple_pattern//3,             % ?S, ?P, ?O
    rdf_dcg_triple_pattern//4              % ?S, ?P, ?O, +Options
  ]
).

/** <module> RDF printing

@author Wouter Beek
@version 2018
*/

:- use_module(library(error)).
:- use_module(library(lists)).

:- use_module(library(dcg)).
:- use_module(library(dict)).
:- use_module(library(semweb/rdf_prefix)).
:- use_module(library(semweb/rdf_term)).

:- rdf_meta
   rdf_dcg_generalized_triple_pattern(t, ?, ?),
   rdf_dcg_generalized_triple_pattern(t, +, ?, ?),
   rdf_dcg_generalized_triple_pattern(r, r, o, ?, ?),
   rdf_dcg_generalized_triple_pattern(r, r, o, +, ?, ?),
   rdf_dcg_term(o, ?, ?),
   rdf_dcg_term(o, +, ?, ?),
   rdf_dcg_term_or_var(o, ?, ?),
   rdf_dcg_term_or_var(o, +, ?, ?),
   rdf_dcg_triple(t, ?, ?),
   rdf_dcg_triple(t, +, ?, ?),
   rdf_dcg_triple(r, r, o, ?, ?),
   rdf_dcg_triple(r, r, o, +, ?, ?),
   rdf_dcg_triple_pattern(t, ?, ?),
   rdf_dcg_triple_pattern(t, +, ?, ?),
   rdf_dcg_triple_pattern(r, r, o, ?, ?),
   rdf_dcg_triple_pattern(r, r, o, +, ?, ?).





%! rdf_dcg_generalized_triple_pattern(+TriplePattern:rdf_triple_pattern)// is det.
%! rdf_dcg_generalized_triple_pattern(+TriplePattern:rdf_triple_pattern, +Options:dict)// is det.
%! rdf_dcg_generalized_triple_pattern(?S:rdf_nonliteral, ?P:iri, ?O:rdf_term)// is det.
%! rdf_dcg_generalized_triple_pattern(?S:rdf_nonliteral, ?P:iri, ?O:rdf_term, +Options:dict)// is det.

rdf_dcg_generalized_triple_pattern(TriplePattern) -->
  rdf_dcg_generalized_triple_pattern(TriplePattern, _{}).


rdf_dcg_generalized_triple_pattern(rdf(S,P,O), Options) -->
  rdf_dcg_generalized_triple_pattern(S, P, O, Options).


rdf_dcg_generalized_triple_pattern(S, P, O) -->
  rdf_dcg_generalized_triple_pattern(S, P, O, _{}).


rdf_dcg_generalized_triple_pattern(S, P, O, Options) -->
  "〈",
  rdf_dcg_term_or_var(S, Options),
  ", ",
  rdf_dcg_term_or_var(P, Options),
  ", ",
  rdf_dcg_term_or_var(O, Options),
  "〉".



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
        rdf_prefix_iri(Alias:Local, Iri)
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



%! rdf_dcg_iri_or_var(?Iri:rdf_iri, +Options:dict)// is det.

rdf_dcg_iri_or_var(NonGround, _) -->
  {\+ ground(NonGround)}, !,
  term(NonGround).
rdf_dcg_iri_or_var(Iri, Options) -->
  rdf_dcg_iri(Iri, Options).



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
rdf_dcg_literal(literal(type(D,Lex)), Options) --> !,
  rdf_dcg_lexical_form(Lex, Options),
  "^^",
  rdf_dcg_iri(D, Options).
rdf_dcg_literal(Term, _) -->
  {type_error(rdf_literal, Term)}.



%! rdf_dcg_nonliteral(+Term:rdf_nonliteral, +Options:dict)// is det.

rdf_dcg_nonliteral(BNode, _) -->
  {rdf_is_bnode(BNode)}, !,
  atom(BNode).
rdf_dcg_nonliteral(Iri, Options) -->
  {rdf_is_iri(Iri)}, !,
  rdf_dcg_iri(Iri, Options).
rdf_dcg_nonliteral(Term, _) -->
  {type_error(rdf_nonliteral, Term)}.



%! rdf_dcg_nonliteral_or_var(?Term:rdf_nonliteral, +Options:dict)// is det.

rdf_dcg_nonliteral_or_var(NonGround, _) -->
  {\+ ground(NonGround)}, !,
  term(NonGround).
rdf_dcg_nonliteral_or_var(Term, Options) -->
  rdf_dcg_nonliteral(Term, Options).




%! rdf_dcg_term(+Term:rdf_term)// is det.
%! rdf_dcg_term(+Term:rdf_term, +Options:dict)// is det.

rdf_dcg_term(Term) -->
  rdf_dcg_term(Term, _{}).


rdf_dcg_term(Lit, Options) -->
  {rdf_is_literal(Lit)}, !,
  rdf_dcg_literal(Lit, Options).
rdf_dcg_term(Term, Options) -->
  rdf_dcg_nonliteral(Term, Options).



%! rdf_dcg_term_or_var(?Term:rdf_term, +Options:dict)// is det.
%! rdf_dcg_term_or_var(?Term:rdf_term, +Options:dict)// is det.

rdf_dcg_term_or_var(Term) -->
  rdf_dcg_term_or_var(Term, _{}).


rdf_dcg_term_or_var(NonGround, _) -->
  {\+ ground(NonGround)}, !,
  term(NonGround).
rdf_dcg_term_or_var(Term, Options) -->
  rdf_dcg_term(Term, Options).



%! rdf_dcg_triple(+Triple:rdf_triple)// is det.
%! rdf_dcg_triple(+Triple:rdf_triple, +Options:dict)// is det.
%! rdf_dcg_triple(+S:rdf_nonliteral, +P:iri, +O:rdf_term)// is det.
%! rdf_dcg_triple(+S:rdf_nonliteral, +P:iri, +O:rdf_term, +Options:dict)// is det.

rdf_dcg_triple(Triple) -->
  rdf_dcg_triple(Triple, _{}).


rdf_dcg_triple(rdf(S,P,O), Options) -->
  rdf_dcg_triple(S, P, O, Options).


rdf_dcg_triple(S, P, O) -->
  rdf_dcg_triple(S, P, O, _{}).


rdf_dcg_triple(S, P, O, Options) -->
  "〈",
  rdf_dcg_nonliteral(S, Options),
  ", ",
  rdf_dcg_iri(P, Options),
  ", ",
  rdf_dcg_term(O, Options),
  "〉".



%! rdf_dcg_triple_pattern(+TriplePattern:rdf_triple_pattern)// is det.
%! rdf_dcg_triple_pattern(+TriplePattern:rdf_triple_pattern, +Options:dict)// is det.
%! rdf_dcg_triple_pattern(?S:rdf_nonliteral, ?P:iri, ?O:rdf_term)// is det.
%! rdf_dcg_triple_pattern(?S:rdf_nonliteral, ?P:iri, ?O:rdf_term, +Options:dict)// is det.

rdf_dcg_triple_pattern(TriplePattern) -->
  rdf_dcg_triple_pattern(TriplePattern, _{}).


rdf_dcg_triple_pattern(rdf(S,P,O), Options) -->
  rdf_dcg_triple_pattern(S, P, O, Options).


rdf_dcg_triple_pattern(S, P, O) -->
  rdf_dcg_triple_pattern(S, P, O, _{}).


rdf_dcg_triple_pattern(S, P, O, Options) -->
  "〈",
  rdf_dcg_nonliteral_or_var(S, Options),
  ", ",
  rdf_dcg_iri_or_var(P, Options),
  ", ",
  rdf_dcg_term_or_var(O, Options),
  "〉".
