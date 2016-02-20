:- module(
  rdfa_api,
  [
    rdfa_date_time//1, % +V
    rdfa_has_po//2,    % +S, +P
    rdfa_prefixes/2    % +Aliases:list(atom), -Prefixes:atom
  ]
).

/** <module> RDFa build

@author Wouter Beek
@version 2016/02
*/

:- use_module(library(apply)).
:- use_module(library(http/html_write)).
:- use_module(library(pairs)).
:- use_module(library(rdf/rdf_api)).

:- rdf_meta
   rdfa_has_po(r, r, ?, ?),
   rdfa_o(o, ?, ?).





%! rdfa_date_time(+V)// is det.

rdfa_date_time(V) -->
  



%! rdfa_has_po(+S, +P)// is det.

rdfa_has_po(S, P) -->
  {once(rdf_has(S, P, O))},
  html(span(property=P, \rdfa_o(O))).



rdfa_o(O^^rdf:'HTML') --> !, html(\[O]).
rdfa_o(O) --> {rdf_is_literal(O), !, rdf_lexical_form(O, Lex)}, html(Lex).
rdfa_o(O) --> html(O).



%! rdfa_prefixes(+Aliases:list(atom), -Prefixes:atom) is det.

rdfa_prefixes(Aliases, Defs) :-
  maplist(rdf_current_prefix, Aliases, Prefixes),
  pairs_keys_values(Pairs, Aliases, Prefixes),
  maplist(pair_to_prefix, Pairs, Defs0),
  atomic_list_concat(Defs0, ' ', Defs).

pair_to_prefix(Alias-Prefix, Def) :-
  atomic_list_concat([Alias,Prefix], ' ', Def).
