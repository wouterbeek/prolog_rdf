:- module(q_array, []).

/** <module> Quine array

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(html/html_ext)).
:- use_module(library(q/qb)).
:- use_module(library(q/q_term)).
:- use_module(library(semweb/rdf11)).

:- qb_alias(tcco, 'http://triply.cc/ontology/').

:- multifile
   q:dcg_q_print_literal_hook//2,
   rdf11:in_ground_type_hook/3,
   rdf11:out_type_hook/3,
   qh:qh_literal_hook//2.


q:dcg_q_print_literal_hook(Array^^D, Opts) -->
  {
    rdf_equal(tcco:array, D), !,
    q_literal_lex(Array^^D, Lex)
  },
  str_ellipsis(Lex, Opts.max_lit_len).


rdf11:in_ground_type_hook(D, L, Lex) :-
  rdf_equal(tcco:array, D), !,
  % @tbd
  (is_list(L) -> atom_phrase(array_gen(L), Lex) ; Lex = L).


rdf11:out_type_hook(D, L, Lex) :-
  rdf_equal(tcco:array, D), !,
  % @tbd
  (atom(Lex) -> atom_phrase(array_par(L), Lex) ; string_phrase(array_par(L), Lex)).


array_gen(L) --> {is_list(L)}, !, "[", seplist(array_gen, " ", L), "]", !.
array_gen(N) --> float(N).


array_par(L) --> "[", !, seplist(array_par, " ", L), "]", !.
array_par(N) --> float(N), !.


qh:qh_literal_hook(Array^^D, Opts) -->
  {
    rdf_equal(tcco:array, D), !,
    q_literal_lex(Array^^D, Lex)
  },
  truncated(Lex, Opts.max_lit_len).
