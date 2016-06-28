:- module(q_array, []).

/** <module> Quine array

"[[[[5.469528183884724 51.34669900730107] [5.46948054297083 51.34672156677506] [5.469399538134988 51.346652770426864] [5.469444439087056 51.34663106685318] [5.469528183884724 51.34669900730107]]]]" ^^ tcco:array

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(dcg/dcg_atom)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(html/html_bs)).
:- use_module(library(q/q_term)).

:- q_create_alias(tcco, 'http://triply.cc/ontology/').

:- multifile
   q:dcg_print_literal_hook//2,
   rdf11:in_ground_type_hook/3,
   rdf11:out_type_hook/3,
   qh:qh_literal_hook//2.

q:dcg_print_literal_hook(Array^^D, Opts) -->
  {
    qis(tcco:array, D), !,
    q_literal_lex(Array^^D, Lex)
  },
  str_ellipsis(Lex, Opts.max_length).

rdf11:in_ground_type_hook(D, L, Lex) :-
  % @tbd
  qis(tcco:array, D), !,
  atom_phrase(array(L), Lex).

rdf11:out_type_hook(D, L, Lex) :-
  % @tbd
  qis(tcco:array, D), !,
  atom_phrase(array(L), Lex).

array(L) --> "[", !, seplist(array, " ", L), "]", !.
array(N) --> float(N).

qh:qh_literal_hook(Array^^D, Opts) -->
  {
    qis(tcco:array, D), !,
    q_literal_lex(Array^^D, Lex)
  },
  bs_truncated(Lex, Opts.max_length).
