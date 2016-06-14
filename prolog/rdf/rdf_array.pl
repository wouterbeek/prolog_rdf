:- module(rdf_array, []).

/** <module> RDF array

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(semweb/rdf11)).

:- rdf_register_prefix(tcco, 'http://triply.cc/ontology/').

:- dynamic
   rdf11:in_ground_type_hook/3,
   rdf11:out_type_hook/3.

:- multifile
   rdf11:in_ground_type_hook/3,
   rdf11:out_type_hook/3.

rdf11:in_ground_type_hook(D, L, Lex) :-
  % @tbd
  rdf_equal(tcco:array, D), !,
  atom_phrase(array(L), Lex).

rdf11:out_type_hook(D, L, Lex) :-
  % @tbd
  rdf_equal(tcco:array, D), !,
  atom_phrase(array(L), Lex).

array(L) --> "[", seplist(array, " ", L), "]", !.
array(N) --> float(N).
