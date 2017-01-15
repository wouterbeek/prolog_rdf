:- module(
  lod_print,
  [
    lod_print/0,
    lod_print/3 % ?S, ?P, ?O
  ]
).

/** <module> LOD Print

Print the LOD Cloud.

@author Wouter Beek
@version 2015/11, 2016/06
*/

:- use_module(library(apply)).
:- use_module(library(q/rdf_print)).
:- use_module(library(service/ll_api)).

:- rdf_meta
   lod_print(r, r, o).





lod_print:-
  lod_print(_, _, _).


lod_print(S, P, O):-
  call_on_lod(S, P, O, rdf_print_quads),
  fail.
