:- module(
  marcxml2rdf,
  [
    marcxml2triple/3 % +Dom, +Alias, -Triple
  ]
).

/** <module> MARCXML to RDF conversion

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(semweb/rdf11)).
:- use_module(library(xml/marcxml)).





%! gen_rdf_marcxml(+Dom, +Alias) is det.

gen_rdf_marcxml(Dom, Alias) :-
  gen_rdf_marcxml_triple(Dom, Alias, Triple),
  gen_ntriple(Triple),
  fail.
gen_rdf_marcxml(_, _).



%! marcxml2triple(+Dom, +Alias, -Triple) is nondet.

marcxml2triple(Dom, Alias, Triple) :-
  rdf_create_bnode(S),
  (   marcxml_controlfield(Dom, Field, Val0),
      rdf_global_id(Alias:Field, P),
      atom_string(Val0, Val)
  ;   marcxml_datafield(Dom, Field, Subfield, Val0),
      atom_concat(Field, Subfield, Local),
      rdf_global_id(Alias:Local, P),
      atom_string(Val0, Val)
  ),
  Triple = rdf(S, P, Val).
