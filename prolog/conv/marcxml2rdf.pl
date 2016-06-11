:- module(
  marcxml2rdf,
  [
    gen_rdf_marcxml/2 % +Dom, +Alias
  ]
).

/** <module> MARCXML to RDF conversion

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(gen/gen_ntuples)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(xml/marcxml)).





%! gen_rdf_marcxml(+Dom, +Alias) is det.

gen_rdf_marcxml(Dom, Alias) :-
  rdf_create_bnode(S),
  forall(marcxml_controlfield(Dom, Field, Val0), (
    rdf_global_id(Alias:Field, P),
    atom_string(Val0, Val),
    gen_ntriple(S, P, Val)
  )),
  forall(marcxml_datafield(Dom, Field, Subfield, Val0), (
    atom_concat(Field, Subfield, Local),
    rdf_global_id(Alias:Local, P),
    atom_string(Val0, Val),
    gen_ntriple(S, P, Val)
  )).
