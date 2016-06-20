:- module(
  marcxml2rdf,
  [
    marcxml2record/3, % +Dom, +Alias, -Triples
    marcxml2stmt/3    % +Dom, +Alias, -Triple
  ]
).

/** <module> MARCXML to RDF conversion

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(semweb/rdf11)).
:- use_module(library(xml/marcxml)).





%! marcxml2record(+Dom, +Alias, -Triples) is nondet.

marcxml2record(Dom, Alias, Triples) :-
  rdf_create_bnode(S),
  findall(Triple, (
    (   marcxml_controlfield(Dom, Field, Val0),
        rdf_global_id(Alias:Field, P),
        atom_string(Val0, Val)
    ;   marcxml_datafield(Dom, Field, Subfield, Val0),
        atom_concat(Field, Subfield, Local),
        rdf_global_id(Alias:Local, P),
        atom_string(Val0, Val)
    ),  rdf_global_term(rdf(S,P,Val^^xsd:string), Triple)
  ), Triples).



%! marcxml2stmt(+Dom, +Alias, -Triple) is nondet.

marcxml2stmt(Dom, Alias, Triple) :-
  marcxml2record(Dom, Alias, Triples),
  member(Triple, Triples).
