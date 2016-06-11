:- module(
  marcxml2rdf,
  [
    rdf_assert_marcxml/3 % +Dom, +Alias, +G
  ]
).

/** <module> MARCXML to RDF conversion

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(semweb/rdf11)).
:- use_module(library(xml/marcxml)).

:- rdf_register_prefix(bf, 'http://bibframe.org/vocab/').
:- rdf_register_prefix(bibo, 'http://purl.org/ontology/bibo/').
:- rdf_register_prefix(wgs84, 'http://www.w3.org/2003/01/geo/wgs84_pos#').

:- rdf_meta
   marcxml_field_property(?, ?, r),
   marcxml_field_subfield_property(?, ?, ?, r),
   rdf_assert_marcxml(+, +, r).

:- dynamic
    marcxml:field_property/3,
    marcxml:field_subfield_property/4.

:- multifile
    marcxml:field_property/3,
    marcxml:field_subfield_property/4.

%! field_property(?Active, ?Field, ?P) is nondet.



%! field_subfield_property(?Active, ?Field, ?Subfield, ?P) is nondet.

marcxml:field_subfield_property(true,  '245', a, dc:title).
marcxml:field_subfield_property(true,  '245', b, bf:subtitle).
marcxml:field_subfield_property(true,  '260', a, wgs84:location).
marcxml:field_subfield_property(true,  '260', b, dc:publisher).
marcxml:field_subfield_property(true,  '260', c, dc:created).
marcxml:field_subfield_property(false, '300', a, bibo:numPages).





%! rdf_assert_marcxml(+Dom, +Alias, +G) is det.

rdf_assert_marcxml(Dom, Alias, G) :-
  % Use the MARC Control Number for the resource name.
  marcxml_controlfield(Dom, '001', Name),
  rdf_global_id(Alias:Name, S),
  
  % Title statement.
  forall((
    marcxml:field_subfield_property(true, Field, Subfield, P),
    marcxml_datafield(Dom, Field, Subfield, Val0)
  ), (
    atom_string(Val0, Val),
    rdf_assert(S, P, Val, G)
  )).
