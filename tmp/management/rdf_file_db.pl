:- module(
  rdf_file_db,
  [
    rdf_serialization_format/1, % ?Format:atom
    rdf_serialization_label/2, % ?Label:atom
                               % ?Format:atom
    rdf_serialization_resource/2 % ?Format:atom
                                 % ?Resource:iri
  ]
).

/** <module> RDF: file database

Basic facts about RDF serialization formats.

@author Wouter Beek
@tbd Add support for JSON-LD: http://www.w3.org/ns/formats/JSON-LD
@tbd Add support for OWL XML Serialization: http://www.w3.org/ns/formats/OWL_XML
@tbd Add support for OWL Functional Syntax: http://www.w3.org/ns/formats/OWL_Functional
@tbd Add support for OWL Manchester Syntax: http://www.w3.org/ns/formats/OWL_Manchester
@tbd Add support for POWDER: http://www.w3.org/ns/formats/POWDER
@tbd Add support for POWDER-S: http://www.w3.org/ns/formats/POWDER-S
@tbd Add support for PROV-N: http://www.w3.org/ns/formats/PROV-N
@tbd Add support for PROV-XML: http://www.w3.org/ns/formats/PROV-XML
@tbd Add support for RDF/JSON: http://www.w3.org/ns/formats/RDF_JSON
@tbd Add support for RIF XML Syntax: http://www.w3.org/ns/formats/RIF_XML
@tbd Add support for SPARQL Results in XML: http://www.w3.org/ns/formats/SPARQL_Results_XML
@tbd Add support for SPARQL Results in JSON: http://www.w3.org/ns/formats/SPARQL_Results_JSON
@tbd Add support for SPARQL Results in CSV: http://www.w3.org/ns/formats/SPARQL_Results_CSV
@tbd Add support for SPARQL Results in TSV: http://www.w3.org/ns/formats/SPARQL_Results_TSV
@version 2014-2015
*/

:- use_module(library(lists), except([delete/3,subset/2])).
:- use_module(library(semweb/rdf_http_plugin)). % Private predicate.
:- use_module(library(solution_sequences)).

:- use_module(plc(dcg/dcg_abnf)).
:- use_module(plc(dcg/dcg_atom)). % Meta-option.
:- use_module(plc(dcg/dcg_generics)).
:- use_module(plc(generics/db_ext)).
:- use_module(plc(generics/dict_ext)).

:- use_module(plHttp(header/content_negotiation/http_accept)).





%! rdf_serialization_label(?Label:atom, ?Format:atom) is multi.

rdf_serialization_label('N3', n3).
rdf_serialization_label('N-Quads', nquads).
rdf_serialization_label('N-Triples', ntriples).
rdf_serialization_label('RDFa', rdfa).
rdf_serialization_label('TriG', trig).
rdf_serialization_label('Turtle', turtle).
rdf_serialization_label('RDF/XML', xml).



%! rdf_serialization_format(+Format:atom) is semidet.
%! rdf_serialization_format(-Format:atom) is multi.

rdf_serialization_format(Format):-
  distinct(Format, rdf_http_plugin:rdf_content_type(_, _, Format)).



%! rdf_serialization_resource(?Resource:iri, ?Format:atom) is multi.

rdf_serialization_resource('http://www.w3.org/ns/formats/N3', n3).
rdf_serialization_resource('http://www.w3.org/ns/formats/N-Quads', nquads).
rdf_serialization_resource('http://www.w3.org/ns/formats/N-Triples',ntriples).
rdf_serialization_resource('http://www.w3.org/ns/formats/RDFa', rdfa).
rdf_serialization_resource('http://www.w3.org/ns/formats/TriG', trig).
rdf_serialization_resource('http://www.w3.org/ns/formats/Turtle', turtle).
rdf_serialization_resource('http://www.w3.org/ns/formats/RDF_XML', xml).
