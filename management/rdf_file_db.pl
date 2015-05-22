:- module(
  rdf_file_db,
  [
    rdf_file_extension/1, % ?Extension:atom
    rdf_file_extension/2, % ?Extension:atom
                          % ?Format:atom
    rdf_media_type/1, % ?MediaType:dict
    rdf_media_type/2, % ?MediaType:dict
                      % ?Format:atom
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

:- dynamic(user:prolog_file_type/2).
:- multifile(user:prolog_file_type/2).

user:prolog_file_type(nq, nquads).
user:prolog_file_type(nt, ntriples).
user:prolog_file_type(html, rdfa).
user:prolog_file_type(n3, n3).
user:prolog_file_type(trig, trig).
user:prolog_file_type(trix, trix).
user:prolog_file_type(ttl, turtle).
user:prolog_file_type(rdf, xml).





%! rdf_file_extension(+Extension:atom) is semidet.
% Succeeds for file extensions of RDF serializations.
%! rdf_file_extension(-Extension:atom) is multi.
% Enumerates file extensions RDF serializations.

rdf_file_extension(Ext):-
  distinct(Ext, rdf_file_extension(Ext, _)).



%! rdf_file_extension(+Extension:atom, +Format:atom) is semidet.
%! rdf_file_extension(+Extension:atom, -Format:atom) is semidet.
%! rdf_file_extension(-Extension:atom, +Format:atom) is det.
%! rdf_file_extension(-Extension:atom, -Format:atom) is multi.

rdf_file_extension(Ext, Format):-
  rdf_http_plugin:rdf_content_type(_, _, Format),
  user:prolog_file_type(Ext, Format).



%! rdf_media_type(+MediaType:atom) is semidet.
%! rdf_media_type(-MediaType:atom) is multi.

rdf_media_type(MediaType):-
  distinct(MediaType, rdf_media_type(MediaType, _)).



%! rdf_media_type(+MediaType:atom, +Format:atom) is semidet.
%! rdf_media_type(+MediaType:atom, -Format:atom) is semidet.
%! rdf_media_type(-MediaType:atom, +Format:atom) is nondet.
%! rdf_media_type(-MediaType:atom, -Format:atom) is multi.

rdf_media_type(MediaType, Format):-
  rdf_http_plugin:rdf_content_type(MediaType, _, Format).



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
