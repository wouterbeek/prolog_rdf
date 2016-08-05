:- module(
  rdf_file,
  [
    rdf_default_file_extension/2, % ?Format, ?Ext
    rdf_format/1,                 % ?Format
    rdf_format/2,                 % ?Mediatype:atom, ?Format
    rdf_format_iri/2              % ?Format, ?Iri
  ]
).

/** <module> RDF file

@author Wouter Beek
@version 2015/08, 2016/01-2016/03, 2016/05, 2016/08
*/

:- use_module(library(error)). % Hook.
:- use_module(library(q/qb)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_http_plugin)). % RDF serialization formats.
:- use_module(library(solution_sequences)).


:- qb_alias(formats, 'http://www.w3.org/ns/formats/').


:- multifile
    error:has_type/2.

error:has_type(rdf_format, F) :-
  error:has_type(turtle_format, F).
error:has_type(rdf_format, F) :-
  error:has_type(rdfxml_format, F).
error:has_type(rdf_format, F) :-
  memberchk(F, [rdfa,jsonld]).

error:has_type(rdfxml_format, F) :-
  memberchk(F, [xml]).

error:has_type(turtle_format, F) :-
  memberchk(F, [nquads,ntriples,trig,turtle]).


:- multifile
    rdf_http_plugin:rdf_content_type/3.

rdf_http_plugin:rdf_content_type('application/ld+json', 0.99, jsonld). %ABC


:- rdf_meta
   rdf_format_iri(?, r).





%! rdf_default_file_extension(?Format, ?Ext) is semidet.

rdf_default_file_extension(nquads, nq).
rdf_default_file_extension(ntriples, nt).
rdf_default_file_extension(rdfa, html).
rdf_default_file_extension(jsonld, jsonld).
rdf_default_file_extension(trig, trig).
rdf_default_file_extension(turtle, ttl).
rdf_default_file_extension(xml, rdf).



%! rdf_format(?Format) is nondet.
%! rdf_format(?MT, ?Format) is nondet.

rdf_format(Format) :-
  rdf_format(_, Format).


rdf_format(MT, Format) :-
  rdf_http_plugin:rdf_content_type(MT, _, Format).



%! rdf_format_iri(?Format, ?Iri) .
% @see https://www.w3.org/ns/formats/

%rdf_format_iri(json,     formats:'RDF_JSON').
rdf_format_iri(jsonld,   formats:'JSON-LD').
%rdf_format_iri(ldpatch,  formats:'LD_Patch').
%rdf_format_iri(micro,    formats:'microdata').
%rdf_format_iri(n3,       formats:'N3').
rdf_format_iri(ntriples, formats:'N-Triples').
rdf_format_iri(nquads,   formats:'N-Quads').
%rdf_format_iri(owlx,     formats:'OWL_XML').
%rdf_format_iri(owlf,     formats:'OWL_Functional').
%rdf_format_iri(owlm,     formats:'OWL_Manchester').
%rdf_format_iri(powder,   formats:'POWDER').
%rdf_format_iri(powders,  formats:'POWDER-S').
%rdf_format_iri(provn,    formats:'PROV-N').
%rdf_format_iri(provx,    formats:'PROV-XML').
rdf_format_iri(rdfa,     formats:'RDFa').
%rdf_format_iri(rif,      formats:'RIF_XML').
%rdf_format_iri(sparqlx,  formats:'SPARQL_Results_XML').
%rdf_format_iri(sparqlj,  formats:'SPARQL_Results_JSON').
%rdf_format_iri(sparqlc,  formats:'SPARQL_Results_CSV').
%rdf_format_iri(sparqlt,  formats:'SPARQL_Results_TSV').
rdf_format_iri(turtle,   formats:'Turtle').
rdf_format_iri(trig,     formats:'TriG').
rdf_format_iri(xml,      formats:'RDF_XML').
