:- module(
  rdf_file,
  [
    rdf_file_extension/1, % ?Ext
    rdf_file_extension/2, % ?Ext, ?Format
    rdf_format/2,         % ?Mediatype:atom, ?Format
    rdf_format_iri/2      % ?Format, ?Iri
  ]
).

/** <module> RDF file

@author Wouter Beek
@version 2015/08, 2016/01-2016/03
*/

:- use_module(library(error)). % Hook.
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_http_plugin)). % RDF serialization formats.
:- use_module(library(solution_sequences)).

:- rdf_meta
   rdf_format_iri(?, r).

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

:- dynamic
    user:prolog_file_type/2,
    rdf_http_plugin:rdf_content_type/2.

:- multifile
    user:prolog_file_type/2,
    rdf_http_plugin:rdf_content_type/2.

user:prolog_file_type(nq,     nquads).
user:prolog_file_type(nt,     ntriples).
user:prolog_file_type(html,   rdfa).
user:prolog_file_type(jsonld, jsonld).
%user:prolog_file_type(n3,     n3).
user:prolog_file_type(trig,   trig).
user:prolog_file_type(ttl,    turtle).
user:prolog_file_type(rdf,    xml).

rdf_http_plugin:rdf_content_type('application/ld+json', 0.99, jsonld). %ABC





%! rdf_file_extension(+Extension:atom) is semidet.
% Succeeds for file extensions of RDF serializations.
%! rdf_file_extension(-Extension:atom) is multi.
% Enumerates file extensions RDF serializations.

rdf_file_extension(Ext) :-
  distinct(Ext, rdf_file_extension(Ext, _)).



%! rdf_file_extension(+Extension:atom, +Format:atom) is semidet.
%! rdf_file_extension(+Extension:atom, -Format:atom) is semidet.
%! rdf_file_extension(-Extension:atom, +Format:atom) is det.
%! rdf_file_extension(-Extension:atom, -Format:atom) is multi.

rdf_file_extension(Ext, Format) :-
  rdf_http_plugin:rdf_content_type(_, _, Format),
  user:prolog_file_type(Ext, Format).



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
