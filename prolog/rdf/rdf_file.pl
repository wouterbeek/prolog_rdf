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
@version 2015/08, 2016/01
*/

:- use_module(library(rdf/rdf_api)).
:- use_module(library(semweb/rdf_http_plugin)). % RDF serialization formats.
:- use_module(library(solution_sequences)).

:- rdf_meta
   rdf_format_iri(?, r).

:- dynamic
    user:prolog_file_type/2.
:- multifile
    user:prolog_file_type/2.

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

rdf_format_iri(jsonld,   format:'JSON-LD').
rdf_format_iri(n3,       format:'N3').
rdf_format_iri(ntriples, format:'N-Triples').
rdf_format_iri(nquads,   format:'N-Quads').
rdf_format_iri(ldpatch,  format:'LD_Patch').
rdf_format_iri(micro,    format:'microdata').
rdf_format_iri(owlx,     format:'OWL_XML').
rdf_format_iri(owlf,     format:'OWL_Functional').
rdf_format_iri(owlm,     format:'OWL_Manchester').
rdf_format_iri(powder,   format:'POWDER').
rdf_format_iri(powders,  format:'POWDER-S').
rdf_format_iri(provn,    format:'PROV-N').
rdf_format_iri(provx,    format:'PROV-XML').
rdf_format_iri(rdfa,     format:'RDFa').
rdf_format_iri(json,     format:'RDF_JSON').
rdf_format_iri(xml,      format:'RDF_XML').
rdf_format_iri(rif,      format:'RIF_XML').
rdf_format_iri(sparqlx,  format:'SPARQL_Results_XML').
rdf_format_iri(sparqlj,  format:'SPARQL_Results_JSON').
rdf_format_iri(sparqlc,  format:'SPARQL_Results_CSV').
rdf_format_iri(sparqlt,  format:'SPARQL_Results_TSV').
rdf_format_iri(turtle,   format:'Turtle').
rdf_format_iri(trig,     format:'TriG').
