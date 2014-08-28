:- module(
  rdf_file_db,
  [
    rdf_content_type/1, % ?ContentType:atom
    rdf_content_type/2, % ?ContentType:atom
                        % ?Format:atom
    rdf_file_extension/1, % ?Extension:atom
    rdf_file_extension/2, % ?Extension:atom
                          % ?Format:atom
    rdf_serialization/5 % ?DefaultExtension:atom
                        % ?DefaultFileType:atom
                        % ?Format:atom
                        % ?ContentTypes:list(atom)
                        % ?Url:atom
  ]
).

/** <module> RDF file database

Some very basic facts about RDF serialization formats,
used by the RDF file modules.

### RDF file types

RDF file types are defined in Semweb as multifile
and with namespace `rdf_db`.

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
@version 2014/04-2014/05, 2014/07-2014/08
*/

:- use_module(library(lists)).



%! rdf_content_type(+ContentType:atom) is semidet.
%! rdf_content_type(-ContentType:atom) is multi.

rdf_content_type(ContentType):-
  rdf_serialization(_, _, _, ContentTypes, _),
  member(ContentType, ContentTypes).

%! rdf_content_type(+ContentType:atom, +Format:atom) is semidet.
%! rdf_content_type(+ContentType:atom, -Format:atom) is semidet.
%! rdf_content_type(-ContentType:atom, +Format:atom) is nondet.
%! rdf_content_type(-ContentType:atom, -Format:atom) is multi.

rdf_content_type(ContentType1, Format):-
  content_type_remove_encoding(ContentType1, ContentType2),
  rdf_serialization(_, _, Format, ContentTypes, _),
  member(ContentType2, ContentTypes).

content_type_remove_encoding(ContentType1, ContentType2):-
  nonvar(ContentType1),
  atomic_list_concat([ContentType2|_], ';', ContentType1), !.
content_type_remove_encoding(ContentType, ContentType).


%! rdf_file_extension(+Extension:atom) is semidet.
% Succeeds for file extensions of RDF serializations.
%! rdf_file_extension(-Extension:atom) is multi.
% Enumerates file extensions RDF serializations.

rdf_file_extension(Ext):-
  rdf_serialization(Ext, _, _, _, _).

%! rdf_file_extension(+Extension:atom, +Format:atom) is semidet.
%! rdf_file_extension(+Extension:atom, -Format:atom) is semidet.
%! rdf_file_extension(-Extension:atom, +Format:atom) is det.
%! rdf_file_extension(-Extension:atom, -Format:atom) is multi.

rdf_file_extension(Ext, Format):-
  rdf_serialization(Ext, _, Format, _, _).


%! rdf_serialization(
%!   ?DefaultExtension:atom,
%!   ?FileType:atom,
%!   ?Format:atom,
%!   ?MIME:list(atom),
%!   ?Url:atom
%! ) is nondet.
%
% @arg DefaultExtension The default extension of the RDF serialization.
%      RDF serializations may have multiple non-default extensions,
%      e.g. =owl= and =xml= for RDF/XML.
% @arg DefaultFileType The default file type of the RDF serialization.
%      File types are registered via user:prolog_file_type/2.
%      Every RDF file type also has the non-default file type `rdf`.
% @arg Format The format name that is used by the Semweb library.
% @arg ContentTypes A list of MIME content types.
%      The first atom is the standardized MIME content type
%      according to the 1.1 recommendations.
% @arg Url The URL at which the serialization is described, if any.
%
% @see http://richard.cyganiak.de/blog/2008/03/what-is-your-rdf-browsers-accept-header/

rdf_serialization(
  nq,
  nquads,
  nquads,
  ['application/n-quads'],
  'http://www.w3.org/ns/formats/N-Quads'
).
rdf_serialization(
  nt,
  ntriples,
  ntriples,
  ['application/n-triples'],
  'http://www.w3.org/ns/formats/N-Triples'
).
rdf_serialization(
  rdf,
  rdf_xml,
  xml,
  [
    'application/rdf+xml',
    'text/rdf+xml',
    'application/xhtml+xml',
    'application/xml',
    'text/rdf',
    'text/xml',
    'application/rss+xml'
  ],
  'http://www.w3.org/ns/formats/RDF_XML'
).
rdf_serialization(
  rdfa,
  rdfa,
  rdfa,
  [
    'application/xhtml+xml',
    'text/html'
  ],
  'http://www.w3.org/ns/formats/RDFa'
).
rdf_serialization(
  trig,
  trig,
  trig,
  [
    'application/trig',
    'application/x-trig'
  ],
  'http://www.w3.org/ns/formats/TriG'
).
rdf_serialization(
  ttl,
  turtle,
  turtle,
  [
    'application/turtle',
    'application/x-turtle',
    'application/rdf+turtle',
    'text/turtle'
  ],
  'http://www.w3.org/ns/formats/Turtle'
).
rdf_serialization(
  n3,
  n3,
  turtle,
  [
    'text/rdf+n3',
    'text/n3'
  ],
  'http://www.w3.org/ns/formats/N3'
).

