:- module(
  rdf_file_db,
  [
    rdf_accept_header_value/1, % -Value:atom
    rdf_file_extension/1, % ?Extension:atom
    rdf_file_extension_format/2, % ?Extension:atom
                                 % ?Format:oneof([nquads,ntriples,rdfa,trig,turtle,xml])
    rdf_media_type/1, % ?MediaType:compound
    rdf_media_type_format/2, % ?MediaType:compound
                             % ?Format:oneof([nquads,ntriples,rdfa,trig,turtle,xml])
    rdf_media_type_string/2, % ?MediaType:compound
                             % ?String:string
    rdf_serialization/5 % ?DefaultExtension:atom
                        % ?DefaultFileType:atom
                        % ?Format:atom
                        % ?MediaTypes:list(pair(between(0.0,1.0),compound))
                        % ?Url:atom
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
@version 2014/04-2014/05, 2014/07-2014/08, 2014/10
*/

:- use_module(library(apply)).
:- use_module(library(lists)).

:- use_module(os(media_type)).



%! rdf_accept_header_value(-Value:atom) is det.
% Returns a value that can be used for the Accept header of an HTTP request
% in order to have a fair chance that RDF content -- if available --
% will be returned.
%
% The media types that are considered are:
%   - all the media types that are specified
%     as part of the rdf_serialization/5 predicate.
%   - the most generic media type, i.e. `*/*`.

rdf_accept_header_value(Value):-
  findall(
    [QValue,MediaType],
    (
      rdf_serialization(_, _, _, MediaTypes, _),
      member(QValue-MediaType, MediaTypes)
    ),
    Pairs1
  ),
  keysort(Pairs1, Pairs2),
  maplist(format_media_type_value, [0.1-media_type('*','*')|Pairs2], Values),
  atomic_list_concat(Values, ', ', Value).

format_media_type_value(QValue-MediaType, Value):-
  format(atom(Value), '~a; q=~1f', [MediaType,QValue]).



%! rdf_file_extension(+Extension:atom) is semidet.
% Succeeds for file extensions of RDF serializations.
%! rdf_file_extension(-Extension:atom) is multi.
% Enumerates file extensions RDF serializations.

rdf_file_extension(Ext):-
  rdf_serialization(Ext, _, _, _, _).



%! rdf_file_extension_format(+Extension:atom, +Format:atom) is semidet.
%! rdf_file_extension_format(+Extension:atom, -Format:atom) is semidet.
%! rdf_file_extension_format(-Extension:atom, +Format:atom) is det.
%! rdf_file_extension_format(-Extension:atom, -Format:atom) is multi.

rdf_file_extension_format(Ext, Format):-
  rdf_serialization(Ext, _, Format, _, _).



%! rdf_media_type(+MediaType:compound) is semidet.
%! rdf_media_type(-MediaType:compound) is multi.

rdf_media_type(MediaType):-
  rdf_serialization(_, _, _, MediaTypes, _),
  member(_-MediaType, MediaTypes).



%! rdf_media_type_format(+MediaType:compound, +Format:atom) is semidet.
%! rdf_media_type_format(+MediaType:compound, -Format:atom) is semidet.
%! rdf_media_type_format(-MediaType:compound, +Format:atom) is nondet.
%! rdf_media_type_format(-MediaType:compound, -Format:atom) is multi.

rdf_media_type_format(MediaType, Format):-
  rdf_serialization(_, _, Format, MediaTypes, _),
  member(_-MediaType, MediaTypes).



%! rdf_media_type_string(+MediaType:compound, -String:string) is det.
%! rdf_media_type_string(-MediaType:compound, +String:string) is det.

rdf_media_type_string(MediaType, String):-
  media_type_string(MediaType, String),
  rdf_media_type(MediaType).



%! rdf_serialization(
%!   ?DefaultExtension:atom,
%!   ?FileType:atom,
%!   ?Format:oneof([nquads,ntriples,rdfa,trig,turtle,xml]),
%!   ?MediaTypes:list(pair(between(0.0,1.0),compound)),
%!   ?Url:atom
%! ) is nondet.
%
% # Content types
%
% Content types are represented as pairs.
% The first element of the pair is a qvalue, as specified in RFC 2616.
% The second element of the pair is the content type name.
%
% The qvalues are determined based on the following criteria:
%   | **Q-Value** | **Reason**                   |
%   | 0.45        | Official content type        |
%   | 0.45        | Specific for RDF content     |
%   | 0.05        | Inofficial content type      |
%   | 0.05        | Not specific for RDF content |
%
% For example, `text/tutle` has qvalue `0.9` because it is
% an official content type that is RDF-specific.
%
% # Arguments
%
% @arg DefaultExtension The default extension of the RDF serialization.
%      RDF serializations may have multiple non-default extensions,
%      e.g. =owl= and =xml= for RDF/XML.
% @arg DefaultFileType The default file type of the RDF serialization.
%      File types are registered via user:prolog_file_type/2.
%      Every RDF file type also has the non-default file type `rdf`.
% @arg Format The format name that is used by the Semweb library.
% @arg MediaTypes A list of content types.
%      The first atom is the standardized content type
%      according to the 1.1 recommendations.
% @arg Url The URL at which the serialization is described, if any.
%
% @see http://richard.cyganiak.de/blog/2008/03/what-is-your-rdf-browsers-accept-header/

rdf_serialization(
  nq,
  nquads,
  nquads,
  [
    0.9-media_type(application,'n-quads')
  ],
  'http://www.w3.org/ns/formats/N-Quads'
).
rdf_serialization(
  nt,
  ntriples,
  ntriples,
  [
    0.9-media_type(application,'n-triples')
  ],
  'http://www.w3.org/ns/formats/N-Triples'
).
rdf_serialization(
  rdf,
  rdf_xml,
  xml,
  [
    0.9-media_type(text,'rdf+xml'),
    0.5-media_type(application,'rdf+xml'),
    0.5-media_type(text,rdf),
    0.5-media_type(text,xml),
    0.5-media_type(application,rdf),
    0.1-media_type(application,'rss+xml'),
    0.1-media_type(application,xml)
  ],
  'http://www.w3.org/ns/formats/RDF_XML'
).
rdf_serialization(
  rdfa,
  rdfa,
  rdfa,
  [
    0.5-media_type(application,'xhtml+xml'),
    0.5-media_type(text,html)
  ],
  'http://www.w3.org/ns/formats/RDFa'
).
rdf_serialization(
  trig,
  trig,
  trig,
  [
    0.9-media_type(application,trig),
    0.5-media_type(application,'x-trig')
  ],
  'http://www.w3.org/ns/formats/TriG'
).
rdf_serialization(
  ttl,
  turtle,
  turtle,
  [
    0.9-media_type(text,turtle),
    0.5-media_type(application,turtle),
    0.5-media_type(application,'x-turtle'),
    0.5-media_type(application,'rdf+turtle')
  ],
  'http://www.w3.org/ns/formats/Turtle'
).
rdf_serialization(
  n3,
  n3,
  turtle,
  [
    0.9-media_type(text,n3),
    0.5-media_type(text,'rdf+n3')
  ],
  'http://www.w3.org/ns/formats/N3'
).

