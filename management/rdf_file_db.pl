:- module(
  rdf_file_db,
  [
    rdf_accept_header_value/1, % -Value:atom
    rdf_file_extension/1, % ?FileExtension:atom
    rdf_file_extension_format/2, % ?FileExtension:atom
                                 % ?Format:rdf_format
    rdf_media_type/1, % ?MediaType:compound
    rdf_media_type_format/2, % ?MediaType:compound
                             % ?Format:rdf_format
    rdf_media_type_string/2, % ?MediaType:compound
                             % ?String:string
    rdf_serialization/4 % ?DefaultFileExtension:atom
                        % ?Format:rdf_format
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

:- use_module(library(lists), except([delete/3])).

:- use_module(os(media_type)).

:- use_module(plDcg(dcg_abnf)).
:- use_module(plDcg(dcg_generics)).

:- use_module(plHttp(parameter/http_media_type)).

:- multifile(error:has_type/2).

error:has_type(rdf_format, Term):-
  error:has_type(oneof([nquads,ntriples,rdfa,trig,turtle,xml]), Term).



%! rdf_accept_header_value(-Value:atom) is det.
% Returns a value that can be used for the Accept header of an HTTP request
% in order to have a fair chance that RDF content -- if available --
% will be returned.
%
% The media types that are considered are:
%   - all the media types that are specified
%     as part of the rdf_serialization/4 predicate.
%   - the most generic media type, i.e. `*/*`.

rdf_accept_header_value(Value):-
  findall(
    MediaType,
    (
      rdf_serialization(_, _, MediaTypes, _),
      member(MediaType, MediaTypes)
    ),
    MediaTypes
  ),
  dcg_with_output_to(atom(Value),
    '#'(
      _,
      'media-type',
			_,
      [media_type('*','*',[q(0.1)])|MediaTypes],
      [separator(atom(', '))]
    )
  ).



%! rdf_file_extension(+FileExtension:atom) is semidet.
% Succeeds for file extensions of RDF serializations.
%! rdf_file_extension(-FileExtension:atom) is multi.
% Enumerates file extensions RDF serializations.

rdf_file_extension(FileExtension):-
  rdf_serialization(FileExtension, _, _, _).



%! rdf_file_extension_format(+FileExtension:atom, +Format:rdf_format) is semidet.
%! rdf_file_extension_format(+FileExtension:atom, -Format:rdf_format) is semidet.
%! rdf_file_extension_format(-FileExtension:atom, +Format:rdf_format) is det.
%! rdf_file_extension_format(-FileExtension:atom, -Format:rdf_format) is multi.

rdf_file_extension_format(Ext, Format):-
  rdf_serialization(Ext, Format, _, _).



%! rdf_media_type(+MediaType:compound) is semidet.
%! rdf_media_type(-MediaType:compound) is multi.

rdf_media_type(MediaType):-
  rdf_serialization(_, _, MediaTypes, _),
  member(_-MediaType, MediaTypes).



%! rdf_media_type_format(+MediaType:compound, +Format:rdf_format) is semidet.
%! rdf_media_type_format(+MediaType:compound, -Format:rdf_format) is semidet.
%! rdf_media_type_format(-MediaType:compound, +Format:rdf_format) is nondet.
%! rdf_media_type_format(-MediaType:compound, -Format:rdf_format) is multi.

rdf_media_type_format(MediaType, Format):-
  rdf_serialization(_, Format, MediaTypes, _),
  member(_-MediaType, MediaTypes).



%! rdf_media_type_string(+MediaType:compound, -String:string) is det.
%! rdf_media_type_string(-MediaType:compound, +String:string) is det.

rdf_media_type_string(MediaType, String):-
  media_type_string(MediaType, String),
  rdf_media_type(MediaType).



%! rdf_serialization(
%!   ?DefaultFileExtension:atom,
%!   ?Format:rdf_format,
%!   ?MediaTypes:list(pair(between(0.0,1.0),compound)),
%!   ?Url:atom
%! ) is nondet.
%
% ### Content types
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
% ### Arguments
%
% @arg DefaultExtension The default extension of the RDF serialization.
%      RDF serializations may have multiple non-default extensions,
%      e.g. =owl= and =xml= for RDF/XML.
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
  [
    media_type(application,'n-quads',[q(0.9)])
  ],
  'http://www.w3.org/ns/formats/N-Quads'
).
rdf_serialization(
  nt,
  ntriples,
  [
    media_type(application,'n-triples',[q(0.9)])
  ],
  'http://www.w3.org/ns/formats/N-Triples'
).
rdf_serialization(
  rdf,
  xml,
  [
    media_type(text,'rdf+xml',[q(0.9)]),
    media_type(application,'rdf+xml',[q(0.5)]),
    media_type(text,rdf,[q(0.5)]),
    media_type(text,xml,[q(0.5)]),
    media_type(application,rdf,[q(0.5)]),
    media_type(application,'rss+xml',[q(0.1)]),
    media_type(application,xml,[q(0.1)])
  ],
  'http://www.w3.org/ns/formats/RDF_XML'
).
rdf_serialization(
  rdfa,
  rdfa,
  [
    media_type(application,'xhtml+xml',[q(0.5)]),
    media_type(text,html,[q(0.5)])
  ],
  'http://www.w3.org/ns/formats/RDFa'
).
rdf_serialization(
  trig,
  trig,
  [
    media_type(application,trig,[q(0.9)]),
    media_type(application,'x-trig',[q(0.5)])
  ],
  'http://www.w3.org/ns/formats/TriG'
).
rdf_serialization(
  ttl,
  turtle,
  [
    media_type(text,turtle,[q(0.9)]),
    media_type(application,turtle,[q(0.5)]),
    media_type(application,'x-turtle',[q(0.5)]),
    media_type(application,'rdf+turtle',[q(0.5)])
  ],
  'http://www.w3.org/ns/formats/Turtle'
).
rdf_serialization(
  n3,
  turtle,
  [
    media_type(text,n3,[q(0.9)]),
    media_type(text,'rdf+n3',[q(0.5)])
  ],
  'http://www.w3.org/ns/formats/N3'
).

