:- module(
  rdf_file_db,
  [
    rdf_content_type/2, % ?ContentType:atom
                        % ?Format:atom
    rdf_extension/1, % ?Extension:atom
    rdf_mime/1, % ?MIME:atom
    rdf_mime_format/2, % ?MIME:atom
                       % ?Format:atom
    rdf_serialization/5 % ?DefaultExtension:oneof([nt,rdf,triples,ttl])
                        % ?DefaultFileType:oneof([ntriples,rdf_xml,turtle])
                        % ?Format:oneof([ntriples,rdf_xml,triples,turtle])
                        % ?MIMEs:list(atom)
                        % ?URL:atom
  ]
).

/** <module> RDF file database

Some very basic facts about RDF serialization formats,
used by the RDF file modules.

### RDF file types

RDF file types are defined in Semweb as multifile
and with namespace `rdf_db`.

@author Wouter Beek
@version 2014/04-2014/05, 2014/07
*/

:- use_module(library(lists)).



%! rdf_content_type(?ContentType:atom, ?Format:atom) is nondet.

rdf_content_type('text/rdf',              xml).
rdf_content_type('text/xml',              xml).
rdf_content_type('text/rdf+xml',          xml).
rdf_content_type('application/rdf+xml',   xml).
rdf_content_type('application/x-turtle',  turtle).
rdf_content_type('application/turtle',    turtle).
rdf_content_type('application/trig',      trig).
rdf_content_type('application/n-triples', ntriples).
rdf_content_type('application/n-quads',   nquads).
rdf_content_type('text/turtle',           turtle).
rdf_content_type('text/rdf+n3',           turtle).  % Bit dubious
rdf_content_type('text/html',             html).
rdf_content_type('application/xhtml+xml', xhtml).


%! rdf_extension(+Extension:atom) is semidet.
% Succeeds for file extensions of RDF serializations.
%! rdf_extension(-Extension:atom) is multi.
% Enumerates file extensions RDF serializations.

rdf_extension(Ext):-
  rdf_serialization(Ext, _, _, _, _).


%! rdf_mime(+MIME:atom) is semidet.
% Succeeds for MIME content types of RDF serializations.
%! rdf_mime(-MIME:atom) is multi.
% Enumerates MIME content types for RDF serializations.

rdf_mime(Mime):-
  rdf_serialization(_, _, _, Mimes, _),
  member(Mime, Mimes).


%! rdf_mime_format(+MIME:atom, +Format:atom) is semidet.
%! rdf_mime_format(+MIME:atom, -Format:atom) is det.
%! rdf_mime_format(-MIME:atom, +Format:atom) is det.
% Relates RDF media content types and RDF formats.

rdf_mime_format(Mime, Format):-
  rdf_serialization(_, _, Format, Mimes, _),
  memberchk(Mime, Mimes).


%! rdf_serialization(
%!   ?DefaultExtension:oneof([nt,rdf,triples,ttl]),
%!   ?FileType:oneof([ntriples,rdf_xml,triples,turtle]),
%!   ?Format:oneof([ntriples,xml,triples,turtle]),
%!   ?MIME:list(atom),
%!   ?URL:atom
%! ) is nondet.
%
% @arg DefaultExtension The default extension of the RDF serialization.
%      RDF serializations may have multiple non-default extensions,
%      e.g. =owl= and =xml= for RDF/XML.
% @arg DefaultFileType The default file type of the RDF serialization.
%      Every file type has the non-default file type =rdf=.
% @arg Format The format name that is used by the Semweb library.
% @arg MIMEs A list of MIME content types.
%      The first atom is the standardized MIME content type
%      according to the 1.1 recommendations.
% @arg URL The URL at which the serialization is described, if any.
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
    'text/xml',
    'application/rss+xml'
  ],
  'http://www.w3.org/ns/formats/RDF_XML'
).
%rdf_serialization(
%  rdfa,
%  rdfa,
%  rdfa,
%  ['text/html'],
%  ''
%).
rdf_serialization(
  trig,
  trig,
  trig,
  ['application/trig','application/x-trig'],
  'http://www.w3.org/ns/formats/TriG'
).
rdf_serialization(
  ttl,
  turtle,
  turtle,
  [
    'text/turtle',
    'application/x-turtle',
    'application/turtle',
    'application/rdf+turtle'
  ],
  'http://www.w3.org/ns/formats/Turtle'
).
rdf_serialization(
  n3,
  n3,
  turtle,
  ['text/n3','text/rdf+n3'],
  'http://www.w3.org/TeamSubmission/n3/'
).

