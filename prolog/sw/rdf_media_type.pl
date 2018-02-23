:- module(
  rdf_media_type,
  [
    rdf_media_type/1,     % ?MediaType
    'rdf_media_type_>'/2, % +SuperMediaType +SubMediaType
    rdfa_media_type/1     % ?MediaType
  ]
).

/** <module> RDF Media Types

@author Wouter Beek
@version 2017-2018
*/

:- use_module(library(media_type)).






%! rdf_media_type(@MediaType:compound) is semidet.
%! rdf_media_type(-MediaType:compound) is multi.

% Ordering represents precedence, as used in HTTP Accept headers, from
% lower to higher.
rdf_media_type(media(application/'xhtml+xml',[])).
rdf_media_type(media(application/'json-ld',[])).
rdf_media_type(media(application/'n-quads',[])).
rdf_media_type(media(application/'n-triples',[])).
rdf_media_type(media(application/'rdf+xml',[])).
rdf_media_type(media(application/trig,[])).
rdf_media_type(media(text/turtle,[])).



%! 'rdf_media_type_>'(+SuperMediaType:compound, +SubMediaType:compound) is semidet.
%
% Strict ordering over RDF Media Types.
%
% An RDF Media Type A is greater than an RDF Media Type B if all valid
% documents in B are also valid documents in A, and there are some
% documents that are valid in A that are not valid in B.

'rdf_media_type_>'(X, Y) :-
  'rdf_media_type_='(X, Y), !.
'rdf_media_type_>'(X, Z) :-
  'rdf_media_type_strict>'(X, Y),
  'rdf_media_type_>'(Y, Z).

'rdf_media_type_='(media(Supertype/Subtype,_),  media(Supertype/Subtype,_)).

'rdf_media_type_strict>'(media(application/trig,_), media(text/turtle,_)).
'rdf_media_type_strict>'(
  media(text/turtle,_),
  media(application/'n-triples',_)
).
'rdf_media_type_strict>'(
  media(application/'n-quads',_),
  media(application/'n-triples',_)
).



%! rdfa_media_type(@MediaType:compound) is semidet.
%! rdfa_media_type(-MediaType:compound) is multi.

rdfa_media_type(MediaType) :-
  ground(MediaType), !,
  MediaType = media(Super/Sub,_),
  rdfa_media_type_(media(Super/Sub,_)).
rdfa_media_type(MediaType) :-
  rdfa_media_type_(MediaType).

rdfa_media_type_(media(application/'xhtml+xml',[])).
rdfa_media_type_(media(text/html,[])).
