:- module(
  rdf_deref,
  [
    rdf_deref_stream/3, % +Uri, +In, :Goal_3
    rdf_deref_stream/4  % +Uri, +In, :Goal_3, +Options
  ]
).

/** <module> RDF dereference

@author Wouter Beek
@version 2017-2018
*/

:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_ntriples)).
:- use_module(library(semweb/rdfa)).
:- use_module(library(semweb/turtle)).

:- use_module(library(default)).
:- use_module(library(media_type)).
:- use_module(library(sw/rdf_guess)).
:- use_module(library(sw/rdf_media_type)).
:- use_module(library(uri_ext)).

:- meta_predicate
    rdf_deref_stream(+, +, 3),
    rdf_deref_stream(+, +, 3, +).





%! rdf_deref_stream(+Uri:atom, +In:stream, :Goal_3) is det.
%! rdf_deref_stream(+Uri:atom, +In:stream, :Goal_3, +Options:list(compound)) is det.
%
% The following options are supported:
%
%   * base_uri(+atom)
%
%     The default is the URI of the last metadata element.
%
%   * bnode_prefix(+atom)
%
%     The default is a well-known IRI as per RDF 1.1.
%
%   * content_type(+MediaType:compound)
%
%     The parsed value of the HTTP `Content-Type' header, if any.
%
%   * media_type(+MediaType:compound)
%
%     Overrules the RDF serialization format.

rdf_deref_stream(Uri, In, Goal_3) :-
  rdf_deref_stream(Uri, In, Goal_3, []).


rdf_deref_stream(Uri, In, Mod:Goal_3, Options1) :-
  % Determine the serialization format.
  (   % An explicitly specified Media Type overrules everything else.
      option(media_type(MediaType), Options1)
  ->  true
  ;   % Heuristic 1: guess based on a first chunk of the data.
      rdf_guess_stream(In, 10 000, MediaTypeGuess),
      (   % Heuristic 2: the value of the HTTP `Content-Type' header.
          option(content_type(MediaType), Options1),
          'rdf_media_type_>'(MediaType, MediaTypeGuess)
      ->  !, true
      ;   print_message(warning, inconsistent_media_types(MediaType,MediaTypeGuess)),
          MediaType = MediaTypeGuess
      ),
      (   % Heuristic 3: the URI path's file name extension.
          uri_media_type(Uri, MediaTypeUri)
      ->  (   'rdf_media_type_>'(MediaType, MediaTypeUri)
          ->  !, true
          ;   print_message(warning, inconsistent_media_types(MediaType,MediaTypeUri))
          )
      ;   true
      )
  ),
  % Determine the base URI.
  option(base_uri(BaseUri), Options1, Uri),

  % Determine the blank node prefix.  Use a well-known IRI with a UUID
  % component by default.
  call_default_option(bnode_prefix(BNodePrefix), Options1, rdf_bnode_iri),
  Goal_3 =.. [Pred|Args1],
  append(Args1, [BNodePrefix], Args2),
  Goal_2 =.. [Pred|Args2],

  % Parse according to the guessed Media Type.
  (   % N-Quads
      media_type_comps(MediaType, application, 'n-quads', _)
  ->  merge_options(
        [anon_prefix(BNodePrefix),base_uri(BaseUri),format(nquads)],
        Options1,
        Options2
      ),
      rdf_process_ntriples(In, Mod:Goal_2, Options2)
  ;   % N-Triples
      media_type_comps(MediaType, application, 'n-triples', _)
  ->  merge_options(
        [anon_prefix(BNodePrefix),base_uri(BaseUri),format(ntriples)],
        Options1,
        Options2
      ),
      rdf_process_ntriples(In, Mod:Goal_2, Options2)
  ;   % RDF/XML
      media_type_comps(MediaType, application, 'rdf+xml', _)
  ->  merge_options([base_uri(BaseUri),max_errors(-1)], Options1, Options2),
      process_rdf(In, Mod:Goal_2, Options2)
  ;   % TriG
      media_type_comps(MediaType, application, trig, _)
  ->  merge_options(
        [
          anon_prefix(BNodePrefix),
          base_uri(BaseUri),
          format(trig),
          resources(iri)
        ],
        Options1,
        Options2
      ),
      rdf_process_turtle(In, Mod:Goal_2, Options2)
  ;   % Turtle
      media_type_comps(MediaType, text, turtle, _)
  ->  merge_options(
        [
          anon_prefix(BNodePrefix),
          base_uri(BaseUri),
          format(turtle),
          resources(iri)
        ],
        Options1,
        Options2
      ),
      rdf_process_turtle(In, Mod:Goal_2, Options2)
  ;   % RDFa
      memberchk(MediaType, [media(application/'xhtml+xml',_),media(text/html,_)])
  ->  merge_options(
        [anon_prefix(BNodePrefix),base(BaseUri),max_errors(-1)],
        Options1,
        Options2
      ),
      read_rdfa(In, Triples, Options2),
      call(Mod:Goal_2, Triples, _)
  ;   % An unsupported Media Type (e.g., JSON-LD).
      print_message(warning, rdf(unsupported_format(MediaType,_)))
  ).
