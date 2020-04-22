:- module(
  rdf_deref,
  [
    rdf_deref_file/2,   % +File, :Goal_3
    rdf_deref_file/3,   % +File, :Goal_3, +Options
    rdf_deref_stream/3, % +Uri, +In, :Goal_3
    rdf_deref_stream/4, % +Uri, +In, :Goal_3, +Options
    rdf_deref_uri/2,    % +Uri, :Goal_3
    rdf_deref_uri/3     % +Uri, :Goal_3, +Options
  ]
).

/** <module> RDF dereference

*/

:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_ntriples)).
:- use_module(library(semweb/rdfa)).
:- use_module(library(semweb/turtle)).

:- use_module(library(archive_ext)).
:- use_module(library(atom_ext)).
:- use_module(library(file_ext)).
:- use_module(library(hash_ext)).
:- use_module(library(http_client2)).
:- use_module(library(media_type)).
:- use_module(library(stream_ext)).
:- use_module(library(rdf_guess)).
:- use_module(library(rdf_media_type)).
:- use_module(library(rdf_prefix)).
:- use_module(library(rdf_term)).
:- use_module(library(uri_ext)).

:- meta_predicate
    rdf_deref_file(+, 3),
    rdf_deref_file(+, 3, +),
    rdf_deref_stream(+, +, 3),
    rdf_deref_stream(+, +, 3, +),
    rdf_deref_uri(+, 3),
    rdf_deref_uri(+, 3, +).

:- rdf_meta
   rdf_deref_uri(r, :),
   rdf_deref_uri(r, :, +).





%! rdf_deref_file(+File, :Goal_3) is det.
%! rdf_deref_file(+File, :Goal_3, +Options:list(compound)) is det.
%
% @arg Options allows the following options to be set.
%
%   * base_uri(+atom)
%
%     By default, the base URI is the file URI.
%
%   * Other options are passed to rdf_defer_stream/4.

rdf_deref_file(Spec, Goal_3) :-
  rdf_deref_file(Spec, Goal_3, []).


rdf_deref_file(Spec, Goal_3, Options1) :-
  absolute_file_name(Spec, File, [access(read)]),
  uri_data_file(BaseUri, File),
  merge_options(Options1, [base_uri(BaseUri)], Options2),
  read_from_file(
    File,
    {BaseUri,Goal_3,Options2}/[In]>>rdf_deref_stream(BaseUri, In, Goal_3, Options2)
  ).



%! rdf_deref_stream(+BaseUri:atom, +In:stream, :Goal_3) is det.
%! rdf_deref_stream(+BaseUri:atom, +In:stream, :Goal_3, +Options:list(compound)) is det.
%
% The following call will be made:
%
% ```
% call(:Goal_3, +Site, +Tuples, ?Graph)
% ```
%
% The following options are supported:
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

rdf_deref_stream(BaseUri, In, Goal_3) :-
  rdf_deref_stream(BaseUri, In, Goal_3, []).


rdf_deref_stream(BaseUri, In1, Mod:Goal_3, Options1) :-
  archive_stream(In1, In2),
  % Determine the serialization format.
  (   % An explicitly specified Media Type overrules everything else.
      option(media_type(MediaType), Options1)
  ->  true
  ;   % Heuristic 1: guess based on a first chunk of the data.
      rdf_guess_stream(In2, 10 000, MediaTypeGuess),
      % Heuristic 2: the value of the HTTP `Content-Type' header.
      ignore(option(content_type(MediaTypeHttp), Options1)),
      % Heuristic 3: the URI path's file name extension.
      ignore(uri_media_type(BaseUri, MediaTypeUri)),
      (   nonvar(MediaTypeHttp),
          \+ 'rdf_media_type_>'(MediaTypeGuess, MediaTypeHttp)
      ->  print_message(warning, inconsistent_media_types(http(MediaTypeHttp),guess(MediaTypeGuess)))
      ;   true
      ),
      (   nonvar(MediaTypeUri),
          \+ 'rdf_media_type_>'(MediaTypeUri, MediaTypeHttp)
      ->  print_message(warning, inconsistent_media_types(MediaType,MediaTypeUri))
      ;   true
      ),
      (   nonvar(MediaTypeHttp)
      ->  MediaType = MediaTypeHttp
      ;   nonvar(MediaTypeUri)
      ->  MediaType = MediaTypeUri
      ;   MediaType = MediaTypeGuess
      )
  ),
  Goal_3 =.. [Pred|Args1],
  append(Args1, [BaseUri], Args2),
  Goal_2 =.. [Pred|Args2],
  % Use a well-known IRI with a UUID as blank node prefix.  The UUID
  % is determined by the base URI seed.
  md5(BaseUri, Hash),
  well_known_iri([Hash], BNodePrefix),
  % Parse according to the guessed Media Type.
  (   % N-Quads
      media_type_comps(MediaType, application, 'n-quads', _)
  ->  merge_options(
        [anon_prefix(BNodePrefix),base_uri(BaseUri),format(nquads)],
        Options1,
        Options2
      ),
      rdf_process_ntriples(In2, Mod:Goal_2, Options2)
  ;   % N-Triples
      media_type_comps(MediaType, application, 'n-triples', _)
  ->  merge_options(
        [anon_prefix(BNodePrefix),base_uri(BaseUri),format(ntriples)],
        Options1,
        Options2
      ),
      rdf_process_ntriples(In2, Mod:Goal_2, Options2)
  ;   % RDF/XML
      media_type_comps(MediaType, application, 'rdf+xml', _)
  ->  merge_options([base_uri(BaseUri),max_errors(-1)], Options1, Options2),
      process_rdf(In2, Mod:Goal_2, Options2)
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
      rdf_process_turtle(In2, Mod:Goal_2, Options2)
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
      rdf_process_turtle(In2, Mod:Goal_2, Options2)
  ;   % RDFa
      memberchk(MediaType, [media(application/'xhtml+xml',_),media(text/html,_)])
  ->  merge_options(
        [anon_prefix(BNodePrefix),base(BaseUri),max_errors(-1)],
        Options1,
        Options2
      ),
      read_rdfa(In2, Triples, Options2),
      call(Mod:Goal_2, Triples, _)
  %;   % JSON-LD
  %    memberchk(MediaType, [media(application/'ld+json',_)])
  %->  read_jsonld(In2, Triples),
  %    call(Mod:Goal_2, Triples, _)
  ;   % An unsupported Media Type.
      print_message(warning, rdf(unsupported_format(MediaType,_)))
  ).



%! rdf_deref_uri(+Uri:atom, :Goal_3) is det.
%! rdf_deref_uri(+Uri:atom, :Goal_3, +Options:list(compound)) is det.
%
% The following options are supported:
%
%   * accept(+MediaTypes:list(compound))
%
%     The value of the HTTP Accept header, from high to low
%     precedence.  The default value is a list of all and only
%     standardized Media Types.
%
%   * Other options are passed to rdf_deref_stream/4.

rdf_deref_uri(Uri, Goal_3) :-
  rdf_deref_uri(Uri, Goal_3, []).


rdf_deref_uri(Uri, Goal_3, Options1) :-
  uri_is_global(Uri), !,
  % `Accept' header
  (   select_option(accept(MediaTypes), Options1, Options2)
  ->  true
  ;   findall(MediaType, rdf_media_type(MediaType), MediaTypes),
      Options2 = Options1
  ),
  http_open2(Uri, In, [accept(MediaTypes),failure(404),metadata(Metas)]),
  call_cleanup(
    (
      (   http_metadata_content_type(Metas, MediaType)
      ->  merge_options(Options2, [content_type(MediaType)], Options3)
      ;   Options3 = Options2
      ),
      rdf_deref_stream(Uri, In, Goal_3, Options3)
    ),
    close(In)
  ).
