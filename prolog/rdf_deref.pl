:- encoding(utf8).
:- module(
  rdf_deref,
  [
    rdf_deref_file/2,   % +FileSpec, :Goal_3
    rdf_deref_file/3,   % +FileSpec, :Goal_3, +Options
    rdf_deref_stream/3, % +Uri, +In, :Goal_3
    rdf_deref_stream/4, % +Uri, +In, :Goal_3, +Options
    rdf_deref_uri/2,    % +Uri, :Goal_3
    rdf_deref_uri/3     % +Uri, :Goal_3, +Options
  ]
).

/** <module> RDF dereference

*/

:- use_module(library(aggregate)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_ntriples), []).
:- use_module(library(semweb/rdfa), []).
:- use_module(library(semweb/turtle), []).
:- use_module(library(yall)).

:- use_module(library(archive_ext)).
:- use_module(library(atom_ext)).
:- use_module(library(dict)).
:- use_module(library(file_ext)).
:- use_module(library(hash_ext)).
:- use_module(library(http_client2)).
:- use_module(library(media_type)).
:- use_module(library(stream_ext)).
:- use_module(library(rdf_guess)).
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



%! rdf_deref_file(+FileSpec:term, :Goal_3) is det.
%! rdf_deref_file(+FileSpec:term, :Goal_3, +Options:options) is det.
%
% @param Options allows the following options to be set.
%
%        * base_iri(+atom)
%
%          By default, the base IRI is the file URI.
%
%        * Other options are passed to rdf_defer_stream/4.
%
%        * Tries to set the ‘media_type’ option based on the file
%          name, if this option is not explicitly specified by the
%          caller.

rdf_deref_file(Spec, Goal_3) :-
  rdf_deref_file(Spec, Goal_3, options{}).


rdf_deref_file(Spec, Goal_3, Options1) :-
  absolute_file_name(Spec, File, [access(read)]),
  uri_file_name(BaseIri, File),
  merge_dicts(Options1, options{base_iri: BaseIri}, Options2),
  read_from_file(
    File,
    {BaseIri,Goal_3,Options2}/[In0]>>
      rdf_deref_stream(BaseIri, In0, Goal_3, Options2)
  ).



%! rdf_deref_stream(+BaseIri:iri, +In:istream, :Goal_3) is det.
%! rdf_deref_stream(+BaseIri:iri, +In:istream, :Goal_3, +Options:options) is det.
%
% The following call will be made:
%
% ```
% call(:Goal_3, +BaseIri:iri, +Tuples:list(rdf_tuple), ?GraphName:rdf_graph_name)
% ```
%
% The following options are supported:
%
%   * bnode_prefix(+atom)
%
%     The default is a well-known IRI as per RDF 1.1.
%
%   * content_type(+MediaType:media_type)
%
%     The parsed value of the HTTP ‘Content-Type’ header, if any.
%
%   * media_type(+MediaType:media_type)
%
%     Overrules the RDF serialization format.

rdf_deref_stream(BaseIri, In, Goal_3) :-
  rdf_deref_stream(BaseIri, In, Goal_3, options{}).


rdf_deref_stream(BaseIri, In1, Mod:Goal_3, Options1) :-
  % BUG: https://github.com/SWI-Prolog/swipl-devel/issues/765
  %archive_stream(In1, In2),
  In2 = In1,

  % Determine the serialization format.
  (   % An explicitly specified Media Type overrules everything else.
      options{media_type: MediaType} :< Options1
  ->  true
  ;   % Heuristic 1: guess based on a first chunk of the data.
      rdf_guess_stream(In2, 10 000, MediaTypeGuess),
      % Heuristic 2: the value of the HTTP Content-Type header.
      ignore(option{content_type: MediaTypeHttp} :< Options1),
      % Heuristic 3: the URI path's file name extension.
      ignore(uri_media_type(BaseIri, MediaTypeIri)),
      (   nonvar(MediaTypeHttp),
          \+ 'rdf_media_type_>'(MediaTypeGuess, MediaTypeHttp)
      ->  print_message(warning, inconsistent_media_types(http(MediaTypeHttp),guess(MediaTypeGuess)))
      ;   true
      ),
      (   nonvar(MediaTypeIri),
          \+ 'rdf_media_type_>'(MediaTypeIri, MediaTypeHttp)
      ->  print_message(warning, inconsistent_media_types(MediaType,MediaTypeIri))
      ;   true
      ),
      (   nonvar(MediaTypeHttp)
      ->  MediaType = MediaTypeHttp
      ;   nonvar(MediaTypeIri)
      ->  MediaType = MediaTypeIri
      ;   MediaType = MediaTypeGuess
      )
  ),
  Goal_3 =.. [Pred|Args1],
  append(Args1, [BaseIri], Args2),
  Goal_2 =.. [Pred|Args2],
  % Use a well-known IRI with a UUID as blank node prefix.  The UUID
  % is determined by the base IRI seed.
  md5(BaseIri, Hash),
  well_known_iri([Hash], BNodePrefix),
  % Parse according to the guessed Media Type.
  (   % N-Quads
      MediaType = media(application/'n-quads',_)
  ->  merge_dicts(
        otpions{anon_prefix: BNodePrefix, base_iri: BaseIri, format: nquads},
        Options1,
        Options2
      ),
      dict_terms(Options2, Options3),
      rdf_ntriples:rdf_process_ntriples(In2, Mod:Goal_2, Options3)
  ;   % N-Triples
      MediaType = media(application/'n-triples',_)
  ->  merge_dicts(
        options{anon_prefix: BNodePrefix, base_iri: BaseIri, format: ntriples},
        Options1,
        Options2
      ),
      dict_terms(Options2, Options3),
      rdf_ntriples:rdf_process_ntriples(In2, Mod:Goal_2, Options3)
  ;   % RDF/XML
      MediaType = media(application/'rdf+xml',_)
  ->  merge_dicts(options{base_iri: BaseIri, max_errors: -1}, Options1, Options2),
      dict_terms(Options2, Options3),
      rdf:process_rdf(In2, Mod:Goal_2, Options3)
  ;   % TriG
      MediaType = media(application/trig,_)
  ->  merge_dicts(
        options{
          anon_prefix: BNodePrefix,
          base_iri: BaseIri,
          format: trig,
          resources: iri
        },
        Options1,
        Options2
      ),
      dict_change_keys(Options2, [base_iri-base_uri], Options3),
      dict_terms(Options3, Options4),
      turtle:rdf_process_turtle(In2, Mod:Goal_2, Options4)
  ;   % Turtle
      MediaType = media(text/turtle,_)
  ->  merge_dicts(
        options{
          anon_prefix: BNodePrefix,
          base_iri: BaseIri,
          format: turtle,
          resources: iri
        },
        Options1,
        Options2
      ),
      dict_terms(Options2, Options3),
      turtle:rdf_process_turtle(In2, Mod:Goal_2, Options3)
  ;   % RDFa
      memberchk(MediaType, [media(application/'xhtml+xml',_),media(text/html,_)])
  ->  merge_dicts(
        options{anon_prefix: BNodePrefix, base: BaseIri, max_errors: -1},
        Options1,
        Options2
      ),
      dict_terms(Options2, Options3),
      rdfa:read_rdfa(In2, Triples, Options3),
      call(Mod:Goal_2, Triples, _)
  %;   % JSON-LD
  %    memberchk(MediaType, [media(application/'ld+json',_)])
  %->  read_jsonld(In2, Triples),
  %    call(Mod:Goal_2, Triples, _)
  ;   % An unsupported Media Type.
      print_message(warning, unsupported_format(MediaType,_))
  ).



%! rdf_deref_uri(+Uri:uri, :Goal_3) is det.
%! rdf_deref_uri(+Uri:uri, :Goal_3, +Options:options) is det.
%
% @param Options The following options are supported:
%
%        * accept(+MediaTypes:list(compound))
%
%          The value of the HTTP Accept header, from high to low
%          precedence.  The default value is a list of all and only
%          standardized Media Types.
%
%   * Other options are passed to rdf_deref_stream/4.

rdf_deref_uri(Uri, Goal_3) :-
  rdf_deref_uri(Uri, Goal_3, options{}).


rdf_deref_uri(Uri, Goal_3, Options1) :-
  % ‘Accept’ header
  (   options{accept: MediaTypes} :< Options1
  ->  true
  ;   aggregate_all(set(MediaType0), media_type_family(MediaType0, rdf), MediaTypes)
  ),
  http_open2(Uri, In, options{accept: MediaTypes, failure: 404, metadata: Metas}),
  call_cleanup(
    (
      (   http_metadata_content_type(Metas, MediaType)
      ->  merge_dicts(Options1, options{content_type: MediaType}, Options2)
      ;   Options2 = Options1
      ),
      rdf_deref_stream(Uri, In, Goal_3, Options2)
    ),
    close(In)
  ).



% GENERICS %

%! 'rdf_media_type_>'(+SuperMediaType:media_type, +SubMediaType:media_type) is semidet.
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
