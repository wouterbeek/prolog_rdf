:- module(
  rdf_stream,
  [
    call_on_rdf/2,       % +UriSpec, :Goal_2
    call_on_rdf/3,       % +UriSpec, :Goal_2, +Options
    rdf_deref_quad/2,    % +Uri, -Quad
    rdf_deref_quad/3,    % +Uri, -Quad, +Options
    rdf_deref_triple/2,  % +Uri, -Triple
    rdf_deref_triple/3,  % +Uri, -Triple, +Options
    rdf_reserialize/2,   % +Uri, +FileSpec
    rdf_reserialize/3,   % +Kind, +Uri, +FileSpec
    rdf_reserialize/4,   % +Kind, +Uri, +FileSpec, +Options
    rdf_to_hdt/2         % +UriSpec, +HdtFile
  ]
).

/** <module> RDF stream

Streamed processing of RDF data.

@author Wouter Beek
@version 2017/09
*/

:- use_module(library(call_ext)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(file_ext)).
:- use_module(library(http/rfc7231)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_ext)).
:- use_module(library(semweb/rdf_guess)).
:- use_module(library(semweb/rdf_http_plugin), []).
:- use_module(library(semweb/rdf_ntriples)).
:- use_module(library(semweb/rdfa)).
:- use_module(library(semweb/turtle)).
:- use_module(library(uri/uri_ext)).

:- meta_predicate
    call_on_rdf(+, 2),
    call_on_rdf(+, 2, +).

:- rdf_meta
   rdf_deref_quad(r, t),
   rdf_deref_quad(r, t, +),
   rdf_deref_triple(r, t),
   rdf_deref_triple(r, t, +).





%! call_on_rdf(+UriSpec:term, :Goal_2) is nondet.
%! call_on_rdf(+UriSpec:term, :Goal_2, +Options:list(compound)) is nondet.
%
% Makes the call `Goal_2(+Tuple, ?G)'.
%
% The following options are supported:
%
%   * accept(+list(compound))
%
%     The value of the HTTP Accept header, from high to low
%     precedence.  The default value is a list of all and only
%     standardized Media Types.
%
%   * base_uri(+atom)
%
%     The default is the URI of the last metadata element.
%
%   * bnode_prefix(+atom)
%
%     The default is a well-known IRI as per RDF 1.1.
%
%   * Other options are passed to call_on_uri/3 and
%     rdf_process_ntriples/3.

call_on_rdf(UriSpec, Goal_2) :-
  call_on_rdf(UriSpec, Goal_2, []).


call_on_rdf(UriSpec, Goal_2, Options1) :-
  findall(MediaType, rdf_media_type(MediaType), DefaultMediaTypes),
  select_option(accept(MediaTypes), Options1, Options2, DefaultMediaTypes),
  atom_phrase(accept(MediaTypes), Accept),
  merge_options(Options2, [request_header('Accept'=Accept)], Options3),
  call_on_uri(UriSpec, call_on_rdf_stream(Goal_2, Options1), Options3).

call_on_rdf_stream(Goal_2, Options1, In, [Dict1|Metadata], [Dict2|Metadata]) :-
  % Guess the Media Type based on peeking inside the stream.
  rdf_guess(In, MediaTypes),
  (memberchk(MediaType1, MediaTypes) -> true ; throw(error(rdf_unknown,_))),

  % Compare the guessed Media Type to the value of the last
  % `Content-Type' header.
  (   metadata_content_type(Metadata, MediaType2)
  ->  media_type_warning(MediaType1, MediaType2)
  ;   true
  ),

  % Compare the guessed Media Type to the URI's path component.
  metadata_uri(Metadata, Uri),
  (   uri_media_type(Uri, MediaType3)
  ->  media_type_warning(MediaType1, MediaType3)
  ;   true
  ),
  
  % Determine the base URI.
  option(base_uri(BaseUri), Options1, Uri),
  
  % Determine the blank node prefix.
  call_default_option(
    bnode_prefix(BNodePrefix),
    Options1,
    rdf_create_bnode_iri
  ),
  
  % Parse according to the guessed Media Type.

  (   % N-Quads
      MediaType1 = media(application/'n-quads',_)
  ->  merge_options(
        [anon_prefix(BNodePrefix),base_uri(BaseUri),format(nquads)],
        Options1,
        Options2
      ),
      rdf_process_ntriples(In, Goal_2, Options2)
  ;   % N-Triples
      MediaType1 = media(application/'n-triples',_)
  ->  merge_options(
        [anon_prefix(BNodePrefix),base_uri(BaseUri),format(ntriples)],
        Options1,
        Options2
      ),
      rdf_process_ntriples(In, Goal_2, Options2)
  ;   % RDF/XML
      MediaType1 = media(application/'rdf+xml',_)
  ->  merge_options(
        [base_uri(BaseUri),blank_nodes(noshare)],
        Options1,
        Options2
      ),
      process_rdf(In, Goal_2, Options2)
  ;   % TriG
      MediaType1 = media(application/trig,_)
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
      rdf_process_turtle(In, Goal_2, Options2)
  ;   % Turtle
      MediaType1 = media(text/turtle,_)
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
      rdf_process_turtle(In, Goal_2, Options2)
  ;   % RDFa
      memberchk(
        MediaType1,
        [media(application/'xhtml+xml',_),media(text/html,_)]
      )
  ->  merge_options(
        [anon_prefix(BNodePrefix),base(BaseUri)],
        Options1,
        Options2
      ),
      read_rdfa(In, Triples, Options2),
      maplist(Goal_2, Triples, _)
  ;   % An unsupported Media Type (e.g., JSON-LD).
      print_message(warning, unsupported_media_type(MediaType1))
  ),
  dict_put(media_type, Dict1, MediaType1, Dict2).

media_type_warning(MediaType1, MediaType2) :-
  'rdf_media_type_>'(MediaType1, MediaType2), !.
media_type_warning(MediaType1, MediaType2) :-
  print_message(warning, different_media_type(MediaType1,MediaType2)).

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

% Ordering represents precedence, from lower to hgiher.
rdf_media_type(media(application/'json-ld',[])).
rdf_media_type(media(application/'rdf+xml',[])).
rdf_media_type(media(text/turtle,[])).
rdf_media_type(media(application/'n-triples',[])).
rdf_media_type(media(application/trig,[])).
rdf_media_type(media(application/'n-quads',[])).



%! rdf_deref_quad(+UriSpec:compound, -Quad:compound) is nondet.
%! rdf_deref_quad(+UriSpec:compound, -Quad:compound,
%!                +Options:list(compound)) is nondet.

rdf_deref_quad(UriSpec, Quad) :-
  rdf_deref_quad(UriSpec, Quad, []).


rdf_deref_quad(UriSpec, Quad, Options) :-
  call_on_rdf(UriSpec, rdf_deref_quad_(Quad), Options).

rdf_deref_quad_(Quad, Tuples, _) :-
  member(Tuple, Tuples),
  rdf_clean_tuple(Tuple, Quad).



%! rdf_deref_triple(+UriSpec:term, -Triple:compound) is nondet.
%! rdf_deref_triple(+UriSpec:term, -Triple:compound,
%!                  +Options:list(compound)) is nondet.
%
% Options are passed to rdf_deref_quad/3.

rdf_deref_triple(UriSpec, Triple) :-
  rdf_deref_triple(UriSpec, Triple, []).


rdf_deref_triple(UriSpec, Triple, Options) :-
  rdf_deref_quad(UriSpec, Quad, Options),
  rdf_tuple_triple(Quad, Triple).



%! rdf_reserialize(+UriSpec:term, +FileSpec:term) is nondet.
%! rdf_reserialize(+Kind:oneof([quads,triples]), +UriSpec:term,
%!                 +FileSpec:term) is nondet.
%! rdf_reserialize(+Kind:oneof([quads,triples]), +UriSpec:term, +FileSpec:term,
%!                 +Options:list(compound)) is nondet.
%
% Reserializes RDF data from URI to N-Quads FileSpec.
%
% Options are passed to call_to_file/3 and call_on_rdf/3.

rdf_reserialize(UriSpec, FileSpec) :-
  rdf_reserialize(quads, UriSpec, FileSpec).


rdf_reserialize(Kind, UriSpec, FileSpec) :-
  rdf_reserialize(Kind, UriSpec, FileSpec, []).


rdf_reserialize(Kind, UriSpec, FileSpec, Options) :-
  call_to_file(FileSpec, rdf_reserialize_(Kind, UriSpec, Options), Options).

rdf_reserialize_(Kind, UriSpec, Options, Out, Meta, Meta) :-
  forall(call_on_rdf(UriSpec, write_ntuples_(Kind, Out), Options), true).

write_ntuples_(Kind, Out, Tuples, _) :-
  convlist(rdf_clean_tuple, Tuples, Quads),
  maplist(write_ntuple_(Kind, Out), Quads).

write_ntuple_(quads, Out, Quad) :- !,
  write_nquad(Out, Quad).
write_ntuple_(triples, Out, Quad) :-
  write_ntriple(Out, Quad).



%! rdf_to_hdt(+UriSpec:term, +FileSpec:term) is det.

rdf_to_hdt(UriSpec, FileSpec) :-
  absolute_file_name(FileSpec, File, [access(write)]),

  % Convert to uncompressed N-Triples.
  debug(semweb(rdf_to_hdt), "Creating uncompressed N-Triples…", []),
  create_temporary_file1(TriplesFileTmp),
  rdf_reserialize(triples, UriSpec, TriplesFileTmp, [compression(none)]),
  debug(semweb(rdf_to_hdt), "…uncompressed N-Triples created.", []),

  % Create HDT file.
  debug(semweb(rdf_to_hdt), "Creating HDT…", []),
  file_name_extension(File, working, HdtFile),
  hdt_create(TriplesFileTmp, [hdt_file(HdtFile)]),
  with_mutex(rdf_to_hdt,
    (   % Somebody else was earlier.
        exists_file(File)
    ->  delete_file(HdtFile)
    ;   rename_file(HdtFile, File)
    )
  ),
  delete_file(TriplesFileTmp),
  debug(semweb(rdf_to_hdt), "…HDT created.", []),

  % Create HDT index file.
  debug(semweb(rdf_to_hdt), "Creating HDT index…", []),
  hdt_call_on_file(File, [Hdt]>>once(hdt(_,_,_,Hdt))),
  debug(semweb(rdf_to_hdt), "…HDT index created.", []).
  
create_temporary_file1(File) :-
  uuid(Uuid),
  absolute_file_name(Uuid, File, [access(write),extensions([tmp])]).
