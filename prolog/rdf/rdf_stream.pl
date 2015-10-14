:- module(
  rdf_stream,
  [
    base_iri/2, % +Input:atom
                % -BaseIri:atom
    rdf_stream_read/3, % +Spec
                       % :Goal_2
                       % +Options:list(compound)
    rdf_stream_write/3 % +Spec
                       % :Goal_1
                       % +Options:list(compound)
  ]
).

/** <module> RDF stream

@author Wouter Beek
@version 2015/08-2015/10
*/

:- use_module(library(apply)).
:- use_module(library(archive)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_debug)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(debug)).
:- use_module(library(dict_ext)).
:- use_module(library(error)).
:- use_module(library(http/http_cookie)).
:- use_module(library(http/http_deb)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(iostream)).
:- use_module(library(rdf/rdf_guess)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(typecheck)).
:- use_module(library(uri)).
:- use_module(library(zlib)).

:- meta_predicate(rdf_stream_read(+,2,+)).
:- meta_predicate(rdf_stream_write(+,1,+)).

http_open:ssl_verify(_SSL, _ProblemCert, _AllCerts, _FirstCert, _Error).

:- predicate_options(base_iri/3, 3, [
     base_iri(+atom)
   ]).
:- predicate_options(rdf_stream_read/3, 3, [
     pass_to(base_iri/3, 3),
     format(+atom),
     metadata(-dict)
   ]).





%! rdf_stream_read(+Spec, :Goal_2, +Options:list(compound)) is det.
% Goal_2 is applied to a read stream and a metadata dictionary.
% The metadata dictionary consists of:
%   * base_iri: atom
%   * compression:
%     * filetype: oneof([block_device,
%                        character_device,
%                        directory,
%                        file,
%                        fifo,
%                        link,
%                        socket])
%     * filters: list(oneof([all,
%                            bzip2,
%                            compress,
%                            gzip,
%                            grzip,
%                            lrzip,
%                            lzip,
%                            lzma,
%                            lzop,
%                            none,
%                            rpm,
%                            uu,
%                            xz]))
%     * format: oneof(['7zip',
%                      all,
%                      ar,
%                      cab,
%                      cpio,
%                      empty,
%                      gnutar,
%                      iso9660,
%                      lha,
%                      mtree,
%                      rar,
%                      raw,
%                      tar,
%                      xar,
%                      zip])
%     * link_target: atom
%     * mtime: float
%     * name: atom
%     * size: nonneg
%   * http:
%      * final_iri: atom
%      * headers:
%        * ...
%      * status_code: between(100,599)
%      * version: pair(nonneg)
%   * input_type: oneof([file,iri])
%   * rdf:
%     * format: atom
%
% The following options are supported:
%   * base_iri(+atom)
%   * format(+atom)
%   * metadata(-dict)

rdf_stream_read(Spec, Goal_2, Opts):-
  % Determine the base IRI.
  base_iri(Spec, BaseIri, Opts),

  % Allow meta-data to be returned by this predicate.
  ignore(option(metadata(M), Opts)),

  % Set HTTP options.
  rdf_http_plugin:rdf_extra_headers(HttpOpts1, Opts),
  HttpOpts2 = [
    final_url(_FinalIri),
    headers(_HttpHeaders),
    version(_HttpVersion),
    status_code(_HttpStatusCode),
    user_agent(plRdf)
  ],
  merge_options(HttpOpts1, HttpOpts2, HttpOpts),

  % Set archive options.
  ArchOpts = [close_parent(false),format(all),format(raw)],

  setup_call_cleanup(
    open_any(Spec, read, Read0, Close, HttpOpts),
    (   is_http_error0(HttpOpts)
    ->  memberchk(status_code(StatusCode), HttpOpts),
        http_status_label(StatusCode, Label),
        throw(
          error(
            permission_error(url,Spec),
            context(_,status(StatusCode,Label))
          )
        )
    ;   setup_call_cleanup(
          archive_open(Read0, Arch, ArchOpts),
          forall(
            archive_data_stream(Arch, Read, [meta_data(MCompress)]),
            call_cleanup(
              (
                rdf_determine_format(Read, Opts, Format),
                create_metadata0(BaseIri, HttpOpts2, MCompress, Format, M),
                call(Goal_2, Read, M)
              ),
              close(Read)
            )
          ),
          archive_close(Arch)
        )
    ),
    close_any(Close)
  ).



%! base_iri0(+Input, -BaseIri:atom, +Options:list(compound)) is det.
% Succeeds if BaseIri is the base IRI of Input.
%
% The base IRI can be determined in the following ways:
%   1. The user sets the base IRI in Options.
%   2. Input if it is an IRI, without the fragment component.
%   3. Input if it is a file name, in which case it is converted
%      to an IRI of scheme `file'.
%
% The following options are supported:
%   * base_iri(+atom)
%     The base IRI that is explicitly set by the user.
%
% @throws existence_error if no base IRI can be determined.

% Explicitly specified option takes precedence.
base_iri0(_, BaseIri, Opts):-
  option(base_iri(BaseIri), Opts), !,
  dcg_debug(rdf_stream, base_iri0("Explicitly set by user", BaseIri)).
% Stream without option throws warning.
base_iri0(Stream, _, _):-
  is_stream(Stream), !,
  existence_error(base_iri, Stream).
base_iri0(Input, BaseIri, _):-
  base_iri(Input, BaseIri),
  dcg_debug(rdf_stream, base_iri0("Based on input IRI", BaseIri)).

base_iri0(Msg, BaseIri) -->
  "[Base IRI] ",
  Msg,
  ": ",
  iri(BaseIri).



%! is_http_error0(+Options:list(compound)) is semidet.
% Succeeds if Options contains an HTTP status code
% that describes an error condition.

is_http_error0(Opts):-
  option(status_code(StatusCode), Opts),
  ground(StatusCode),
  between(400, 599, StatusCode).



%! create_metadata(
%!   +BaseIri:atom,
%!   +HttpOptions:list(compound),
%!   +CompressionMetadata:dict,
%!   +RdfFormat:atom,
%!   -Metadata:dict
%! ) is det.

% Metadata for an input file.
create_metadata0(BaseIri, _, MCompress, Format, M):-
  is_file_iri(BaseIri), !,
  M = metadata{
	base_iri: BaseIri,
	compression: MCompress,
	input_type: file,
	rdf: metadata{format: Format}
      }.
% Metadata for an input IRI.
create_metadata0(BaseIri, HttpOpts, MCompress, Format, M):-
  create_http_metadata0(HttpOpts, MHttp),
  M = metadata{
	base_iri: BaseIri,
	compression: MCompress,
	http: MHttp,
	input_type: iri,
	rdf: metadata{format: Format}
      }.


%! create_http_metadata0(+Options:list(compound), -Metadata:dict) is det.

create_http_metadata0(Opts, M):-
  Opts = [
    final_url(FinalIri),
    headers(Headers),
    version(Version),
    status_code(StatusCode),
    user_agent(_)
  ],
  create_grouped_sorted_dict(Headers, http_headers, MHeaders),
  M = metadata{
	final_iri: FinalIri,
	headers: MHeaders,
	status_code: StatusCode,
	version: Version
      }.



%! rdf_stream_write(+Spec, :Goal_1, +Options:list(compound)) is det.

rdf_stream_write(Spec, Goal_1, Opts):- !,
  setup_call_cleanup(
    open_any(Spec, write, Write0, Close, Opts),
    (
      (   option(compress(Comp), Opts),
          must_be(oneof([deflate,gzip]), Comp)
      ->  zopen(Write0, Write, [format(Comp)])
      ;   Write = Write0
      ),
      call(Goal_1, Write)
    ),
    close_any(Close)
  ).





% HELPERS %

%! base_iri(+Input:atom, -BaseIri:atom) is det.

% The IRI that is read from, sans the fragment component.
base_iri(Iri, BaseIri):-
  is_uri(Iri), !,
  % Remove the fragment part, if any.
  uri_components(Iri, uri_components(Scheme,Auth,Path,Query,_)),
  uri_components(BaseIri, uri_components(Scheme,Auth,Path,Query,_)).
% The file is treated as an IRI.
base_iri(File, BaseIri):-
  uri_file_name(Iri, File),
  base_iri(Iri, BaseIri).



%! is_file_iri(+Iri:atom) is semidet.
% Succeeds if Iri is syntactically an IRI that denotes a file.

is_file_iri(Iri):-
  uri_components(Iri, uri_components(file,_,_,_,_)).



%! rdf_determine_format(
%!   +Read:stream,
%!   +Options:list(compound),
%!   -Format:rdf_format
%! ) is det.
% Succeeds if Format is either supplied in Options
% or is heuristically detected in the Read stream
% and then also optionally returned as an option.

rdf_determine_format(Read, Opts, Format):-
  (   option(format(Format), Opts)
  ->  (   ground(Format)
      ->  true
      ;   rdf_guess_format(Read, Format)
      )
  ;   rdf_guess_format(Read, Format)
  ).
