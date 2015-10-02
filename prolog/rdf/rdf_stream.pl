:- module(
  rdf_stream,
  [
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
:- use_module(library(dcg/dcg_phrase)).
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

:- predicate_options(rdf_stream_read/3, 3, [
     base_iri(+atom),
     format(+atom),
     meta_data(-dict)
   ]).





%! rdf_stream_read(+Spec, :Goal_2, +Options:list(compound)) is det.
% The following options are supported:
%   * base_iri(+atom)
%   * format(+atom)
%   * meta_data(-dict)

rdf_stream_read(Spec, Goal_2, Opts):-
  % Determine the base IRI.
  rdf_base_iri(Spec, BaseIri, Opts),

  % Allow meta-data to be returned.
  ignore(option(meta_data(M), Opts)),

  % HTTP options.
  rdf_http_plugin:rdf_extra_headers(HttpOpts1, Opts),
  HttpOpts2 = [
    final_url(_FinalIri),
    headers(_HttpHeaders),
    version(_HttpVersion),
    status_code(_HttpStatusCode),
    user_agent(plRdf)
  ],
  merge_options(HttpOpts1, HttpOpts2, HttpOpts),

  % Archive options.
  ArchOpts = [close_parent(false),format(all),format(raw)],

  setup_call_cleanup(
    open_any(Spec, read, Read0, Close, HttpOpts),
    (   is_http_error0(HttpOpts)
    ->  memberchk(status_code(HttpStatusCode), HttpOpts),
        http_status_label(HttpStatusCode, Label),
        throw(
          error(
            permission_error(url,Spec),
            context(_,status(HttpStatusCode,Label))
          )
        )
    ;   setup_call_cleanup(
          archive_open(Read0, Arch, ArchOpts),
          forall(
            archive_data_stream(Arch, Read, [meta_data(MCompress)]),
            call_cleanup(
              (
                rdf_determine_format(Read, Opts, Format),
                meta_data0(BaseIri, HttpOpts2, MCompress, Format, M),
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

% Explicitly specified option takes precedence.
rdf_base_iri(_, BaseIri, Opts):-
  option(base_iri(BaseIri), Opts), !.
% Stream without option throws warning.
rdf_base_iri(Stream, _, _):-
  is_stream(Stream), !,
  existence_error(base_iri, Stream).
% The IRI that is read from, sans the fragment component.
rdf_base_iri(Iri, BaseIri, _):-
  is_uri(Iri), !,
  % Remove the fragment part, if any.
  uri_components(Iri, uri_components(Scheme,Auth,Path,Query,_)),
  uri_components(BaseIri, uri_components(Scheme,Auth,Path,Query,_)).
% The file is treated as an IRI.
rdf_base_iri(File, BaseIri, Opts):-
  uri_file_name(Iri, File),
  rdf_base_iri(Iri, BaseIri, Opts).

is_http_error0(HttpOpts):-
  option(status_code(HttpStatusCode), HttpOpts),
  nonvar(HttpStatusCode),
  between(400, 599, HttpStatusCode), !.

meta_data0(BaseIri, _, MCompress, Format, M):-
  is_file_iri(BaseIri), !,
  M = meta_data{base_iri: BaseIri, compression: MCompress, input_type: file, rdf_format: Format}.
meta_data0(BaseIri, HttpOpts, MCompress, Format, M):-
  http_meta_data0(HttpOpts, MHttp),
  M = meta_data{base_iri: BaseIri, compression: MCompress, http: MHttp, input_type: iri, rdf_format: Format}.

http_meta_data0(HttpOpts, HttpM):-
  HttpOpts = [
    final_url(FinalIri),
    headers(HttpHeaders1),
    version(HttpVersion),
    status_code(HttpStatusCode),
    user_agent(_)
  ],
  sort(HttpHeaders1, HttpHeaders2),
  group_pairs_by_key(HttpHeaders2, HttpHeaders3),
  dict_pairs(MHeaders, http_headers, HttpHeaders3),
  HttpM = http_meta_data{
	    final_iri: FinalIri,
	    headers: MHeaders,
	    version: HttpVersion,
	    status_code: HttpStatusCode
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

%! is_file_iri(+Iri:atom) is semidet.

is_file_iri(Iri):-
  uri_components(Iri, uri_components(file,_,_,_,_)).



%! rdf_determine_format(
%!   +Read:stream,
%!   +Options:list(compound),
%!   -Format:rdf_format
%! ) is det.

rdf_determine_format(Read, Opts, Format):-
  (   option(format(Format), Opts)
  ->  (   ground(Format)
      ->  true
      ;   rdf_guess_format(Read, Format)
      )
  ;   rdf_guess_format(Read, Format)
  ).
