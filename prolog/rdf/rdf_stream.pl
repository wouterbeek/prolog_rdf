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
@version 2015/08-2015/09
*/

:- use_module(library(apply)).
:- use_module(library(archive)).
:- use_module(library(error)).
:- use_module(library(http/http_cookie)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(iostream)).
:- use_module(library(rdf/rdf_guess)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(uri)).
:- use_module(library(zlib)).

:- meta_predicate(rdf_stream_read(+,2,+)).
:- meta_predicate(rdf_stream_write(+,1,+)).

http_open:ssl_verify(_SSL, _ProblemCert, _AllCerts, _FirstCert, _Error).

:- predicate_options(rdf_stream_read/3, 3, [
     format(+atom),
     meta_data(-dict)
   ]).





%! rdf_stream_read(+Spec, :Goal_2, +Options:list(compound)) is det.
% The following options are supported:
%   * format(+atom)
%   * meta_data(-dict)

rdf_stream_read(Spec, Goal_2, Opts):-
  % Allow meta-data to be returned.
  ignore(option(meta_data(M), Opts)),
  what_is0(Spec, InputType),

  % HTTP options.
  rdf_http_plugin:rdf_extra_headers(HttpOpts1, Opts),
  HttpOpts2 = [
    final_url(_FinalIri),
    headers(_HttpHeaders),
    version(_HttpVersion),
    status_code(_HttpStatusCode)
  ],
  merge_options(HttpOpts1, HttpOpts2, HttpOpts),

  % Archive options.
  ArchOpts = [close_parent(false),format(all),format(raw)],

  setup_call_cleanup(
    open_any(Spec, read, Read0, Close, HttpOpts),
    (   is_http_error0(HttpOpts)
    ->  MCompress = []
    ;   setup_call_cleanup(
          archive_open(Read0, Arch, ArchOpts),
          (
            % NONDET
            archive_data_stream(Arch, Read, [meta_data(MCompress)]),
            call_cleanup(
              (
                rdf_determine_format(Read, Opts, Format),
                call(Goal_2, Format, Read)
              ),
              close(Read)
            )
          ),
          archive_close(Arch)
        )
    ),
    close_any(Close)
  ),
  meta_data0(InputType, HttpOpts2, MCompress, M).

is_http_error0(HttpOpts):-
  option(status_code(HttpStatusCode), HttpOpts),
  between(400, 599, HttpStatusCode), !.

what_is0(X, stream):-
  is_stream(X), !.
what_is0(stream(_), stream):- !.
what_is0(file(_), file):- !.
what_is0(Iri, iri):-
  uri_components(Iri, uri_components(Scheme,Auth,_,_,_)),
  maplist(atom, [Scheme,Auth]), !.
what_is0(_, file).

meta_data0(InputType, HttpOpts, MCompress, M):-
  http_meta_data0(HttpOpts, MHttp),
  M = meta_data{compression: MCompress, input_type: InputType, http: MHttp}.

http_meta_data0(HttpOpts, HttpM):-
  HttpOpts = [
    final_url(FinalIri),
    headers(HttpHeaders1),
    version(HttpVersion),
    status_code(HttpStatusCode)
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
