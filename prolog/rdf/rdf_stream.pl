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
@version 2015/08
*/

:- use_module(library(archive)).
:- use_module(library(error)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(iostream)).
:- use_module(library(rdf/rdf_guess)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(zlib)).

:- meta_predicate(rdf_stream_read(+,2,+)).
:- meta_predicate(rdf_stream_write(+,1,+)).





%! rdf_stream_read(+Spec, :Goal_2, +Options:list(compound)) is det.

rdf_stream_read(Spec, Goal_2, Opts):-
  rdf_http_plugin:rdf_extra_headers(HttpOpts, Opts),
  ArchOpts = [close_parent(false),format(all),format(raw)],
  setup_call_cleanup(
    open_any(Spec, read, Read0, Close, HttpOpts),
    setup_call_cleanup(
      archive_open(Read0, Arch, ArchOpts),
      (
        % NONDET
        archive_data_stream(Arch, Read, [meta_data(_)]),
        call_cleanup(
          (
            rdf_determine_format(Read, Opts, Format),
            call(Goal_2, Format, Read)
          ),
          close(Read)
        )
      ),
      archive_close(Arch)
    ),
    close_any(Close)
  ).



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
  option(format(Format), Opts),
  (   ground(Format)
  ->  true
  ;   rdf_guess_format(Read, Format)
  ).
rdf_determine_format(Read, _, Format):-
  rdf_guess_format(Read, Format).
