:- module(
  rdf_stream,
  [
    rdf_stream_read/2, % +In
                       % :Goal_2
    rdf_stream_read/3, % +In
                       % :Goal_2
                       % +Options:list(compound)
    rdf_stream_write/2, % +Out
                        % :Goal_2
    rdf_stream_write/3 % +Out
                       % :Goal_2
                       % +Options:list(compound)
  ]
).

/** <module> RDF stream

@author Wouter Beek
@version 2015/08-2015/10
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_debug)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(debug)).
:- use_module(library(dict_ext)).
:- use_module(library(error)).
:- use_module(library(open_any2)).
:- use_module(library(os/archive_ext)).
:- use_module(library(rdf/rdf_guess)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(typecheck)).
:- use_module(library(uri)).
:- use_module(library(zlib)).

:- meta_predicate(rdf_stream_read(+,2)).
:- meta_predicate(rdf_stream_read(+,2,+)).
:- meta_predicate(rdf_stream_read0(2,+,+,+)).
:- meta_predicate(rdf_stream_write(+,2)).
:- meta_predicate(rdf_stream_write(+,2,+)).

:- predicate_options(rdf_stream_read/3, 3, [
     format(+oneof([nquads,ntriples,trig,triples,turtle,xml]))
   ]).
:- predicate_options(rdf_stream_write/3, 3, [
     compress(+oneof([deflate,gzip])),
     pass_to(open_any/5, 5)
   ]).





%! rdf_stream_read(+In, :Goal_2, +Options:list(compound)) is det.
% Wrapper around rdf_stream_read/3 with default options.

rdf_stream_read(In, Goal_2):-
  rdf_stream_read(In, Goal_2, []).


%! rdf_stream_read(+In, :Goal_2, +Options:list(compound)) is det.
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
%   * archive_entry(+dict)
%   * base_iri(+atom)
%   * format(+oneof([nquads,ntriples,trig,triples,turtle,xml]))

rdf_stream_read(In, Goal_2, Opts):-
  rdf_http_plugin:rdf_extra_headers(RdfOpts, Opts),
  setup_call_cleanup(
    open_any2(In, read, Read, Close_0, [metadata(M)|RdfOpts]),
    call_archive_entry(Read, rdf_stream_read0(Goal_2, [metadata(M)|Opts])),
    close_any2(Close_0)
  ).

rdf_stream_read0(Goal_2, Opts, MCompress, Read):-
  rdf_determine_format(Read, Opts, RdfFormat),
  M = metadata{compression: MCompress, rdf: metadata{format: RdfFormat}},
  call(Goal_2, M, Read).



%! rdf_stream_write(+Out, :Goal_2) is det.
% Wrapper around rdf_stream_write/3 with default options.

rdf_stream_write(Out, Goal_2):-
  rdf_stream_write(Out, Goal_2, []).

  
%! rdf_stream_write(+Out, :Goal_2, +Options:list(compound)) is det.

rdf_stream_write(Out, Goal_2, Opts0):- !,
  merge_options([metadata(M1)], Opts0, Opts),
  setup_call_cleanup(
    open_any2(Out, write, Write0, Close_0, Opts),
    (
      (   option(compress(Comp), Opts),
          must_be(oneof([deflate,gzip]), Comp)
      ->  zopen(Write0, Write, [format(Comp)]),
          put_dict(compress, M1, Comp, M2)
      ;   Write = Write0,
          M2 = M1
      ),
      call(Goal_2, M2, Write)
    ),
    close_any(Close_0)
  ).





% HELPERS %

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
