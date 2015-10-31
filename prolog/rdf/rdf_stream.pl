:- module(
  rdf_stream,
  [
    rdf_call_on_stream/3, % +In, +Mode, :Goal_2
    rdf_call_on_stream/4 % +In
                         % +Mode:oneof([append,read,write])
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
:- use_module(library(option)).
:- use_module(library(os/archive_ext)).
:- use_module(library(os/call_on_stream)).
:- use_module(library(os/open_any2)).
:- use_module(library(rdf/rdf_guess)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(typecheck)).
:- use_module(library(uri)).
:- use_module(library(zlib)).

:- meta_predicate(rdf_call_on_stream(+,+,2)).
:- meta_predicate(rdf_call_on_stream(+,+,2,+)).
:- meta_predicate(rdf_call_on_read_stream(2,+,+,+)).
:- meta_predicate(rdf_call_on_write_stream(2,+,+)).

:- predicate_options(rdf_call_on_stream/4, 4, [
     pass_to(call_on_stream/4, 4),
     pass_to(rdf_call_on_read_stream/4, 2),
     pass_to(rdf_http_plugin:rdf_extra_headers/2, 2)
   ]).
:- predicate_options(rdf_call_on_read_stream/4, 2, [
     format(+oneof([nquads,ntriples,trig,triples,turtle,xml]))
   ]).





%! rdf_call_on_stream(+In, +Mode:oneof([append,read,write]), :Goal_2) is det.
% Wrapper around rdf_call_on_stream/4.

rdf_call_on_stream(In, Mode, Goal_2):-
  rdf_call_on_stream(In, Mode, Goal_2, []).


%! rdf_call_on_stream(
%!   +In,
%!   +Mode:oneof([append,read,write]),
%!   :Goal_2,
%!   +Options:list(compound)
%! ) is det.
% Goal_2 is applied to a read stream and a metadata dictionary.
% The metadata dictionary consists of:
%   * base_iri: atom
%   * compress:
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

% Read mode.
rdf_call_on_stream(In, Mode, Goal_2, Opts1):-
  read_mode(Mode), !,
  % Accept headers for RDF are specified in `library(semweb/rdf_http_plugin))'.
  rdf_http_plugin:rdf_extra_headers(DefaultRdfOpts, Opts1),
  merge_options(DefaultRdfOpts, Opts1, Opts2),
  
  % Remove option format/1 as it will confuse archive_open/3.
  % Archive format ↔ RDF serialization format
  (select_option(format(_), Opts2, Opts3) -> true ; Opts3 = Opts2),
  
  call_on_stream(In, Mode, rdf_call_on_read_stream(Goal_2, Opts2), Opts3).
% Write mode.
rdf_call_on_stream(In, Mode, Goal_2, Opts):-
  write_mode(Mode), !,
  call_on_stream(In, Mode, rdf_call_on_write_stream(Goal_2), Opts).
rdf_call_on_stream(_, Mode, _, _):-
  domain_error(oneof([append,read,write]), Mode).
  

rdf_call_on_read_stream(Goal_2, Opts, M1, Read):-
  % Guess the RDF serialization format in case option `format(+)'
  % is not given.
  (   option(format(Format), Opts),
      ground(Format)
  ->  true
  ;   rdf_guess_format(Read, Format)
  ),
  % `Format' is now instantiated.
  put_dict(rdf, M1, metadata{format: Format}, M2),
  call(Goal_2, M2, Read).


rdf_call_on_write_stream(Goal_2, M, Write):-
  call(Goal_2, M, Write).
