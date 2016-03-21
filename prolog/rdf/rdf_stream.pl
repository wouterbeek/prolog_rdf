:- module(
  rdf_stream,
  [
    rdf_read_from_stream/2, % +Source, :Goal_2
    rdf_read_from_stream/3, % +Source, :Goal_2, +Opts
    rdf_write_to_stream/2,  % +Source, :Goal_2
    rdf_write_to_stream/3   % +Source, :Goal_2, +Opts
  ]
).

/** <module> RDF stream

@author Wouter Beek
@version 2015/08-2016/02
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(error)).
:- use_module(library(jsonld/jsonld_metadata)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(os/archive_ext)).
:- use_module(library(os/call_on_stream)).
:- use_module(library(os/open_any2)).
:- use_module(library(rdf/rdf_file)). % Type definition.
:- use_module(library(rdf/rdf_guess)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(typecheck)).
:- use_module(library(uri/uri_ext)).
:- use_module(library(zlib)).

:- meta_predicate
   rdf_read_from_stream(+,2),
   rdf_read_from_stream(+,2,+),
   rdf_write_to_stream(+,2),
   rdf_write_to_stream(+,2,+),
   rdf_read_from_stream0(2,+,+,+).

:- predicate_options(rdf_read_from_stream/3, 3, [
     pass_to(rdf_read_from_stream0/4, 2),
     pass_to(rdf_http_plugin:rdf_extra_headers/2, 2),
     pass_to(read_from_stream/4, 4)
   ]).
:- predicate_options(rdf_read_from_stream0/4, 2, [
     rdf_format(+rdf_format),
     pass_to(rdf_guess_format/3, 3)
   ]).
:- predicate_options(rdf_write_to_stream/3, 3, [
     pass_to(write_to_stream/3, 3)
   ]).





%! rdf_read_from_stream(+Source, :Goal_2) is det.
%! rdf_read_from_stream(+Source, :Goal_2, +Opts) is det.
% Goal_2 is applied to a metadata dictionary and a stream (in that order).
%
% @throws existence_error if an HTTP request returns an error code.

rdf_read_from_stream(Source, Goal_2) :-
  rdf_read_from_stream(Source, Goal_2, []).


rdf_read_from_stream(Source, Goal_2, Opts1) :-
  % Accept headers for RDF are specified in `library(semweb/rdf_http_plugin))'.
  rdf_http_plugin:rdf_extra_headers(DefaultRdfOpts, Opts1),
  merge_options(DefaultRdfOpts, Opts1, Opts2),
  read_from_stream(Source, rdf_read_from_stream0(Goal_2, Opts2), Opts2).

rdf_read_from_stream0(Goal_2, Opts1, M1, Source) :-
  % Guess the RDF serialization format in case option `rdf_format/1'
  % is not given.
  (   option(rdf_format(Format1), Opts1),
      ground(Format1)
  ->  true
  ;   rdf_guess_format_options0(M1, Opts1, Opts2),
      rdf_guess_format(Source, Format1, Opts2)
  ),
  (Format1 == jsonld -> set_stream(Source, encoding(utf8)) ; true),
  % `Format' is now instantiated.
  rdf_format_iri(Format1, Format2),
  jsonld_metadata_abbreviate_iri(Format2, Format3),
  M2 = M1.put(_{'llo:rdf_format': Format3}),
  call(Goal_2, M2, Source).

rdf_guess_format_options0(M, Opts1, Opts2) :-
  uri_file_extensions(M.'llo:base_iri'.'@value', Exts1),
  reverse(Exts1, Exts2),
  member(Ext, Exts2),
  rdf_file_extension(Ext, Format), !,
  merge_options(Opts1, [default_rdf_format(Format)], Opts2).
rdf_guess_format_options0(_, Opts, Opts).



%! rdf_write_to_stream(+Out, :Goal_2) is det.
%! rdf_write_to_stream(+Out, :Goal_2, +Opts) is det.

rdf_write_to_stream(Out, Goal_2) :-
  rdf_write_to_stream(Out, Goal_2, []).

rdf_write_to_stream(Out, Goal_2, Opts) :-
  write_to_stream(Out, Goal_2, Opts).
