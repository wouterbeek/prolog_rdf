:- module(
  rdf_stream,
  [
    rdf_call_on_stream/2, % +Source, :Goal_3
    rdf_call_on_stream/3  % +Source, :Goal_3, +Opts
  ]
).

/** <module> RDF stream

@author Wouter Beek
@version 2015/08-2016/02, 2016/04
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(error)).
:- use_module(library(http/http_ext)).
:- use_module(library(iri/iri_ext)).
:- use_module(library(jsonld/jsonld_metadata)).
:- use_module(library(lists)).
:- use_module(library(option_ext)).
:- use_module(library(os/open_any2)).
:- use_module(library(rdf/rdf_file)). % Type definition.
:- use_module(library(rdf/rdf_guess)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(typecheck)).
:- use_module(library(zlib)).

:- meta_predicate
   rdf_call_on_stream(+, 3),
   rdf_call_on_stream(+, 3, +),
   rdf_call_on_stream0(3, +, +, +, -).





%! rdf_call_on_stream(+Source, :Goal_3) is det.
%! rdf_call_on_stream(+Source, :Goal_3, +Opts) is det.
% Goal_2 is applied to a metadata dictionary and a stream (in that order).
%
% Options are passed to read_from_stream/3 and rdf_guess_fomat/3
%
% @throws existence_error if an HTTP request returns an error code.

rdf_call_on_stream(Source, Goal_3) :-
  rdf_call_on_stream(Source, Goal_3, []).


rdf_call_on_stream(Source, Goal_3, Opts1) :-
  % Accept headers for RDF are specified in `library(semweb/rdf_http_plugin))'.
  rdf_http_plugin:rdf_extra_headers(DefRdfOpts, Opts1),
  merge_options(DefRdfOpts, Opts1, Opts2),
  call_on_stream(Source, rdf_call_on_stream0(Goal_3, Opts2), Opts2).

rdf_call_on_stream0(Goal_3, Opts, In, M1, M3) :-
  % Guess the RDF serialization format in case option rdf_format/1
  % is not given.
  (   option(rdf_format(Format1), Opts),
      ground(Format1)
  ->  true
  ;   rdf_guess_format_options0(M1, Opts, GuessOpts),
      % Make sure the metadata option of the RDF source does not get overwritten
      % when opening the stream for guessing the RDF serialization format.
      rdf_guess_format(In, Format1, GuessOpts)
  ),
  (Format1 == jsonld -> set_stream(In, encoding(utf8)) ; true),
  % `Format' is now instantiated.
  (rdf_format_iri(Format1, Format2) -> true ; domain_error(rdf_format, Format1)),
  jsonld_metadata_abbreviate_iri(Format2, Format3),
  M2 = M1.put(_{'llo:rdf_format': Format3}),
  call(Goal_3, In, M2, M3).

rdf_guess_format_options0(M, Opts1, Opts2) :-
  iri_file_extensions(M.'llo:base_iri', Exts1),
  reverse(Exts1, Exts2),
  member(Ext, Exts2),
  rdf_file_extension(Ext, Format), !,
  merge_options(Opts1, [default_rdf_format(Format)], Opts2).
rdf_guess_format_options0(_, Opts, Opts).
