:- module(
  rdf_clean,
  [
    rdf_clean/2, % +Source, +Sink
    rdf_clean/3  % +Source, +Sink, +Opts
  ]
).

/** <module> RDF cleaning

@author Wouter Beek
@version 2015/08-2015/11, 2016/01, 2016/03-2016/05
*/

:- use_module(library(apply)).
:- use_module(library(debug_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(filesex)).
:- use_module(library(gen/gen_ntuples)).
:- use_module(library(json_ext)).
:- use_module(library(jsonld/jsonld_metadata)).
:- use_module(library(option_ext)).
:- use_module(library(os/compress_ext)).
:- use_module(library(os/file_ext)).
:- use_module(library(os/gnu_sort)).
:- use_module(library(os/io)).
:- use_module(library(print_ext)).
:- use_module(library(rdf/rdf_file)). % Type definition.
:- use_module(library(rdf/rdfio)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfa)).
:- use_module(library(semweb/rdf_ntriples)).
:- use_module(library(semweb/turtle)).
:- use_module(library(stream_ext)).
:- use_module(library(yall)).





%! rdf_clean(+Source, +Sink) is det.
%! rdf_clean(+Source, +Sink, +Opts) is det.
%
% The following options are supported:
%
%   * metadata(-dict)
%
%   * Other options are passed to:
%       * rdf_call_on_stream/3
%       * rdf_clean/5
%
% @throws existence_error If an HTTP request returns an error code.

rdf_clean(Source, Sink) :-
  rdf_clean(Source, Sink, []).


rdf_clean(Source, Sink, Opts) :-
  rdf_call_on_stream(Source, rdf_clean0(Sink, Opts), Opts).
  %ignore(option(metadata(M2), Opts)).


rdf_clean0(Sink, Opts1, In) :-
  Opts0 = [quads(NumQuads),triples(NumTriples),tuples(NumTuples)],
  merge_options(Opts0, Opts1, Opts2),
  option(compress(Compress), Opts1, none),
  
  absolute_file_name(cleaning, TmpSink0, [access(write)|Opts1]),
  thread_file(TmpSink0, TmpSink),
  call_to_ntuples(TmpSink, gen_ntuples(M, _), Opts1),
  deb_cleaned_tuples(NumTuples, NumTriples, NumQuads),
  M3 = M2.put(_{
    processed_quads: NumQuads,
    processed_triples: NumTriples,
    processed_tuples: NumTuples
  }),

  sort_file(TmpSink, Opts1),

  % Count the number of unique tuples.
  count_numlines(TmpSink, NumLines),
  NumDuplicates is NumTuples - NumLines,
  deb_wrote_tuples(NumTuples, NumDuplicates),
  M4 = M3.put(_{
    unique_tuples: NumTuples,
    duplicate_tuples: NumDuplicates
  }),
  
  % Compress the file, according to user option.
  compress_file(TmpSink, Compress, Sink),
  delete_file(TmpSink).
  




% HELPERS %

deb_cleaned_tuples(NumTuples, NumTriples, NumQuads) :-
  debug(
    rdf(clean),
    "Cleaned ~D tuples (~D triples and ~D quads).",
    [NumTuples,NumTriples,NumQuads]
  ).



deb_wrote_tuples(NumTuples, NumDuplicates) :-
  debug(
    rdf(clean),
    "Wrote ~D unique tuples (skipping ~D duplicates).",
    [NumTuples,NumDuplicates]
  ).
