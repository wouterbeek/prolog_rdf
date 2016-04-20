:- module(
  rdf_clean,
  [
    rdf_clean/2, % +From, +To
    rdf_clean/3  % +From, +To, +Opts
  ]
).

/** <module> RDF cleaning

@author Wouter Beek
@version 2015/08-2015/11, 2016/01, 2016/03
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
:- use_module(library(os/gnu_wc)).
:- use_module(library(print_ext)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_file)). % Type definition.
:- use_module(library(rdf/rdf_stream)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfa)).
:- use_module(library(semweb/rdf_ntriples)).
:- use_module(library(semweb/turtle)).
:- use_module(library(stream_ext)).





%! rdf_clean(+From, +To) is det.
%! rdf_clean(+From, +To, +Opts) is det.
% The following options are supported:
%    * compress(+oneof([deflate,gzip,none]))
%      What type of compression is used on the output file.
%      Default is `none`.
%    * rdf_format(+oneof([jsonld,ntriples,nquads,rdfa,trig,trix,turtle,xml]))
%      The RDF serialization format of the input.
%      When absent this is guessed heuristically.
%
% @throws existence_error If an HTTP request returns an error code.

rdf_clean(From, To) :-
  rdf_clean(From, To, []).

rdf_clean(From, To, Opts) :-
  rdf_read_from_stream(From, rdf_clean_stream(To, Opts), Opts).


%! rdf_clean_stream(+To, +Opts, +Metadata, +Source) is det.

rdf_clean_stream(To, Opts1, M1, Source) :-
  Opts0 = [quads(NumQuads),triples(NumTriples),tuples(NumTuples)],
  merge_options(Opts0, Opts1, Opts2),
  option(compress(Compress), Opts1, none),
  
  absolute_file_name(cleaning, Tmp0, [access(write)|Opts1]),
  thread_file(Tmp0, Tmp),
  setup_call_cleanup(
    (
      open(Tmp, write, Sink),
      gen_ntuples:gen_ntuples_begin(State, Opts2)
    ),
    rdf_load:rdf_call_on_tuples_stream0(gen_ntuple0(Sink, State), Opts1, M1, Source),
    (
      gen_ntuples:gen_ntuples_end(State, Opts2),
      close(Sink)
    )
  ),
  deb_cleaned_tuples(NumTuples, NumTriples, NumQuads),
  M2 = M1.put(_{
    'llo:processed_quads': NumQuads,
    'llo:processed_triples': NumTriples,
    'llo:processed_tuples': NumTuples
  }),

  sort_file(Tmp, Opts1),

  % Count the number of unique tuples.
  file_lines(Tmp, NumLines),
  NumDuplicates is NumTuples - NumLines,
  deb_wrote_tuples(NumTuples, NumDuplicates),
  M3 = M2.put(_{
    'llo:unique_tuples': NumTuples,
    'llo:duplicate_tuples': NumDuplicates
  }),
  
  % Compress the file, according to user option.
  compress_file(Tmp, Compress, To),
  delete_file(Tmp).

gen_ntuple0(Sink, State, _, S, P, O, G) :-
  gen_ntuples:gen_ntuple(Sink, State, S, P, O, G).





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
