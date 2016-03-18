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
:- use_module(library(msg_ext)).
:- use_module(library(option_ext)).
:- use_module(library(os/compress_ext)).
:- use_module(library(os/file_ext)).
:- use_module(library(os/gnu_sort)).
:- use_module(library(os/gnu_wc)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(rdf/rdf_file)). % Type definition.
:- use_module(library(rdf/rdf_stream)).
:- use_module(library(semweb/rdfa)).
:- use_module(library(semweb/rdf_ntriples)).
:- use_module(library(semweb/turtle)).
:- use_module(library(stream_ext)).

:- predicate_options(rdf_clean/3, 3, [
     pass_to(absolute_file_name/3, 3),
     pass_to(rdf_read_from_stream/3, 3),
     pass_to(rdf_clean_read/4, 2)
   ]).
:- predicate_options(rdf_clean_stream/4, 2, [
     compress(+oneof([deflate,gzip,none])),
     metadata(-dict),
     pass_to(sort_file/2, 2)
   ]).





%! rdf_clean(+From, +To) is det.
%! rdf_clean(+From, +To, +Opts) is det.
% The following options are supported:
%    * compress(+oneof([deflate,gzip,none]))
%      What type of compression is used on the output file.
%      Default is `none`.
%    * metadata(-dict)
%      Returns the metadata for cleaning the From source.
%    * rdf_format(+oneof([ntriples,nquads,rdfa,trig,trix,turtle,xml]))
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
  merge_options([quads(NoQuads),triples(NoTriples),tuples(NoTuples)], Opts1, Opts2),
  option(metadata(M4), Opts1, _),
  option(compress(Compress), Opts1, none),
  
  absolute_file_name(cleaning, Tmp0, [access(write)|Opts1]),
  thread_file(Tmp0, Tmp),
  debug(rdf(clean), "Temporarily storing clean RDF in ~a.", [Tmp]),
  debug_verbose(
    rdf(clean),
    setup_call_cleanup(
      (
        open(Tmp, write, Sink),
        gen_ntuples:gen_ntuples_begin(State, Opts2)
      ),
      rdf_load:rdf_call_on_tuples_stream(gen_ntuples:gen_ntuple(Sink, State), Opts1, M1, Source),
      (
        flush_output(Sink),
        gen_ntuples:gen_ntuples_end(State, Opts2),
        close(Sink)
      )
    ),
    "Cleaning tuples on a one-by-one basis."
  ),
  debug(rdf(clean), "Processed ~D tuples (~D triples and ~D quads).", [NoTuples,NoTriples,NoQuads]),
  M2 = M1.put(_{
    'llo:processed_quads': _{'@type': 'xsd:nonNegativeInteger', '@value': NoQuads},
    'llo:processed_triples': _{'@type': 'xsd:nonNegativeInteger', '@value': NoTriples},
    'llo:processed_tuples': _{'@type': 'xsd:nonNegativeInteger', '@value': NoTuples}
  }),

  % Store input stream properties.
  % @tbd Why does the stream not have any properties?
  stream_metadata(Source, MStream),
  M3 = M2.put(MStream),

  debug_verbose(rdf(clean), sort_file(Tmp, Opts1), "Sorting cleaned tuples file."),

  % Count the number of unique tuples.
  file_lines(Tmp, NoLines),
  NoDuplicates is NoTuples - NoLines,
  debug(rdf(clean), "Wrote ~D unique tuples (skipping ~D duplicates).", [NoTuples,NoDuplicates]),
  M4 = M3.put(_{
    'llo:unique_tuples': _{'@type': 'xsd:nonNegativeInteger', '@value': NoTuples},
    'llo:duplicate_tuples': _{'@type': 'xsd:nonNegativeInteger', '@value': NoDuplicates}
  }),
  
  % Compress the file, according to user option.
  debug_verbose(rdf(clean), compress_file(Tmp, Compress, To), "Compressing sorted tuple file.").
