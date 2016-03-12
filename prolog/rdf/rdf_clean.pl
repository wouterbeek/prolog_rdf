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
:- use_module(library(json_ext)).
:- use_module(library(jsonld/jsonld_metadata)).
:- use_module(library(msg_ext)).
:- use_module(library(option_ext)).
:- use_module(library(os/compress_ext)).
:- use_module(library(os/file_ext)).
:- use_module(library(os/gnu_sort)).
:- use_module(library(os/gnu_wc)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_debug)).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(rdf/rdf_file)). % Type definition.
:- use_module(library(rdf/rdf_stream)).
:- use_module(library(semweb/rdfa)).
:- use_module(library(semweb/rdf_ntriples)).
:- use_module(library(semweb/turtle)).
:- use_module(library(simple/write_SimpleRDF)).
:- use_module(library(stream_ext)).

:- predicate_options(rdf_clean/3, 3, [
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


%! rdf_clean_stream(+To, +Opts, +Metadata, +Read) is det.

rdf_clean_stream(To, Opts1, D1, Read) :-
  option(metadata(D4), Opts1, _),

  % Data compression option.  By default no compression is used.
  option(compress(Compress), Opts1, none),

  % Convert the RDF input stream into Simple-Triples.
  % This is done on a per triple basis.
  absolute_file_name(cleaning, Tmp0, [access(write)]),
  thread_file(Tmp0, Tmp),
  debug(rdf(clean), "Temporarily storing clean RDF in ~a.", [Tmp]),

  % Read and write all triples.
  merge_options([quads(NoQuads),triples(NoTriples),tuples(NoTuples)], Opts1, Opts2),
  debug_verbose(
    rdf(clean),
    setup_call_cleanup(
      open(Tmp, write, Write),
      rdf_write_clean_stream(Read, D1, Write, Opts2),
      close(Write)
    ),
    "Cleaning tuples on a one-by-one basis."
  ),
  debug(rdf(clean), "Processed ~D tuples (~D triples and ~D quads).", [NoTuples,NoTriples,NoQuads]),
  D2 = D1.put(_{
    'llo:processed_quads': _{'@type': 'xsd:nonNegativeInteger', '@value': NoQuads},
    'llo:processed_triples': _{'@type': 'xsd:nonNegativeInteger', '@value': NoTriples},
    'llo:processed_tuples': _{'@type': 'xsd:nonNegativeInteger', '@value': NTuples}
  }),

  % Store input stream properties.
  % @tbd Why does the stream not have any properties?
  stream_metadata(Read, DStream),
  D3 = D2.put(DStream),

  % Sort unique.
  debug_verbose(rdf(clean), sort_file(Tmp, Opts1), "Sorting cleaned tuples file."),

  % Count the number of unique tuples.
  file_lines(Tmp, NoLines),
  NoDuplicates is NoTuples - NoLines,
  debug(rdf(clean), "Wrote ~D unique tuples (skipping ~D duplicates).", [NoTuples,NoDuplicates]),
  D4 = D3.put(_{
    'llo:unique_tuples': _{'@type': 'xsd:nonNegativeInteger', '@value': NoTuples},
    'llo:duplicate_tuples': _{'@type': 'xsd:nonNegativeInteger', '@value': NoDuplicates}
  }),
  
  % Compress the file, according to user option.
  debug_verbose(rdf(clean), compress_file(Tmp, Compress, To), "Compressing sorted tuple file.").


%! rdf_write_clean_stream(+Read, +Metadata, +Write, +Opts) is det.

rdf_write_clean_stream(Read, D, Write, Opts1) :-
  % Library Semweb uses option base_uri/1.  We use option base_iri/1.
  BaseIri = D.'llo:base_iri'.'@value',
  jsonld_metadata_expand_iri(D.'llo:rdf_format', Format1),
  rdf_format_iri(Format2, Format1),
  merge_options([base_iri(BaseIri)], Opts1, Opts2),
  merge_options([base_uri(BaseIri),format(Format2)], Opts2, Opts3),
  setup_call_cleanup(
    write_simple_begin(BPrefix, TripleCounter, QuadCounter, Opts2),
    (
      merge_options([anon_prefix(BPrefix)], Opts3, Opts4),
      (   Format2 == rdfa
      ->  read_rdfa(Read, Tuples, [max_errors(-1),syntax(style)]),
          clean_streamed_tuples(Write, BPrefix, TripleCounter, QuadCounter, Tuples, _)
      ;   memberchk(Format2, [nquads,ntriples])
      ->  rdf_process_ntriples(Read,
            clean_streamed_tuples(Write, BPrefix, TripleCounter, QuadCounter),
            Opts4
          )
      ;   memberchk(Format2, [trig,turtle])
      ->  rdf_process_turtle(Read,
            clean_streamed_tuples(Write, BPrefix, TripleCounter, QuadCounter),
            Opts4
          )
      ;   Format2 == xml
      ->  process_rdf(Read,
            clean_streamed_tuples(Write, BPrefix, TripleCounter, QuadCounter),
            Opts4
          )
      )
    ),
    (
      flush_output(Write),
      write_simple_end(TripleCounter, QuadCounter, Opts4)
    )
  ).


%! clean_streamed_tuples(
%!   +Write,
%!   +BPrefix,
%!   +TripleCounter:compound,
%!   +QuadCounter:compound,
%!   +Tuples,
%!   +LinePosition:compound
%! ) is det.

clean_streamed_tuples(Write, BPrefix, TripleCounter, QuadCounter, Tuples, _) :-
  with_output_to(Write,
    maplist(write_simple_statement(BPrefix, TripleCounter, QuadCounter), Tuples)
  ).
