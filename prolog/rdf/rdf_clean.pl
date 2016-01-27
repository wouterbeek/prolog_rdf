:- module(
  rdf_clean,
  [
    rdf_clean/2, % +From, -To
    rdf_clean/3  % +From, -To, +Opts
  ]
).

/** <module> RDF cleaning

@author Wouter Beek
@version 2015/08-2015/11, 2016/01
*/

:- use_module(library(apply)).
:- use_module(library(debug_ext)).
:- use_module(library(filesex)).
:- use_module(library(msg_ext)).
:- use_module(library(option_ext)).
:- use_module(library(os/compress_ext)).
:- use_module(library(os/file_ext)).
:- use_module(library(os/gnu_sort)).
:- use_module(library(os/gnu_wc)).
:- use_module(library(rdf/rdf_clean_msg)).
:- use_module(library(rdf/rdf_metadata_print)).
:- use_module(library(rdf/rdf_stream)).
:- use_module(library(semweb/rdfa)).
:- use_module(library(semweb/rdf_ntriples)).
:- use_module(library(semweb/turtle)).
:- use_module(library(simple/write_SimpleRDF)).
:- use_module(library(stream_ext)).

:- predicate_options(rdf_clean/3, 3, [
     format(+oneof([ntriples,nquads,rdfa,trig,trix,turtle,xml])),
     pass_to(rdf_read_from_stream/3, 3),
     pass_to(rdf_clean_read/4, 2)
   ]).
:- predicate_options(rdf_clean_stream/4, 2, [
     compress(+oneof([deflate,gzip,none])),
     metadata(-dict),
     pass_to(sort_file/2, 2)
   ]).





%! rdf_clean(+From, -To) is det.
%! rdf_clean(+From, -To, +Opts) is det.
% The following options are supported:
%    * compress(+oneof([deflate,gzip,none]))
%      What type of compression is used on the output file.
%      Default is `none`.
%    * format(+oneof([ntriples,nquads,rdfa,trig,trix,turtle,xml]))
%      The RDF serialization format of the input.
%      When absent this is guessed heuristically.
%    * metadata(-dict)
%      Returns the metadata for cleaning the From source.
%
% @throws existence_error If an HTTP request returns an error code.

rdf_clean(From, To) :-
  rdf_clean(From, To, []).

rdf_clean(From, To, Opts) :-
  % Be careful with the `format/1' option.  It either denotes the RDF
  % serialization format or the archive format.
  (   % The output RDF serialization format is given: take it into account
      % during the streaming process (not needed in the cleaning process).
      select_option(format(Format), Opts, CleanOpts),
      ground(Format)
  ->  StreamOpts = [format(Format)]
  ;   % The output RDF serialization format is not given: allow it to be returned
      % to the calling context through an option.
      merge_options([format(_)], Opts, StreamOpts),
      CleanOpts = Opts
  ),

  rdf_read_from_stream(From, rdf_clean_stream(To, CleanOpts), StreamOpts).


%! rdf_clean_stream(-To, +Opts, +Metadata, +Read) is det.

rdf_clean_stream(To, Opts1, M1, Read) :-
  option(metadata(M5), Opts1, _),

  % Data compression option.  By default no compression is used.
  option(compress(Compress), Opts1, none),

  % Convert the RDF input stream into Simple-Triples.
  % This is done on a per triple basis.
  absolute_file_name(cleaning, Tmp0, [access(write)]),
  thread_file(Tmp0, Tmp),
  debug(rdf(clean), "Temporarily storing clean RDF in ~a.", [Tmp]),

  % Read and write all triples.
  merge_options([quadruples(NQ),statements(NS1),triples(NT)], Opts1, Opts2),
  debug_verbose(
    rdf(clean),
    setup_call_cleanup(
      open(Tmp, write, Write),
      rdf_write_clean_stream(Read, M1, Write, Opts2),
      close(Write)
    ),
    "Cleaning triples on a one-by-one basis."
  ),
  debug(
    rdf(clean),
    "Processed ~D statements (~D triples and ~D quadruples).",
    [NS1,NT,NQ]
  ),
  M2 = M1.put(rdf/quadruples, NQ),
  M3 = M2.put(rdf/statements, NS1),
  M4 = M3.put(rdf/triples, NT),

  % Store input stream properties.
  % @tbd Why does the stream not have any properties?
  stream_metadata(Read, MStream),
  M5 = M4.put(stream, MStream),

  % Sort unique.
  debug_verbose(
    rdf(clean),
    sort_file(Tmp, Opts1),
    "Sorting cleaned triples file."
  ),

  % Count the number of unique statements.
  file_lines(Tmp, NS2),
  NS3 is NS2 - NS1,
  debug(
    rdf(clean),
    "Wrote ~D unique statements (skipped ~D duplicates).",
    [NS2,NS3]
  ),

  % Determine output file name.
  Base = clean,
  (Compress == gzip -> Ext = 'nq.gz' ; Ext = nq),
  file_name_extension(Base, Ext, Name),
  absolute_file_name(Name, To, [access(write)]),

  % Compress the file, according to user option.
  debug_verbose(
    rdf(clean),
    compress_file(Tmp, Compress, To),
    "Compressing sorted triple file."
  ),

  % Show metadata.
  if_debug(rdf(clean), rdf_metadata_print(M5)).


%! rdf_write_clean_stream(+Read, +Metadata, +Write, +Opts) is det.

rdf_write_clean_stream(Read, M, Write, Opts1) :-
  % Library Semweb uses option base_uri/1.  We use option base_iri/1.
  merge_options([base_iri(M.base_iri)], Opts1, Opts2),
  merge_options([base_uri(M.base_iri),format(M.rdf.format)], Opts2, Opts3),
  setup_call_cleanup(
    write_simple_begin(BNodePrefix, C1, C2, Opts2),
    (
      merge_options([anon_prefix(BNodePrefix)], Opts3, Opts4),
      (   M.rdf.format == rdfa
      ->  read_rdfa(Read, Ts, [max_errors(-1),syntax(style)]),
          clean_streamed_triples(Write, BNodePrefix, C1, C2, Ts, _)
      ;   memberchk(M.rdf.format, [nquads,ntriples])
      ->  rdf_process_ntriples(
            Read,
            clean_streamed_triples(Write, BNodePrefix, C1, C2),
            Opts4
          )
      ;   memberchk(M.rdf.format, [trig,turtle])
      ->  rdf_process_turtle(
            Read,
            clean_streamed_triples(Write, BNodePrefix, C1, C2),
            Opts4
          )
      ;   M.rdf.format == xml
      ->  process_rdf(
            Read,
            clean_streamed_triples(Write, BNodePrefix, C1, C2),
            Opts4
          )
      )
    ),
    (
      flush_output(Write),
      write_simple_end(C1, C2, Opts4)
    )
  ).


%! clean_streamed_triples(
%!   +Write,
%!   +BNodePrefix:atom,
%!   +TripleCounter:compound,
%!   +QuadrupleCounter:compound,
%!   +Triples:list(compound),
%!   +LinePosition:compound
%! ) is det.

clean_streamed_triples(Write, BNodePrefix, C1, C2, Stmts, _) :-
  with_output_to(
    Write,
    maplist(write_simple_statement(BNodePrefix, C1, C2), Stmts)
  ).
