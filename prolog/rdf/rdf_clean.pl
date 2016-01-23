:- module(
  rdf_clean,
  [
    rdf_clean/2, % +From, ?To:atom
    rdf_clean/3  % +From, ?To:atom, +Opts
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
:- use_module(library(os/file_ext)).
:- use_module(library(os/gnu_sort)).
:- use_module(library(os/gnu_wc)).
:- use_module(library(rdf/rdf_clean_metadata)).
:- use_module(library(rdf/rdf_clean_msg)).
:- use_module(library(rdf/rdf_stream)).
:- use_module(library(semweb/rdfa)).
:- use_module(library(semweb/rdf_ntriples)).
:- use_module(library(semweb/turtle)).
:- use_module(library(simple/write_SimpleRDF)).
:- use_module(library(stream_ext)).

:- predicate_options(determine_sort_buffer_size/3, 3, [
     max_sort_buffer_size(+float)
   ]).
:- predicate_options(determine_sort_buffer_threads/3, 3, [
     max_sort_threads(+nonneg)
   ]).
:- predicate_options(rdf_clean/3, 3, [
     format(+oneof([ntriples,nquads,rdfa,trig,trix,turtle,xml])),
     pass_to(rdf_read_from_stream/3, 3),
     pass_to(rdf_clean_read/4, 2)
   ]).
:- predicate_options(rdf_clean_stream/4, 2, [
     compress(+oneof([deflate,gzip,none])),
     metadata(-dict),
     show_metadata(+boolean),
     pass_to(sort_file/2, 2)
   ]).
:- predicate_options(sort_file/2, 2, [
     sort_dir(+atom),
     pass_to(determine_sort_buffer_size/3, 3),
     pass_to(determine_sort_buffer_threads/3, 3)
   ]).





%! rdf_clean(+From, ?To:atom) is det.
% Wrapper for rdf_clean/3 with default options.

rdf_clean(From, To) :-
  rdf_clean(From, To, []).


%! rdf_clean(+From, ?To:atom, +Opts) is det.
% The following options are supported:
%    * compress(+oneof([deflate,gzip,none]))
%      What type of compression is used on the output file.
%      Default is `none`.
%    * format(+oneof([ntriples,nquads,rdfa,trig,trix,turtle,xml]))
%      The RDF serialization format of the input.
%      When absent this is guessed heuristically.
%    * metadata(-dict)
%    * show_metadata(+boolean)

rdf_clean(From, To, Opts) :-
  % Process output RDF serialization option.
  (   % The output RDF serialization format is given: take it into account
      % by relaying it to a different options list.
      select_option(format(Format), Opts, CleanOpts),
      ground(Format)
  ->  StreamOpts = [format(Format)]
  ;   % Allow the output RDF serialization format to be returned
      % to the calling context through an option.
      merge_options([format(_)], Opts, StreamOpts),
      CleanOpts = StreamOpts
  ),

  rdf_read_from_stream(From, rdf_clean_stream(To, CleanOpts), StreamOpts).


%! rdf_clean_stream(?To:atom, +Opts, +Metadata, +Read) is det.

rdf_clean_stream(Local0, Opts1, M1, Read) :-
  option(metadata(M5), Opts1, _),

  % Process data compression option.
  option(compress(Compress), Opts1, none),

  % Convert to the RDF input stream into C-Triples
  % on a triple-by-triple basis.
  absolute_file_name(cleaning, Tmp0, [access(write)]),
  thread_file(Tmp0, Tmp),
  debug(
    rdf(clean),
    "Going to temporarily store clean triples in file ~a.",
    [Tmp]
  ),

  % Read and write all triples.
  merge_options([quadruples(NQ),statements(NS),triples(NT)], Opts1, Opts2),
  debug_verbose(
    rdf(clean),
    setup_call_cleanup(
      open(Tmp, write, Write),
      rdf_write_clean_stream(Read, M1, Write, Opts2),
      close(Write)
    ),
    "Cleaning triples on a one-by-one basis."
  ),
  msg_notification(
    "Wrote ~D statements (~D triples and ~D quadruples).~n",
    [NS,NT,NQ]
  ),
  M2 = M1.put(rdf/quadruples, NQ),
  M3 = M2.put(rdf/statements, NS),
  M4 = M3.put(rdf/triples, NT),
  
  
  % Store input stream properties.
  stream_metadata(Read, MStream),
  M5 = M4.put(stream, MStream),

  % Sort unique.
  debug_verbose(
    rdf(clean),
    sort_file(Tmp, Opts),
    "Sorting cleaned triples file."
  ),

  % Count the number of triples.
  file_lines(Tmp, NS2),
  debug(rdf(clean), "Unique statements: ~D", [NS2]),

  % Determine output file name.
  (ground(Local0) -> true ; Local0 = out),

  % Modify the output file name for the current archive entry.
  absolute_file_name(Local0, Dir0),
  archive_entry_name(Dir0, M5.archive_entry, Path0),
  atomic_list_concat([Local|_], ., Path0),
  
  % Strip outdated file extensions from the output file name
  % and set the extensions of the output file name.
  (NQ > 0 -> Ext = nq ; Ext = nt),
  (Compress == gzip -> Exts = [Ext,gz] ; Exts = [Ext]),
  atomic_list_concat([Local|Exts], ., Path),

  % Compress the file, according to user option.
  debug_verbose(
    rdf(clean),
    compress_file(Tmp, Compress, Path),
    "Compressing sorted triple file."
  ),

  % Print metadata.
  if_option(show_metadata(true), Opts, rdf_clean_metadata(M5)).


%! rdf_write_clean_stream(+Read, +Metadata, +Write, +Opts) is det.

rdf_write_clean_stream(Read, M, Write, Opts1) :-
  % Library Semweb uses option base_uri/1.  We use option base_iri/1.
  merge_options([base_iri(M.base_iri)], Opts1, Opts2),
  write_simple_begin(BNodePrefix, C1, C2, Opts2),
  merge_options(
    [anon_prefix(BNodePrefix),base_uri(M.base_iri),format(M.rdf.format)],
    Opts2,
    Opts3
  ),
  
  (   M.rdf.format == rdfa
  ->  read_rdfa(Read, Ts, [max_errors(-1),syntax(style)]),
      clean_streamed_triples(Write, BNodePrefix, C1, C2, Ts, _)
  ;   memberchk(M.rdf.format, [nquads,ntriples])
  ->  rdf_process_ntriples(
        Read,
        clean_streamed_triples(Write, BNodePrefix, C1, C2),
        Opts3
      )
  ;   memberchk(M.rdf.format, [trig,turtle])
  ->  rdf_process_turtle(
        Read,
        clean_streamed_triples(Write, BNodePrefix, C1, C2),
        Opts3
      )
  ;   M.rdf.format == xml
  ->  process_rdf(
        Read,
        clean_streamed_triples(Write, BNodePrefix, C1, C2),
        Opts3
      )
  ),
  flush_output(Write),
  write_simple_end(C1, C2, Opts3).



%! archive_entry_name(
%!   +PathPrefix:atom,
%!   +Compression:list(dict),
%!   -EntryPath:atom
%! ) is det.
% Succeeds if EntryPath describes the file path of
% the nested archive entry described in Compression
% that uses the given PathPrefix.

% The raw archive entry's path is the prefix path.
archive_entry_name(Path, [H], Path) :-
  is_unarchived(H), !.
% A non-raw archive entry: add its name to the prefix path.
archive_entry_name(Prefix, [H|T], EntryPath) :-
  make_directory_path(Prefix),
  directory_file_path(Prefix, H.name, Path),
  archive_entry_name(Path, T, EntryPath).



%! clean_streamed_triples(
%!   +Write:stream,
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



%! is_unarchived(+CompressionNode:dict) is semidet.
% Succeed if CompressionNode descibes a leaf node in a compression tree.
% A leaf node in a compression tree describes an unarchived or raw file.

is_unarchived(D) :-
  D.name == data,
  D.format == raw, !.



%! sort_file(+File:atom, +Options:list(compound)) is det.
% The following options are supported:
%   * max_sort_buffer_size(+float)
%     The maximum size of the sort buffer in Gigabytes.
%     Default is 1.0 GB.
%   * sort_dir(+atom)
%     The directory that is used for disk-based sorting.

sort_file(File, Opts) :-
  % Determine the directory that is used for disk-based sorting.
  (   option(sort_dir(Dir), Opts)
  ->  access_file(Dir, write)
  ;   absolute_file_name(., Dir, [access(write),file_type(directory)])
  ),
  debug(rdf(clean), "Using directory ~a for disk-based softing.", [Dir]),

  % Determine the buffer size that is used for sorting.
  determine_sort_buffer_size(File, BufferSize, Opts),

  % Determine the number of threads that is used for sorting.
  determine_sort_threads(BufferSize, Threads, Opts),

  % Perform the actual sort.
  gnu_sort(
    File,
    [
      buffer_size(BufferSize),
      duplicates(false),
      output(File),
      parallel(Threads),
      temporary_directory(Dir),
      utf8(true)
    ]
  ).



%! determine_sort_buffer_size(
%!   +File:atom,
%!   -BufferSize:nonneg,
%!   +Options:list(compound)
%! ) is det.
% The following options are supported:
%   * max_sort_buffer_size(+float)
%     The maximum size of the buffer used for sorting.
%     Default is `1.0'.

determine_sort_buffer_size(File, BufferSize, Opts) :-
  calc_sort_buffer_size(File, Calc),
  option(max_sort_buffer_size(Max), Opts, 1.0),
  BufferSize is min(round(Max * (1024 ** 3)), Calc),
  BufferSize0 is BufferSize / (1024 ** 3),
  debug(rdf(clean), "Using buffer size ~2f GB for sorting.", [BufferSize0]).



%! determine_sort_buffer_threads(
%!   +BufferSize:nonneg,
%!   -Threads:nonneg,
%!   +Options:list(compound)
%! ) is det.
% The following options are supported:
%   * max_sort_threads(+nonneg)
%     The maximum number of threads that is allowed to be used.
%     Default is the value of `current_prolog_flag(cpu_count, X)'.

determine_sort_threads(BufferSize, Threads, Opts) :-
  calc_sort_threads(BufferSize, Calc),
  current_prolog_flag(cpu_count, Default),
  option(max_sort_threads(Max), Opts, Default),
  Threads is min(Max, Calc),
  debug(rdf(clean), "Using ~D threads for sorting.", [Threads]).

  

%! calc_sort_threads(+BufferSize:nonneg, -Threads:nonneg) is det.
% Heuristically determine the number of threads to use for sorting
% a file with the given BufferSize.

calc_sort_threads(BufferSize, 3) :- % 6GB<x
  BufferSize > 6 * (1024 ** 3), !.
calc_sort_threads(BufferSize, 2) :- % 3GB<x=<6GB
  BufferSize > 3 * (1024 ** 3), !.
calc_sort_threads(_, 1). % x=<3GB



%! calc_sort_buffer_size(+File:atom, -BufferSize:nonneg) is det.
% Determines the BufferSize that will be used for sorting File
% according to a simple heuristic.

calc_sort_buffer_size(File, BufferSize) :-
  size_file(File, FileSize),
  (   FileSize =:= 0
  ->  BufferSize = 1024
  ;   BufferSize is round(FileSize * log(FileSize))
  ).



%! compress_file(
%!   +From:atom,
%!   +Compress:oneof([deflate,gzip,none]),
%!   +To:atom
%! ) is det.

compress_file(From, none, To) :- !,
  rename_file(From, To).
compress_file(From, Compress, To) :-
  setup_call_cleanup(
    gzopen(To, write, Write, [format(Compress)]),
    setup_call_cleanup(
      open(From, read, Read),
      copy_stream_data(Read, Write),
      close(Read)
    ),
    close(Write)
  ).



%! file_with_new_extensions(
%!   +File1:atom,
%!   +Extensions:list(atom),
%!   -File2:atom
%! ) is det.

file_with_new_extensions(File1, Exts, File2) :-
  atomic_list_concat([Name|_], ., File1),
  atomic_list_concat([Name|Exts], '.', File2).
