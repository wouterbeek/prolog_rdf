:- module(
  rdf_convert,
  [
    rdf_convert/3 % +From
		  % ?To:atom
                  % +Options:list(compound)
  ]
).

/** <module> LOD Tools: Convert

Covnerter between RDF serialization formats.

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(apply)).
:- use_module(library(ctriples/ctriples_write_generics)).
:- use_module(library(ctriples/ctriples_write_graph)).
:- use_module(library(ctriples/ctriples_write_triples)).
:- use_module(library(option)).
:- use_module(library(os/file_ext)).
:- use_module(library(os/gnu_sort)).
:- use_module(library(os/gnu_wc)).
:- use_module(library(rdf/rdf_file)).
:- use_module(library(rdf/rdf_stream)).
:- use_module(library(semweb/rdfa)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_ntriples)).
:- use_module(library(semweb/turtle)).
:- use_module(library(typecheck)).

:- thread_local(has_quadruples/1).

:- predicate_options(rdf_convert/3, 3, [
     compress(+oneof([deflate,gzip,none])),
     pass_to(rdf_stream_read/3, 3)
   ]).





%! rdf_convert(+From, ?To:atom, +Options:list(compund)) is det.
% The following options are supported:
%    * compress(+oneof([deflate,gzip,none]))
%      What type of compression is used on the output file.
%      Default is `none`.
%    * format(+rdf_format)
%      The RDF serialization format of the input.
%      When absent this is guessed heuristically.
%
% @tbd What can we not specify `format(?rdf_format)`?

rdf_convert(From, To, Opts0):-
  % Process output RDF serialization option.
  (   % The output RDF serialization format is given: take it into account.
      option(format(Format), Opts0),
      ground(Format)
  ->  Opts = Opts0
  ;   % Allow the output RDF serialization format to be returned
      % to the calling context through an option.
      merge_options([format(_)], Opts0, Opts)
  ),
  
  % Process data compression option.
  option(compress(Compress), Opts, none),

  % Convert to the RDF input stream into C-Triples
  % on a triple-by-triple basis.
  thread_file(tmp, Tmp),
  setup_call_cleanup(
    open(Tmp, write, Write),
    rdf_stream_read(From, rdf_convert0(Write), Opts),
    close(Write)
  ),

  % Set the file name.
  (   ground(To)
  ->  true
  ;   % Establish the file name extension.
      (   retract(has_quadruples(true))
      ->  Ext = nq
      ;   Ext = nt
      ),
      (   Compress == gzip
      ->  Exts = [Ext,gz]
      ;   Exts = [Ext]
      ),
      thread_file(out, ThreadBase),
      atomic_list_concat([ThreadBase|Exts], '.', To0)
  ),

  % Sort unique, count, compress.
  sort_file(Tmp, Opts),
  file_lines(Tmp, N),
  debug(rdf(convert), 'Unique triples:~t~D~n', [N]),
  compress_file(Tmp, Compress, To0),
  (   is_absolute_file_name(From)
  ->  file_base_name(From, Base),
      file_with_new_extensions(Base, Exts, To)
  ;   is_uri(From)
  ->  uri_components(From, uri_components(_,Auth,Path,_,_)),
      atomic_list_concat(Subpaths, /, Path),
      (last(Subpaths, Base), Base \== '' -> true ; Base = Auth),
      file_with_new_extensions(Base, Exts, To)
  ;   To = To0
  ),
  rename_file(To0, To).

rdf_convert0(Write, Format, Read):-
  ctriples_write_begin(State, BNPrefix, []),
  Opts = [anon_prefix(BNPrefix),format(Format)],
  (   Format == rdfa
  ->  read_rdfa(Read, Ts, []),
      clean_streamed_triples(Write, State, BNPrefix, Ts, _)
  ;   memberchk(Format, [nquads,ntriples])
  ->  rdf_process_ntriples(
        Read,
        clean_streamed_triples(Write, State, BNPrefix),
        Opts
      )
  ;   memberchk(Format, [trig,turtle])
  ->  rdf_process_turtle(
        Read,
        clean_streamed_triples(Write, State, BNPrefix),
        Opts
      )
  ;   Format == xml
  ->  process_rdf(Read, clean_streamed_triples(Write, State, BNPrefix), [])
  ),
  flush_output(Write),
  ctriples_write_end(State, []).


%! clean_streamed_triples(
%!   +Write:stream,
%!   +State:compound,
%!   +BNPrefix:atom,
%!   +Triples:list(compound),
%!   +LinePosition:compound
%! ) is det.

clean_streamed_triples(Write, State, BNPrefix, Ts0, _):-
  maplist(fix_triple, Ts0, Ts),
  maplist(ctriples_write_triple(Write, State, BNPrefix), Ts).


%! fix_triple(
%!   +Graph:atom,
%!   +WonkyStatement:compound,
%!   -Statement:compound
%! ) is det.
%

fix_triple(rdf(S,P,O,G), T):- !,
  (   is_named_graph(G)
  ->  set_has_quadruples,
      T = rdf(S,P,O,G)
  ;   is_named_graph(G)
  ->  set_has_quadruples,
      T = rdf(S,P,O,G)
  ;   T = rdf(S,P,O)
  ).
fix_triple(T, T).


%! is_named_graph(+Graph:atom) is semidet.
% Succeeds for all and only named graphs.

is_named_graph(G):-
  ground(G),
  G \== user.


%! set_has_quadruples is det.
% Store the fact that a quadruple occurred in the parser stream
% as a thread-local global Prolog fact.

set_has_quadruples:-
  has_quadruples(true), !.
set_has_quadruples:-
  assert(has_quadruples(true)).



%! sort_file(+File:atom, +Options:list(compound)) is det.

sort_file(File, Opts):-
  % Sort directory.
  (   option(sort_dir(Dir), Opts)
  ->  access_file(Dir, write)
  ;   absolute_file_name(., Dir, [access(write),file_type(directory)])
  ),

  % Buffer size for sorting.
  buffer_size_file(File, BufferSize),
  (   BufferSize > 6 * (1024 ** 3) % >6GB
  ->  Threads = 3
  ;   BufferSize > 3 * (1024 ** 3) % >3GB
  ->  Threads = 2
  ;   Threads = 1 % =<3GB
  ),
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



%! buffer_size_file(+File:atom, -BufferSize:nonneg) is det.

buffer_size_file(File, BufferSize):-
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

compress_file(From, none, To):- !,
  rename_file(From, To).
compress_file(From, Compress, To):-
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

file_with_new_extensions(File1, Exts, File2):-
  atomic_list_concat([Name|_], ., File1),
  atomic_list_concat([Name|Exts], '.', File2).
