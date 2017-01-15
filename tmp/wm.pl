:- module(
  wm,
  [
    wm_clean_inner/2, % +Uri, +ArchiveHash
    wm_clean_uri/1,   % +Uri
    wm_reset_hash/1,  % +Hash
    wm_reset_uri/1    % +Uri
  ]
).

/** <module> LOD Washing Machine

The following debug flags are defined:
  * wm(begin)
  * wm(done)
  * wm(end)
  * wm(idle)

@author Wouter Beek
@tbd Can we also count (byte_count, char_count, lines_count) what is
     _read_?
@version 2016/01-2017/01
*/

:- use_module(library(apply)).
:- use_module(library(debug_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(filesex)).
:- use_module(library(hash_ext)).
:- use_module(library(hdt/hdt_api)).
:- use_module(library(http/http_io)).
:- use_module(library(lists)).
:- use_module(library(os/archive_ext)).
:- use_module(library(os/compress_ext)).
:- use_module(library(os/file_ext)).
:- use_module(library(os/gnu_sort)).
:- use_module(library(os/io)).
:- use_module(library(os/thread_ext)).
:- use_module(library(pl_ext)).
:- use_module(library(print_ext)).
:- use_module(library(rdf/rdf__io)).
:- use_module(library(rdf/rdf_api)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_error)).
:- use_module(library(service/rocks_api), []).
:- use_module(library(zlib)).

:- meta_predicate
    call_meta_warn(+, 2),
    call_meta_warn(+, +, +, 2),
    call_meta_warn_streams(+, 2, +, +).

prolog_stack:stack_guard('C').
prolog_stack:stack_guard(none).





% API %

%! wm_clean_uri(+Uri) is det.

wm_clean_uri(Uri0) :-
  iri_normalized(Uri0, Uri),
  md5(Uri, Hash),
  wm_clean_inner(Uri, Hash).



%! wm_reset_hash(+Hash) is det.

wm_reset_hash(Hash) :-
  q_dir_hash(Dir, Hash),
  with_mutex(ll, delete_directory_and_contents_msg(Dir)).



%! wm_reset_uri(+Uri) is det.

wm_reset_uri(Uri0) :-
  iri_normalized(Uri0, Uri),
  md5(Uri, Hash),
  % Remove the directories for all entry hashes, if any.
  ignore(
    forall(
      call_on_stream(Uri, wm_reset_entry(Uri)),
      true
    )
  ),
  % Remove the directory for the archive hash
  wm_reset_hash(Hash).

wm_reset_entry(Uri, _, InPath, InPath) :-
  path_entry_name(InPath, EntryName),
  md5(Uri-EntryName, EntryHash),
  wm_reset_hash(EntryHash).





% CORE %

%! wm_clean_inner(+From, +ArchiveHash) is det.

wm_clean_inner(From, ArchiveHash) :-
  atomic_list_concat([a,ArchiveHash], :, ArchiveAlias),
  archive_label(From, ArchiveHash, ArchiveLbl),
  call_meta_warn(ArchiveAlias, ArchiveLbl, ArchiveHash, clean_archive(From)).

clean_archive(From, ArchiveHash, _) :-
  % Iterate over all entries inside the document stored in From.  We
  % need to catch TCP exceptions, because there will not be an input
  % stream to run the cleaning goal on.
  forall(rdf_call_on_stream(From, clean_entry(From, ArchiveHash)), true).

clean_entry(From, ArchiveHash, In, InPath, InPath) :-
  % Make sure that the HTTP status code is in the 2xx range.
  http_check_for_success(InPath),
  path_entry_name(InPath, EntryName),
  md5(From-EntryName, EntryHash),
  atomic_list_concat([e,ArchiveHash,EntryHash], :, EntryAlias),
  entry_label(From, ArchiveHash, EntryName, EntryHash, EntryLbl),
  call_meta_warn(
    EntryAlias,
    EntryLbl,
    EntryHash,
    clean_stream1(In, InPath, ArchiveHash)
  ).

clean_stream1(In, InPath, ArchiveHash, EntryHash, MetaM) :-
  q_dir_hash(EntryDir, EntryHash),
  q_graph_hash(MetaG, meta, EntryHash),
  clean_stream2(EntryDir, OutPath, TmpFile, CleanHash, In, InPath),
  OutPath = [OutEntry|_],
  % Handle the cleaned data file, if any.
  (   var(TmpFile)
  ->  true
  ;   q_graph_hash(DataG, data, CleanHash),
      qb(MetaM, MetaG, nsdef:dataGraph, DataG),
      % Compress the cleaned file.
      q_file_hash(CleanFile, data, ntriples, CleanHash),
      (   exists_file(CleanFile)
      ->  true
      ;   create_file_directory(CleanFile),
          compress_file(TmpFile, CleanFile),
          delete_file(TmpFile)
      ),
      hdt_prepare_file(CleanFile, HdtCleanFile),
      maplist(file_touch_ready, [CleanFile,HdtCleanFile]),
      % Link the entry directory to the data directory.
      file_directory_name(CleanFile, CleanDir),
      link_dirs(EntryDir, 'data.hdt', CleanDir),
      link_dirs(EntryDir, 'data.hdt.index', CleanDir),
      link_dirs(EntryDir, 'data.nt.gz', CleanDir),
      link_dirs(EntryDir, 'data.nt.gz.ready', CleanDir),
      % Store the number of tuples in RocksDB and ElasticSearch.
      get_dict(number_of_tuples, OutEntry, NumTuples)
  ),
  % Explicitly turn off compression when asserting metadata, otherwise
  % we compress twice.
  rdf_global_id(nsid:ArchiveHash, Archive),
  qb(MetaM, MetaG, nsdef:hasArchive, Archive),
  qb(MetaM, MetaG, nsdef:numberOfBytes, OutEntry.number_of_bytes^^xsd:nonNegativeInteger),
  qb(MetaM, MetaG, nsdef:numberOfCharacters, OutEntry.number_of_chars^^xsd:nonNegativeInteger),
  qb(MetaM, MetaG, nsdef:numberOfLines, OutEntry.number_of_lines^^xsd:nonNegativeInteger),
  dict_get(number_of_quads, OutEntry, 0, NumQuads),
  qb(MetaM, MetaG, nsdef:numberOfQuads, NumQuads^^xsd:nonNegativeInteger),
  dict_get(number_of_triples, OutEntry, 0, NumTriples),
  qb(MetaM, MetaG, nsdef:numberOfTriples, NumTriples^^xsd:nonNegativeInteger),
  dict_get(number_of_tuples, OutEntry, 0, NumTuples),
  qb(MetaM, MetaG, nsdef:numberOfTuples, NumTuples^^xsd:nonNegativeInteger),
  NumDuplicates is NumTuples - OutEntry.number_of_lines + 1,
  qb(MetaM, MetaG, nsdef:numberOfDuplicates, NumDuplicates^^xsd:nonNegativeInteger),
  dicts_getchk(rdf_media_type, InPath, MT),
  once(rdf_media_type(MT, Format, _)),
  qb(MetaM, MetaG, nsdef:rdfFormat, Format),
  forall(
    nth1(N, InPath, InEntry),
    rdf_store_metadata_entry(N, InEntry, MetaG, MetaM)
  ).

clean_stream2(EntryDir, OutPath2, TmpFile, CleanHash, In, InPath):-
  absolute_file_name(
    cleaning,
    TmpFile0,
    [access(write),relative_to(EntryDir)]
  ),
  thread_file(TmpFile0, TmpFile),
  rdf_call_to_ntriples(
    file(TmpFile),
    clean_stream3(In, InPath),
    [
      compression(false),
      md5(CleanHash),
      metadata(OutPath1),
      name(data),
      number_of_quads(NumQuads),
      number_of_triples(NumTriples),
      number_of_tuples(NumTuples)
    ]
  ),
  % Sort the N-Triples on disk.
  sort_file(TmpFile),
  OutPath1 = [OutEntry1|OutPath],
  OutEntry2 = OutEntry1.put(_{
    number_of_quads: NumQuads,
    number_of_triples: NumTriples,
    number_of_tuples: NumTuples
  }),
  OutPath2 = [OutEntry2|OutPath].

clean_stream3(In, InPath, State, Out) :-
  rdf_call_on_tuples_stream(In, clean_tuple(State, Out), InPath).

clean_tuple(State, Out, _, S, P, O, G) :-
  rdf_write_ntuple(S, P, O, G, State, Out).





% HELPERS %

%! archive_label(+From, +ArchiveHash, -ArchiveLbl) is det.

archive_label(From, ArchiveHash, ArchiveLbl) :-
  format(string(ArchiveLbl), "[A] ~a (~a)", [From,ArchiveHash]).



%! call_meta_warn(+Alias, +Lbl, +Hash, :Goal_2) is det.
%
% Calls ‘Goal_2(+Hash,+MetaM)’ while storing all metadata and warnings
% inside a Hash-based directory and graph.
%
% @arg Alias is the name of the thread.  This allows us to see for
%      each error messages which archive/entry it is about.
%
% @arg Lbl is the label is solely used for display in dedicated debug
%      messages, under debug flags ‘wm(begin)’, ‘wm(end)’, and
%      ‘wm(done)’.
%
% @arg Hash is either the hash of an archive or an entry.  This is
%      used to name the files/directory and graph.
%
% @arg Goal_2 is called with arguments ‘Hash’ and ‘MetaM’.

call_meta_warn(Alias, Lbl, Hash, Goal_2) :-
  q_dir_hash(Dir, Hash),
  with_mutex(ll, existed_dir(Dir, Existed)),
  (   Existed == true
  ->  debug(wm(done), "No need to recrawl ~s", [Lbl])
  ;   debug(wm(begin), "»~a ~s", [Alias,Lbl]),
      call_in_thread(Alias, call_meta_warn(Hash, Goal_2)),
      debug(wm(end), "«~a ~s", [Alias,Lbl])
  ).

call_meta_warn(Hash, Goal_2) :-
  % Use the Hash directory Dir to assert metadata (MetaFile) and
  % warnings (WarnFile).
  q_dir_hash(Dir, Hash),
  q_dir_file(Dir, meta, ntriples, MetaFile),
  q_dir_file(Dir, warn, ntriples, WarnFile),
  call_to_streams(
    file(MetaFile),
    file(WarnFile),
    call_meta_warn_streams(Hash, Goal_2)
  ),
  hdt_prepare_file(MetaFile),
  file_touch_ready(MetaFile),
  hdt_prepare_file(WarnFile),
  file_touch_ready(WarnFile).

call_meta_warn_streams(Hash, Goal_2, MetaOut, WarnOut) :-
  Opts = [rdf_media_type(application/'n-quads')],
  MetaOpts = [name(meta)|Opts],
  rdf__io:rdf_write_ntuples_begin(MetaState, MetaOpts),
  MetaM = stream(MetaState,MetaOut),
  WarnOpts = [name(warn)|Opts],
  rdf__io:rdf_write_ntuples_begin(WarnState, WarnOpts),
  % @hack Count the number of warnings.  WarnState should be able to
  %       do this as well.
  flag(Hash, _, 0),
  q_graph_hash(MetaG, meta, Hash),
  % Assert all warnings as RDF.
  asserta((
    user:thread_message_hook(Term,Kind,_) :-
      error_kind(Kind),
      flag(Hash, NumWarns, NumWarns + 1),
      % @bug WarnState gets reset each time so that
      %      ‘number_of_triples’ equals 1.
      rdf_store_warning(stream(WarnState,WarnOut), MetaG, Term)
  )),
  (catch(call(Goal_2, Hash, MetaM), E, true) -> true ; E = fail),
  (   var(E)
  ->  End = true
  ;   with_output_to(string(End), write_term(E)),
      msg_warning("[FAILED] ~s (~a)~n", [End,Hash])
  ),
  qb(MetaM, MetaG, nsdef:end, End^^xsd:string),
  flag(Hash, NumWarns, 0),
  qb(MetaM, MetaG, nsdef:warnings, NumWarns^^xsd:nonNegativeInteger),
  rdf__io:rdf_write_ntuples_end(MetaState, MetaOpts),
  rdf__io:rdf_write_ntuples_end(WarnState, WarnOpts).



%! entry_label(+From, +ArchiveHash, +EntryName, +EntryHash, -EntryLbl) is det.

entry_label(From, ArchiveHash, EntryName, EntryHash, EntryLbl) :-
  format(
    string(EntryLbl),
    "[E] ~a ~a (~a ~a)",
    [From,EntryName,ArchiveHash,EntryHash]
  ).



%! existed_dir(+Dir, -Existed) is det.
%
% Ensures that directory Dir is created and returns whether or not it
% already existed before.

existed_dir(Dir, true) :-
  exists_directory(Dir), !.
existed_dir(Dir, false) :-
  make_directory_path(Dir).



%! http_check_for_success(+InPath) is det.
%
% @throws http_status/2 exception

http_check_for_success(InPath) :-
  % The last HTTP status code must have been success.
  (   dicts_getchk(status, InPath, Status)
  ->  (   http_status_is_success(Status)
      ->  true
      ;   throw(http_status(Status, '2xx'))
      )
  ;   true
  ).



%! link_dirs(+Dir1, +Local, +Dir2) is det.

link_dirs(Dir1, Local, Dir2) :-
  directory_file_path(Dir1, Local, File1),
  directory_file_path(Dir2, Local, File2),
  create_file_link(File1, File2).



rdf_store_metadata_entry(N, InEntry, G, M) :-
  rdfs_container_membership_property(P, N),
  atom_concat('_:', N, S),
  qb(M, G, P, S),
  dict_pairs(InEntry, InPairs),
  maplist(rdf_store_metadata_entry_pair(M, S), InPairs).


% Properties that are skipped.
rdf_store_metadata_entry_pair(_, _, filetype-_).
rdf_store_metadata_entry_pair(_, _, mode-_).
rdf_store_metadata_entry_pair(_, _, mtime-_).
rdf_store_metadata_entry_pair(_, _, name-_).
rdf_store_metadata_entry_pair(_, _, newline-_).
rdf_store_metadata_entry_pair(_, _, number_of_bytes-_).
rdf_store_metadata_entry_pair(_, _, number_of_chars-_).
rdf_store_metadata_entry_pair(_, _, number_of_lines-_).
rdf_store_metadata_entry_pair(_, _, rdf_media_type-_).
rdf_store_metadata_entry_pair(_, _, size-_).


% Properties for which metadata is stored.
rdf_store_metadata_entry_pair(M, S, '@id'-Uri) :-
  qb(M, S, nsdef:uri, Uri).
rdf_store_metadata_entry_pair(M, S, '@type'-Local) :-
  rdf_global_id(nsdef:Local, C),
  qb(M, S, rdf:type, C).
rdf_store_metadata_entry_pair(M, S, filters-Filters) :-
  atomic_list_concat(Filters, ',', A),
  qb(M, S, nsdef:filters, A^^xsd:string).
rdf_store_metadata_entry_pair(M, S, format-Format) :-
  (   Format == raw
  ->  true
  ;   qb(M, S, nsdef:format, Format^^xsd:string)
  ).
rdf_store_metadata_entry_pair(M, S, headers-Dict) :-
  dict_pairs(Dict, Pairs),
  forall(
    member(Key-Vals, Pairs),
    forall(
      member(Val, Vals),
      (
        rdf_global_id(nsdef:Key, P),
        qb(M, S, P, Val.raw^^xsd:string)
      )
    )
  ).
rdf_store_metadata_entry_pair(M, S, permissions-Mask) :-
  qb(M, S, nsdef:permissions, Mask^^xsd:string).
rdf_store_metadata_entry_pair(M, S, status-Status) :-
  qb(M, S, nsdef:status, Status^^xsd:positiveInteger).
rdf_store_metadata_entry_pair(M, S, time-Time) :-
  qb(M, S, nsdef:time, Time^^xsd:float).
rdf_store_metadata_entry_pair(M, S, version-Version) :-
  qb(M, S, nsdef:version, Version^^xsd:string).
rdf_store_metadata_entry_pair(_, _, Key-Val) :-
  gtrace, %DEB
  writeln(Key-Val).
