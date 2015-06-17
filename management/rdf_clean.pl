:- module(
  rdf_clean,
  [
    rdf_clean/1 % +From:compound
  ]
).

/** <module> RDF clean

Cleans RDF serialization formats by streaming.
The streaming window is one triples (for Turtle-family input)
or one subject term node (for RDF/XML input)
or the complete document (for RDFa input).

@author Wouter Beek
@version 2015/06
*/

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(filesex)).
:- use_module(library(option)).
:- use_module(library(rdf)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdf_ntriples)).
:- use_module(library(semweb/turtle)).
:- use_module(library(uri)).
:- use_module(library(uuid)).
:- use_module(library(zlib)).

:- use_module(plc(io/file_ext)).
:- use_module(plc(io/file_gnu)).
:- use_module(plc(io/open_any)).
:- use_module(plc(process/gnu_sort)).

:- use_module(plRdf(management/rdf_guess_format)).
:- use_module(plRdf(management/rdf_load_any)).
:- use_module(plRdf(syntax/ctriples/ctriples_write_generics)).
:- use_module(plRdf(syntax/ctriples/ctriples_write_graph)).
:- use_module(plRdf(syntax/ctriples/ctriples_write_triples)).

:- thread_local(has_quadruples/1).





%! rdf_clean(+In:compound) is det.

rdf_clean(In):-
  % Set the HTTP headers for RDF retrieval.
  rdf_http_plugin:rdf_extra_headers(HttpOpts, []),
  
  forall(
    open_any(In, SubIn, M, HttpOpts),
    call_cleanup(
      rdf_transaction(
        rdf_clean_stream(In, M),
        _,
        [snapshot(true)]
      ),
      close(SubIn)
    )
  ).

rdf_clean_stream(In, M):-
  absolute_file_name(data(.), Dir, [access(write),file_type(directory)]),
  
  % Extract the base URI.
  uuid(Uuid0),
  atomic_list_concat(UuidComps, '-', Uuid0),
  atomic_list_concat(UuidComps, '', Uuid),
  atomic_list_concat(['',Uuid], '/', Path),
  uri_components(BaseUri, uri_components(http,'lodlaundromat.org',Path,_,_)),
  
  % Guess the RDF serialization format based on the file name extension.
  ignore(file_name_extension(_, InExt, BaseUri)),
  rdf_guess_format(In, InExt, _, InFormat),
  
  % Set options: base URI, RDF serialization format, XML namespaces.
  set_stream(In, file_name(Base)),
  InOpts = [
    base_uri(BaseUri),
    format(InFormat),
    graph(user),
    max_errors(-1),
    register_namespaces(false),
    silent(true),
    syntax(style)
  ],
  OutOpts = [bnode_base(Scheme-Authority-Uuid)],
  
  absolute_file_name(data(cleaning), CleaningFile, [access(write)]),
  (   InFormat == rdfa
  ->  rdf_load(stream(In), InOpt3),
      setup_call_cleanup(
        open(CleaningFile, write, UnsortedOut),
        ctriples_write_graph(UnsortedOut, _NoGraph, OutOpts),
        close(UnsortedOut)
      )
  ;   setup_call_cleanup(
        ctriples_write_begin(State, BNodePrefix, OutOpts),
        setup_call_cleanup(
          open(CleaningFile, write, UnsortedOut),
          clean_triples(
            InFormat,
            In,
            UnsortedOut,
            State,
            BNodePrefix,
            InOpts3
          ),
          close(UnsortedOut)
        ),
        ctriples_write_end(State, OutOpts)
      )
  ),
  
  % Establish the file name extension.
  (   retract(has_quadruples(true))
  ->  Ext = nq
  ;   Ext = nt
  ),
  
  % Sort file.
  sort_file(CleaningFile, Dir),

  file_lines(CleaningFile, NumberOfUniqueTriples),
  format(user_output, 'Unique triples:~t~D~n', [NumberOfUniqueTriples]),

  % Compress file.
  compress_file(Dir, Ext, CleaningFile),
  
  delete_file(CleaningFile).





% HELPERS %

clean_triples(xml, In, Out, State, BNodePrefix, Options):- !,
  process_rdf(
    In,
    clean_streamed_triples(Out, State, BNodePrefix),
    Options
  ).
clean_triples(Format, In, Out, State, BNodePrefix, Options1):-
  memberchk(Format, [nquads,ntriples]), !,
  merge_options([anon_prefix(BNodePrefix)], Options1, Options2),
  rdf_process_ntriples(
    In,
    clean_streamed_triples(Out, State, BNodePrefix),
    Options2
  ).
clean_triples(Format, In, Out, State, BNodePrefix, Options1):-
  memberchk(Format, [trig,turtle]), !,
  merge_options([anon_prefix(BNodePrefix)], Options1, Options2),
  rdf_process_turtle(
    In,
    clean_streamed_triples(Out, State, BNodePrefix),
    Options2
  ).

%! clean_streamed_triples(
%!   +Out:stream,
%!   +State:compound,
%!   +BNodePrefix:atom,
%!   +Triples:compound,
%!   +LinePosition:compound
%! ) is det.

clean_streamed_triples(Out, State, BNodePrefix, Triples0, Graph0):-
  graph_without_line(Graph0, Graph),
  maplist(fix_triple(Graph), Triples0, Triples),
  maplist(ctriples_write_triple(Out, State, BNodePrefix), Triples).

%! graph_without_line(+WonkyGraph:compound, -Graph:atom) is det.
% Remove file line numbers from the graph name.

graph_without_line(Graph:_, Graph):- !.
graph_without_line(Graph, Graph).

%! fix_triple(
%!   +Graph:atom,
%!   +WonkyStatement:compound,
%!   -Statement:compound
%! ) is det.
%

fix_triple(Graph, rdf(S,P,O), Triple):- !,
  (   is_named_graph(Graph)
  ->  set_has_quadruples,
      Triple = rdf(S,P,O,Graph)
  ;   Triple = rdf(S,P,O)
  ).
fix_triple(Graph, rdf(S,P,O,G0), Triple):-
  (   graph_without_line(G0, G),
      is_named_graph(G)
  ->  set_has_quadruples,
      Triple = rdf(S,P,O,G)
  ;   is_named_graph(Graph)
  ->  set_has_quadruples,
      Triple = rdf(S,P,O,Graph)
  ;   Triple = rdf(S,P,O)
  ).

%! is_named_graph(+Graph:atom) is semidet.
% Succeeds for all and only named graphs.

is_named_graph(Graph):-
  ground(Graph),
  Graph \== user.

%! set_has_quadruples is det.
% Store the fact that a quadruple occurred in the parser stream
% as a thread-local global Prolog fact.

set_has_quadruples:-
  has_quadruples(true), !.
set_has_quadruples:-
  assert(has_quadruples(true)).



%! compress_file(+Directory:atom, +Extension:atom, +CleaningFile:atom) is det.

compress_file(Dir, Ext, CleaningFile):-
  atomic_list_concat([clean,Ext,gz], '.', LocalName),
  directory_file_path(Dir, LocalName, CleanFile),
  setup_call_cleanup(
    gzopen(CleanFile, write, CleanOut),
    setup_call_cleanup(
      open(CleaningFile, read, SortedIn),
      copy_stream_data(SortedIn, CleanOut),
      close(SortedIn)
    ),
    close(CleanOut)
  ).



%! sort_file(+File:atom, ?TmpDirectory) is det.

sort_file(CleaningFile, SortDir):-
  (   var(SortDir)
  ->  absolute_file_name(
        data(.),
        SortDir,
        [access(write),file_type(directory)]
      )
  ;   true
  ),
  buffer_size_file(CleaningFile, BufferSize),
  (   BufferSize > 6 * (1024 ** 3) % >6GB
  ->  Threads = 3
  ;   BufferSize > 3 * (1024 ** 3) % >3GB
  ->  Threads = 2
  ;   Threads = 1 % =<3GB
  ),
  gnu_sort(
    CleaningFile,
    [
      buffer_size(BufferSize),
      duplicates(false),
      output(CleaningFile),
      parallel(Threads),
      temporary_directory(SortDir),
      utf8(true)
    ]
  ).
