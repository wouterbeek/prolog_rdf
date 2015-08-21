:- module(
  rdf_clean,
  [
    rdf_clean/1, % +From:compound
    rdf_clean/2 % +From:compound
                % +To:atom
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
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).
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
  absolute_file_name(data(.), Dir, [access(write),file_type(directory)]),
  rdf_clean(In, Dir).

%! rdf_clean(+In:compound, +Directory:atom) is det.

rdf_clean(In, Dir):-
  % Set the HTTP headers for RDF retrieval.
  rdf_http_plugin:rdf_extra_headers(HttpOpts, []),

  forall(
    open_any(In, SubIn, M, HttpOpts),
    call_cleanup(
      rdf_transaction(
        rdf_clean_stream(SubIn, M, Dir),
        _,
        [snapshot(true)]
      ),
      close(SubIn)
    )
  ).

%! rdf_clean_stream(In:stream) is det.

rdf_clean_stream(In, M, Dir):-
  % Format, base URI, wfu.
  create_bases(BaseUri, BNodeBase),
  set_stream(In, file_name(BaseUri)),
  rdf_guess_format(In, M, InFormat),
  
  % Cleaning.
  call_cleanup(
    (
      absolute_file_name(data(cleaning), CleaningFile, [access(write)]),
      rdf_clean_triples(In, BaseUri, InFormat, CleaningFile, BNodeBase),
      
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
      compress_file(Dir, Ext, CleaningFile)
    ),
    delete_file(CleaningFile)
  ).





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




%! create_bases(-BaseUri:atom, BNodeBase:compound) is det.

create_bases(BaseUri, http-'lodlaundromat.org'-Uuid):-
  uuid(Uuid0),
  atomic_list_concat(UuidComps, -, Uuid0),
  atomic_list_concat(UuidComps, '', Uuid),
  atomic_list_concat(['',Uuid], /, Path),
  uri_components(BaseUri, uri_components(http,'lodlaundromat.org',Path,_,_)).



%! rdf_clean_triples(
%!   +In:stream,
%!   +BaseUri:uri,
%!   +InFormat:atom,
%!   +CleaningFile:atom,
%!   +BNodeBase:compound
%! ) is det.

rdf_clean_triples(In, BaseUri, InFormat, CleaningFile, BNodeBase):-
  InOpts = [
    base_uri(BaseUri),
    format(InFormat),
    graph(user),
    max_errors(-1),
    register_namespaces(false),
    silent(true),
    syntax(style)
  ],
  OutOpts = [bnode_base(BNodeBase)],
  rdf_clean_triples0(InFormat, In, InOpts, CleaningFile, OutOpts).

%! rdf_clean_triples0(
%!   +Format:oneof([nquads,ntriples,rdfa,trig,turtle,xml]),
%!   +In:stream,
%!   +InOpts:list(compound),
%!   +CleaningFile:atom,
%!   +OutOpts:list(compound)
%! ) is det.

rdf_clean_triples0(rdfa, In, InOpts, CleaningFile, OutOpts):-
  rdf_load(stream(In), InOpts),
  setup_call_cleanup(
    open(CleaningFile, write, UnsortedOut),
    ctriples_write_graph(UnsortedOut, _NoGraph, OutOpts),
    close(UnsortedOut)
  ).
rdf_clean_triples0(Format, In, InOpts, CleaningFile, OutOpts):-
  setup_call_cleanup(
    ctriples_write_begin(State, BNodePrefix, OutOpts),
    setup_call_cleanup(
      open(CleaningFile, write, UnsortedOut),
      clean_triples(
        Format,
        In,
        UnsortedOut,
        State,
        BNodePrefix,
        InOpts
      ),
      close(UnsortedOut)
    ),
    ctriples_write_end(State, OutOpts)
  ).



%! rdf_guess_format(+In:stream, +Metadata:dict, -Format:atom) is det.
% Guesses the RDF serialization format based on the file name extension.

rdf_guess_format(In, M, Format):-
  ignore(file_name_extension(_, Ext, M.path)),
  rdf_guess_format(In, Ext, _, Format).
