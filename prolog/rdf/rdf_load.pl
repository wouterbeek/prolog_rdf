:- module(
  rdf_load,
  [
    rdf_call_on_graph/2,      % +Source, :Goal_1
    rdf_call_on_graph/3,      % +Source, :Goal_1, +Opts
    rdf_call_on_statements/2, % +Source, :Goal_2
    rdf_call_on_statements/3, % +Source, :Goal_2, +Opts
    rdf_download_to_file/2,   % +Iri, +File
    rdf_download_to_file/3,   % +Iri, +File, +Opts
    rdf_load_file/1,          % +Source
    rdf_load_file/2,          % +Source, +Opts
    rdf_load_statements/2,    % +Source, -Stmts
    rdf_load_statements/3     % +Source, -Stmts, +Opts
  ]
).
:- reexport(library(semweb/rdf_db), [
     rdf_make/0,
     rdf_source_location/2 % +Subject, -Location
   ]).

/** <module> RDF load

Support for loading RDF data.

@author Wouter Beek
@version 2015/08, 2015/10-2016/01
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(iri/rfc3987_gen)).
:- use_module(library(msg_ext)).
:- use_module(library(option)).
:- use_module(library(os/file_ext)).
:- use_module(library(os/io_ext)).
:- use_module(library(os/thread_counter)).
:- use_module(library(rdf), [process_rdf/3]).
:- use_module(library(rdf/rdf_api)).
:- use_module(library(rdf/rdf_graph)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(rdf/rdf_statement)).
:- use_module(library(rdf/rdf_stream)).
:- use_module(library(semweb/rdfa), [read_rdfa/3]).
:- use_module(library(semweb/rdf_ntriples), [rdf_process_ntriples/3]).
:- use_module(library(semweb/turtle), [rdf_process_turtle/3]).
:- use_module(library(uuid_ext)).
:- use_module(library(yall)).

:- meta_predicate
    rdf_call_on_graph(+,1),
    rdf_call_on_graph(+,1,+),
    rdf_call_on_statements(+,2),
    rdf_call_on_statements(+,2,+),
    rdf_call_on_statements_stream(+,2,+,+).

:- predicate_options(rdf_call_on_graph/3, 3, [
     pass_to(rdf_load_file/2)
   ]).
:- predicate_options(rdf_call_on_statements/3, 3, [
     pass_to(rdf_read_from_stream/3, 3)
   ]).
:- predicate_options(rdf_download_to_file/3, 3, [
     pass_to(rdf_read_from_stream/3, 3)
   ]).
:- predicate_options(rdf_load_file/2, 2, [
     quadruples(-nonneg),
     statements(-nonneg),
     triples(-nonneg),
     pass_to(rdf_call_on_statements/3, 3)
   ]).
:- predicate_options(rdf_load_statements/3, 3, [
     pass_to(rdf_load_file/2, 2)
   ]).





%! rdf_call_on_graph(+Source, :Goal_1) .
% Wrapper around rdf_call_on_graph/3 with default options.
%
% @throws existence_error if an HTTP request returns an error code.

rdf_call_on_graph(In, Goal_1) :-
  rdf_call_on_graph(In, Goal_1, []).


%! rdf_call_on_graph(+Source, :Goal_1, +Options:list(compound)) .
%
% @throws existence_error if an HTTP request returns an error code.

rdf_call_on_graph(In, Goal_1, Opts0) :-
  setup_call_cleanup(
    rdf_tmp_graph(G),
    (
      merge_options([graph(G)], Opts0, Opts),
      rdf_load_file(In, Opts),
      call(Goal_1, G)
    ),
    rdf_unload_graph(G)
  ).



%! rdf_call_on_statements(+Source, :Goal_2) is nondet.
% Wrapper around rdf_call_on_statements/3 with default options.
%
% @throws existence_error if an HTTP request returns an error code.

rdf_call_on_statements(In, Goal_2) :-
  rdf_call_on_statements(In, Goal_2, []).


%! rdf_call_on_statements(+Source, :Goal_2, +Options:list(compound)) is nondet.
%
% @throws existence_error if an HTTP request returns an error code.

rdf_call_on_statements(In, Goal_2, Opts) :-
  option(graph(G0), Opts, default),
  rdf_global_id(G0, G),
  catch(
    rdf_read_from_stream(In, rdf_call_on_statements_stream(G, Goal_2), Opts),
    E,
    (writeln(Goal_2), print_message(warning, E))
  ).


%! rdf_call_on_statements_stream(
%!   ?Graph:iri,
%!   :Goal_2,
%!   +Metadata:dict,
%!   +Read:stream
%! ) is det.
% The following call is made:
% `call(:Goal_2, +Statements:list(compound), ?Graph:atom)'

rdf_call_on_statements_stream(G, Goal_2, M, Read) :-
  memberchk(M.rdf.format, [nquads,ntriples]), !,
  rdf_process_ntriples(
    Read,
    Goal_2,
    [base_uri(M.base_iri),format(M.rdf.format),graph(G)]
  ).
rdf_call_on_statements_stream(G, Goal_2, M, Read) :-
  memberchk(M.rdf.format, [trig,turtle]), !,
  uuid_no_hyphen(UniqueId),
  atomic_list_concat(['__',UniqueId,:], BNodePrefix),
  Opts = [anon_prefix(BNodePrefix),base_uri(M.base_iri),graph(G)],
  rdf_process_turtle(Read, Goal_2, Opts).
rdf_call_on_statements_stream(G, Goal_2, M, Read) :-
  xml == M.rdf.format, !,
  process_rdf(Read, Goal_2, [base_uri(M.base_iri),graph(G)]).
rdf_call_on_statements_stream(G, Goal_2, M, Read) :-
  rdfa == M.rdf.format, !,
  read_rdfa(Read, Ts, [max_errors(-1),syntax(style)]),
  call(Goal_2, Ts, G).
rdf_call_on_statements_stream(_, _, _, M) :-
  msg_warning("Unrecognized RDF serialization format: ~a~n", [M.rdf.format]).



%! rdf_download_to_file(+Iri, +File:atom) is det.
%! rdf_download_to_file(+Iri, ?File:atom, +Opts) is det.
% @throws existence_error if an HTTP request returns an error code.

rdf_download_to_file(Iri, File) :-
  rdf_download_to_file(Iri, File, []).

rdf_download_to_file(Iri, File, Opts) :-
  thread_file(File, TmpFile),
  rdf_read_from_stream(Iri, write_stream_to_file0(TmpFile), Opts),
  rename_file(TmpFile, File).

write_stream_to_file0(TmpFile, _, Read) :-
  write_stream_to_file(Read, TmpFile).



%! rdf_load_file(+Source) is det.
% Wrapper around rdf_load_file/2 with default options.
%
% @throws existence_error if an HTTP request returns an error code.

rdf_load_file(In) :-
  rdf_load_file(In, []).


%! rdf_load_file(+Source, +Options:list(compound)) is det.
% The following options are supported:
%   * base_iri(+atom)
%   * graph(+rdf_graph)
%   * triples(-nonneg)
%   * quadruples(-nonneg)
%   * statements(-nonneg)
%
% @throws existence_error if an HTTP request returns an error code.

rdf_load_file(In, Opts) :-
  % Allow statistics about the number of statements to be returned.
  option(quadruples(NQ), Opts, _),
  option(triples(NT), Opts, _),
  option(statements(NS), Opts, _),

  % In the absence of a graph name use the base IRI.
  (   option(graph(G), Opts, _),
      var(G),
      option(base_iri(BaseIri), Opts)
  ->  G = BaseIri
  ;   true
  ),
  setup_call_cleanup(
    (
      create_thread_counter(triples),
      create_thread_counter(quadruples)
    ),
    rdf_call_on_statements(In, rdf_load_statements(triples, quadruples), Opts),
    (
      delete_thread_counter(triples, NT),
      delete_thread_counter(quadruples, NQ),
      NS is NT + NQ,
      debug(
        rdf(load),
        "Loaded ~D statements from ~w (~D triples and ~D quadruples).~n",
        [NS,In,NT,NQ]
      )
    )
  ).


rdf_load_statements(CT, CQ, Stmts, G:_) :-
  maplist(rdf_load_statement(CT, CQ, G), Stmts).


% Load a triple.
rdf_load_statement(CT, _, G, rdf(S,P,O)) :- !,
  increment_thread_counter(CT),
  rdf_load_statement0(S, P, O, G).
% Load a quadruple.
rdf_load_statement(_, CQ, _, rdf(S,P,O,G:_)) :- !,
  increment_thread_counter(CQ),
  rdf_load_statement0(S, P, O, G).

rdf_load_statement0(S1, P1, O0, G1) :-
  rdf11:post_object(O1, O0),
  maplist(term_norm, [S1,P1,O1,G1], [S2,P2,O2,G2]),
  with_output_to(user_output, rdf_print_triple(S2, P2, O2, G2)),
  rdf_assert(S2, P2, O2, G2).

term_norm(T1, T2) :- rdf_is_iri(T1), !, iri_norm(T1, T2).
term_norm(T, T).



%! rdf_load_statements(+Source, -Stmts) is det.
% Wrapper around rdf_load_statements/3 using default options.
%
% @throws existence_error if an HTTP request returns an error code.

rdf_load_statements(Source, Stmts) :-
  rdf_load_statements(Source, Stmts, []).


%! rdf_load_statements(+Source, -Stmts, +Opts) is det.
%
% @throws existence_error if an HTTP request returns an error code.

rdf_load_statements(Source, Stmts, Opts) :-
  rdf_snap((
    rdf_retractall(_, _, _),
    rdf_load_file(Source, Opts),
    aggregate_all(set(Stmt), rdf_statement(Stmt), Stmts)
  )).
