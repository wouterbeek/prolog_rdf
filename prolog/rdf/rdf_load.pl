:- module(
  rdf_load,
  [
    rdf_call_on_graph/2,    % +Source, :Goal_1
    rdf_call_on_graph/3,    % +Source, :Goal_1, +Opts
    rdf_call_on_tuples/2,   % +Source, :Goal_2
    rdf_call_on_tuples/3,   % +Source, :Goal_2, +Opts
    rdf_download_to_file/2, % +Iri, +File
    rdf_download_to_file/3, % +Iri, +File, +Opts
    rdf_load_file/1,        % +Source
    rdf_load_file/2,        % +Source, +Opts
    rdf_load_tuples/2,      % +Source, -Tuples
    rdf_load_tuples/3       % +Source, -Tuples, +Opts
  ]
).
:- reexport(library(semweb/rdf_db), [
     rdf_make/0,
     rdf_source_location/2 % +Subject, -Location
   ]).

/** <module> RDF load

Support for loading RDF data.

@author Wouter Beek
@version 2015/08, 2015/10-2016/03
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(http/json)).
:- use_module(library(iri/rfc3987_gen)).
:- use_module(library(jsonld/jsonld_metadata)).
:- use_module(library(jsonld/jsonld_read)).
:- use_module(library(option)).
:- use_module(library(os/file_ext)).
:- use_module(library(os/io_ext)).
:- use_module(library(os/thread_counter)).
:- use_module(library(rdf), [process_rdf/3]).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(rdf/rdf_file)). % Type definition.
:- use_module(library(rdf/rdf_graph)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(rdf/rdf_stream)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfa), [read_rdfa/3]).
:- use_module(library(semweb/rdf_ntriples), [rdf_process_ntriples/3]).
:- use_module(library(semweb/turtle), [rdf_process_turtle/3]).
:- use_module(library(uuid_ext)).
:- use_module(library(yall)).

:- meta_predicate
    rdf_call_on_graph(+,1),
    rdf_call_on_graph(+,1,+),
    rdf_call_on_tuples(+,2),
    rdf_call_on_tuples(+,2,+),
    rdf_call_on_tuples_stream(+,2,+,+).

:- predicate_options(rdf_call_on_graph/3, 3, [
     pass_to(rdf_load_file/2)
   ]).
:- predicate_options(rdf_call_on_tuples/3, 3, [
     pass_to(rdf_read_from_stream/3, 3)
   ]).
:- predicate_options(rdf_download_to_file/3, 3, [
     pass_to(rdf_read_from_stream/3, 3)
   ]).
:- predicate_options(rdf_load_file/2, 2, [
     quads(-nonneg),
     triples(-nonneg),
     tuples(-nonneg),
     pass_to(rdf_call_on_tuples/3, 3)
   ]).
:- predicate_options(rdf_load_tuples/3, 3, [
     pass_to(rdf_load_file/2, 2)
   ]).





%! rdf_call_on_graph(+Source, :Goal_1) .
%! rdf_call_on_graph(+Source, :Goal_1, +Opts) .
%
% @throws existence_error if an HTTP request returns an error code.

rdf_call_on_graph(Source, Goal_1) :-
  rdf_call_on_graph(Source, Goal_1, []).

rdf_call_on_graph(Source, Goal_1, Opts0) :-
  setup_call_cleanup(
    rdf_tmp_graph(G),
    (
      merge_options([graph(G)], Opts0, Opts),
      rdf_load_file(Source, Opts),
      call(Goal_1, G)
    ),
    rdf_unload_graph(G)
  ).



%! rdf_call_on_tuples(+Source, :Goal_2) is nondet.
%! rdf_call_on_tuples(+Source, :Goal_2, +Opts) is nondet.
%
% @throws existence_error if an HTTP request returns an error code.

rdf_call_on_tuples(Source, Goal_2) :-
  rdf_call_on_tuples(Source, Goal_2, []).

rdf_call_on_tuples(Source, Goal_2, Opts) :-
  option(graph(G1), Opts, default),
  rdf_global_id(G1, G2),
  catch(
    rdf_read_from_stream(Source, rdf_call_on_tuples_stream(G2, Goal_2), Opts),
    E,
    (writeln(Goal_2), print_message(warning, E))
  ).


%! rdf_call_on_tuples_stream(?G, :Goal_2, +Metadata, +Read) is det.
% The following call is made:
% `call(:Goal_2, +Tuples:list(compound), ?Graph:atom)`

rdf_call_on_tuples_stream(G, Goal_2, M, Read) :-
  BaseIri = M.'llo:base_iri'.'@value',
  rdf_equal(M.'llo:rdf_format', Format1),
  jsonld_metadata_expand_iri(Format1, Format2),
  rdf_format_iri(Format3, Format2),
  (   memberchk(Format3, [nquads,ntriples])
  ->  rdf_process_ntriples(
        Read,
        Goal_2,
        [base_uri(BaseIri),format(Format3),graph(G)]
      )
  ;   memberchk(Format3, [trig,turtle])
  ->  uuid_no_hyphen(UniqueId),
      atomic_list_concat(['__',UniqueId,:], BPrefix),
      Opts = [anon_prefix(BPrefix),base_uri(BaseIri),graph(G)],
      rdf_process_turtle(Read, Goal_2, Opts)
  ;   Format3 == jsonld
  ->  json_read_dict(Read, Json),
      forall(
        jsonld_tuple(Json, Tuple, [base_iri(BaseIri)]),
        call(Goal_2, [Tuple], G)
      )
  ;   Format3 == xml
  ->  process_rdf(Read, Goal_2, [base_uri(BaseIri),graph(G)])
  ;   Format3 == rdfa
  ->  read_rdfa(Read, Ts, [max_errors(-1),syntax(style)]),
      call(Goal_2, Ts, G)
  ;   existence_error(rdf_format, [Format1])
  ).



%! rdf_download_to_file(+Iri, +File) is det.
%! rdf_download_to_file(+Iri, ?File, +Opts) is det.
% Options are passed to rdf_read_from_stream/3 and write_stream_to_file/3.
%
% @throws existence_error if an HTTP request returns an error code.

rdf_download_to_file(Iri, File) :-
  rdf_download_to_file(Iri, File, []).

rdf_download_to_file(Iri, File, Opts) :-
  thread_file(File, TmpFile),
  rdf_read_from_stream(Iri, write_stream_to_file0(TmpFile, Opts), Opts),
  rename_file(TmpFile, File).

write_stream_to_file0(TmpFile, Opts, _, Read) :-
  write_stream_to_file(Read, TmpFile, Opts).



%! rdf_load_file(+Source) is det.
%! rdf_load_file(+Source, +Opts) is det.
% The following options are supported:
%   * base_iri(+atom)
%   * graph(+rdf_graph)
%   * quads(-nonneg)
%   * triples(-nonneg)
%   * tuples(-nonneg)
%
% @throws existence_error if an HTTP request returns an error code.

rdf_load_file(Source) :-
  rdf_load_file(Source, []).

rdf_load_file(Source, Opts) :-
  % Allow statistics about the number of tuples to be returned.
  option(quads(NoQuads), Opts, _),
  option(triples(NoTriples), Opts, _),
  option(tuples(NoTuples), Opts, _),

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
      create_thread_counter(quads)
    ),
    rdf_call_on_tuples(Source, rdf_load_tuples(triples, quads), Opts),
    (
      delete_thread_counter(triples, NoTriples),
      delete_thread_counter(quads, NoQuads),
      NoTuples is NoTriples + NoQuads,
      debug(
        rdf(load),
        "Loaded ~D tuples from ~w (~D triples and ~D quads).~n",
        [NoTuples,Source,NoTriples,NoQuads]
      )
    )
  ).

rdf_load_tuples(CT, CQ, Tuples, G) :-
  maplist(rdf_load_tuple(CT, CQ, G), Tuples).

rdf_load_tuple(CT, _, G, rdf(S,P,O)) :- !,
  increment_thread_counter(CT),
  rdf_load_tuple0(S, P, O, G).
rdf_load_tuple(_, CQ, _, rdf(S,P,O,G0)) :- !,
  rdf11:post_graph(G, G0),
  increment_thread_counter(CQ),
  rdf_load_tuple0(S, P, O, G).

rdf_load_tuple0(S, P, O0, G) :-
  (rdf_is_term(O0) -> O = O0 ; rdf11:post_object(O, O0)),
  %maplist(term_norm, [S1,P1,O1,G1], [S2,P2,O2,G2]),
  debug(rdf(load), rdf_print_tuple(S, P, O, G)),
  rdf_assert(S, P, O, G).

% @tbd
%term_norm(T1, T2) :- rdf_is_iri(T1), !, iri_norm(T1, T2).
%term_norm(T, T).



%! rdf_load_tuples(+Source, -Tuples) is det.
%! rdf_load_tuples(+Source, -Tuples, +Opts) is det.
%
% @throws existence_error if an HTTP request returns an error code.

rdf_load_tuples(Source, Tuples) :-
  rdf_load_tuples(Source, Tuples, []).

rdf_load_tuples(Source, Tuples, Opts) :-
  rdf_snap((
    rdf_retractall(_, _, _),
    rdf_load_file(Source, Opts),
    aggregate_all(set(Tuple), rdf_tuple(Tuple), Tuples)
  )).
