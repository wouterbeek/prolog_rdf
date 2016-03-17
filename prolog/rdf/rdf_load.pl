:- module(
  rdf_load,
  [
    rdf_call_on_graph/2,    % +Source, :Goal_1
    rdf_call_on_graph/3,    % +Source, :Goal_1, +Opts
    rdf_call_on_tuples/2,   % +Source, :Goal_4
    rdf_call_on_tuples/3,   % +Source, :Goal_4, +Opts
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
    rdf_call_on_quad(4, +),
    rdf_call_on_quads(4, +),
    rdf_call_on_quads(4, +, +),
    rdf_call_on_tuples(+,4),
    rdf_call_on_tuples(+,4,+),
    rdf_call_on_tuples_stream(4,+,+,+).

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



%! rdf_call_on_tuples(+Source, :Goal_4) is nondet.
%! rdf_call_on_tuples(+Source, :Goal_4, +Opts) is nondet.
%
% @throws existence_error if an HTTP request returns an error code.

rdf_call_on_tuples(Source, Goal_4) :-
  rdf_call_on_tuples(Source, Goal_4, []).

rdf_call_on_tuples(Source, Goal_4, Opts) :-
  catch(
    rdf_read_from_stream(Source, rdf_call_on_tuples_stream(Goal_4, Opts), Opts),
    E, (writeln(Goal_4), print_message(warning, E))
  ).


%! rdf_call_on_tuples_stream(:Goal_4, +Opts, +Metadata, +Source) is det.
% The following call is made: `call(:Goal_4, +S, +P, +O, +G)`.

rdf_call_on_tuples_stream(Goal_4, Opts1, M, Source) :-
  % Library Semweb uses option base_uri/1.  We use option base_iri/1.
  BaseIri = M.'llo:base_iri'.'@value',
  jsonld_metadata_expand_iri(M.'llo:rdf_format', FormatIri),
  rdf_format_iri(Format, FormatIri),
  %%%%uuid_no_hyphen(UniqueId),
  %%%%atomic_list_concat(['_:',UniqueId,-], BPrefix),
  Opts2 = [
    annon_prefix('_:'),
    base(BaseIri),
    base_iri(BaseIri),
    base_uri(BaseIri),
    format(Format),
    max_errors(-1),
    syntax(style)
  ],
  merge_options(Opts1, Opts2, Opts3),
  (   % N-Quads & N-Triples.
      memberchk(Format, [nquads,ntriples])
  ->  rdf_process_ntriples(Source, rdf_call_on_quads(Goal_4), Opts3)
  ;   % Trig & Turtle.
      memberchk(Format, [trig,turtle])
  ->  rdf_process_turtle(Source, rdf_call_on_quads(Goal_4), Opts3)
  ;   % JSON-LD.
      Format == jsonld
  ->  json_read_dict(Source, Json),
      forall(jsonld_tuple(Json, Tuple, Opts3), rdf_call_on_quad(Goal_4, Tuple))
  ;   % RDF/XML.
      Format == xml
  ->  process_rdf(Source, rdf_call_on_quads(Goal_4), Opts3)
  ;   % RDFa.
      Format == rdfa
  ->  read_rdfa(Source, Triples, Opts3),
      rdf_call_on_quads(Goal_4, Triples)
  ;   existence_error(rdf_format, [Format])
  ).

rdf_call_on_quad(Goal_4, rdf(S,P,O0,G0)) :- !,
  (rdf_is_term(O0) -> O = O0 ; rdf11:post_object(O, O0)),
  rdf11:post_graph(G, G0),
  call(Goal_4, S, P, O, G).
rdf_call_on_quad(Goal_4, rdf(S,P,O)) :-
  rdf_default_graph(G),
  rdf_call_on_quad(Goal_4, rdf(S,P,O,G)).

rdf_call_on_quads(Goal_4, Tuples) :-
  maplist(rdf_call_on_quad(Goal_4), Tuples).

rdf_call_on_quads(Goal_4, Tuples, _) :-
  rdf_call_on_quads(Goal_4, Tuples).



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

write_stream_to_file0(TmpFile, Opts, _, Source) :-
  write_stream_to_file(Source, TmpFile, Opts).



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
    rdf_call_on_tuples(Source, rdf_load_tuple(triples, quads), Opts),
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

% @tbd IRI normalization.
rdf_load_tuple(CT, CQ, S, P, O, G) :-
  (debugging(rdf(load)) -> rdf_print(S, P, O, G) ; true),
  rdf_assert(S, P, O, G),
  (rdf_default_graph(G) -> C = CT ; C = CQ),
  increment_thread_counter(C).



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
