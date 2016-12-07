:- module(
  rdf__io,
  [
    rdf_call_on_graph/2,         % +Source, :Goal_1
    rdf_call_on_graph/3,         % +Source, :Goal_1, +Opts
    rdf_call_on_stream/2,        % +Source, :Goal_3
    rdf_call_on_stream/3,        % +Source, :Goal_3, +Opts
    rdf_call_on_tuples/2,        % +Source, :Goal_5
    rdf_call_on_tuples/3,        % +Source, :Goal_5, +Opts
    rdf_call_on_tuples_stream/4, % +In, :Goal_5, +Path, +Opts
    rdf_call_onto_stream/3,      % +Source, +Sink, :Goal_4
    rdf_call_onto_stream/5,      % +Source, +Sink, :Goal_4, +SourceOpts, +SinkOpts
    rdf_call_to_graph/2,         % ?Sink, :Goal_1
    rdf_call_to_graph/3,         % ?Sink, :Goal_1, +Opts
    rdf_download_to_file/2,      % +Iri, +File
    rdf_download_to_file/4,      % +Iri, +File, +InOpts, +OutOpts
    rdf_graph_is_fresh/1,        % +G
    rdf_load_file/1,             % +Source
    rdf_load_file/2,             % +Source,           +Opts
    rdf_load_quads/2,            % +Source, -Quads
    rdf_load_quads/3,            % +Source, -Quads,   +Opts
    rdf_load_triples/2,          % +Source, -Triples
    rdf_load_triples/3,          % +Source, -Triples, +Opts
    rdf_media_type/3,            % ?MT, ?Iri, ?DefExt
    rdf_reserialize/2,           % +Source, +Sink
    rdf_reserialize/4,           % +Source, +Sink, +SourceOpts, +SinkOpts
    rdf_reserialize_legacy/2,    % +Source, +Sink
    rdf_reserialize_legacy/3,    % +Source, +Sink, +Opts
    rdf_write_to_sink/3,         % ?Sink, ?M,             ?G
    rdf_write_to_sink/4,         % ?Sink, ?M,             ?G, +Opts
    rdf_write_to_sink/5,         % ?Sink, ?M, ?S, ?P, ?O
    rdf_write_to_sink/6,         % ?Sink, ?M, ?S, ?P, ?O, ?G
    rdf_write_to_sink/7,         % ?Sink, ?M, ?S, ?P, ?O, ?G, +Opts
    rdf_write_to_sink_legacy/2,  % ?Sink, ?M
    rdf_write_to_sink_legacy/3   % ?Sink, ?M, +Opts
  ]
).

/** <module> RDF I/O

@author Wouter Beek
@version 2015/08-2016/02, 2016/04-2016/12
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug_ext)).
:- use_module(library(default)).
:- use_module(library(dict_ext)).
:- use_module(library(error)).
:- use_module(library(gen/gen_ntuples)).
:- use_module(library(http/http_ext)).
:- use_module(library(http/json)).
:- use_module(library(iostream)).
:- use_module(library(iri/iri_ext)).
:- use_module(library(jsonld/jsonld_read)).
:- use_module(library(lists)).
:- use_module(library(option_ext)).
:- use_module(library(os/file_ext)).
:- use_module(library(os/io)).
:- use_module(library(q/q_fs)).
:- use_module(library(q/q_io)).
:- use_module(library(q/q_prefix), []).
:- use_module(library(q/q_print)).
:- use_module(library(q/q_rdf)).
:- use_module(library(q/q_term)).
:- use_module(library(q/qb)).
:- use_module(library(rdf), [process_rdf/3]).
:- use_module(library(rdf/rdf_graph)).
:- use_module(library(rdf/rdf_guess)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdf_db), [rdf_save/2 as rdf_save_xmlrdf]).
:- use_module(library(semweb/rdf_ntriples), [rdf_process_ntriples/3]).
:- use_module(library(semweb/rdf_turtle_write)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfa), [read_rdfa/3]).
:- use_module(library(semweb/turtle), [rdf_process_turtle/3]).
:- use_module(library(typecheck)).
:- use_module(library(uri)).
:- use_module(library(uuid)).
:- use_module(library(yall)).
:- use_module(library(zlib)).

:- meta_predicate
    rdf_call_on_graph(+, 1),
    rdf_call_on_graph(+, 1, +),
    rdf_call_on_quad0(5, +, +),
    rdf_call_on_quads0(5, +, +),
    rdf_call_on_quads0(5, +, +, +),
    rdf_call_on_stream(+, 3),
    rdf_call_on_stream(+, 3, +),
    rdf_call_on_stream0(3, +, +, +, -),
    rdf_call_on_tuples(+, 5),
    rdf_call_on_tuples(+, 5, +),
    rdf_call_on_tuples_stream(+, 5, +, +),
    rdf_call_onto_stream(+, +, 4),
    rdf_call_onto_stream(+, +, 4, +, +),
    rdf_call_onto_stream0(4, +, +, +, -, +),
    rdf_call_to_graph(?, 1),
    rdf_call_to_graph(?, 1, +).

:- multifile
    error:has_type/2,
    q_io:q_cache_format_hook/2,
    q_io:q_cache2view_hook/2,
    q_io:q_store2cache_hook/4,
    q_io:q_source2store_hook/5,
    q_io:q_source_format_hook/2,
    q_io:q_view_graph_hook/3,
    q_io:q_view_rm_hook/2,
    rdf_http_plugin:rdf_content_type/3.

% RDF serialization format information is not backend-specific.

error:has_type(ntuples_media_type, MT) :-
  memberchk(MT, [
    application/'n-quads',
    application/'n-triples'
  ]).

error:has_type(rdf_media_type, MT) :-
  error:has_type(turtle_media_type, MT).
error:has_type(rdf_media_type, MT) :-
  error:has_type(rdfa_media_type, MT).
error:has_type(rdf_media_type, MT) :-
  memberchk(MT, [
    application/'ld+json',
    application/'rdf+xml'
  ]).

error:has_type(rdfa_media_type, MT) :-
  memberchk(MT, [application/'xhtml+xml',text/html]).

error:has_type(turtle_media_type, MT) :-
  error:has_type(ntuples_media_type, MT).
error:has_type(turtle_media_type, MT) :-
  memberchk(MT, [
    application/trig,
    application/turtle
  ]).

q_io:q_cache_format_hook(trp, [trp]).

q_io:q_cache2view_hook(trp, G) :-
  dcg_with_output_to(string(Msg), deb_q_io("TRP", G, "MEM")),
  q_file_graph(File, trp, G),
  indent_debug(io, Msg),
  rdf_load_db(File).

q_io:q_source2store_hook(rdf, Source, Sink, SourceOpts, SinkOpts) :-
  rdf_reserialize(Source, Sink, SourceOpts, SinkOpts).

q_io:q_source_format_hook(rdf, Ext) :-
  rdf_media_type(_, _, Ext).

q_io:q_store2cache_hook(trp, Source, Sink, G) :-
  dcg_with_output_to(string(Msg1), deb_q_io("N-Triples", G, "MEM")),
  dcg_with_output_to(string(Msg2), deb_q_io("MEM", G, "TRP")),
  indent_debug_call(io, Msg1,
    setup_call_cleanup(
      rdf_load_file(Source, [graph(G)]),
      indent_debug_call(io, Msg2,
        rdf_save_db(Sink, G)
      ),
      rdf_unload_graph(G)
    )
  ).

q_io:q_view_graph_hook(trp, G, false) :-
  rdf_graph(G).

q_io:q_view_rm_hook(trp, G) :-
  rdf_unload_graph(G),
  dcg_with_output_to(string(Msg), deb_q_io("TRP", G, "MEM")),
  indent_debug(io, Msg).

:- rdf_meta
   rdf_call_on_graph(+, :, t),
   rdf_call_on_tuples(+, :, t),
   rdf_graph_is_fresh(r),
   rdf_load_file(+, t),
   rdf_load_quads(+, -, t),
   rdf_load_triples(+, -, t),
   rdf_media_type(?, r, ?),
   rdf_write_to_sink(+, ?, r),
   rdf_write_to_sink(+, ?, r, +),
   rdf_write_to_sink(+, ?, r, r, o),
   rdf_write_to_sink(+, ?, r, r, o, r),
   rdf_write_to_sink(+, ?, r, r, o, r, +).

rdf_http_plugin:rdf_content_type('application/ld+json', 0.99, jsonld). %ABC





%! rdf_call_on_graph(+Source, :Goal_1) .
%! rdf_call_on_graph(+Source, :Goal_1, +Opts) .
%
% Loads Source into a graph and call Goal_1 on it.
%
% The following call is made: `call(:Goal_1, +G, +Path1, -Path2)`.
%
% Options are passed to rdf_load_file/2.

rdf_call_on_graph(Source, Goal_1) :-
  rdf_call_on_graph(Source, Goal_1, []).


rdf_call_on_graph(Source, Goal_1, Opts1) :-
  setup_call_cleanup(
    rdf_tmp_graph(G),
    (
      merge_options(Opts1, [force_graph(G)], Opts2),
      rdf_load_file(Source, Opts2),
      call(Goal_1, G)
    ),
    rdf_unload_graph(G)
  ).



%! rdf_call_on_stream(+Source, :Goal_3) is det.
%! rdf_call_on_stream(+Source, :Goal_3, +Opts) is det.
%
% The following call is made: `call(Goal_3, In, Path1, Path2)`.
%
% Options are passed to read_from_stream/3 and rdf_guess_fomat/3
%
% @throws existence_error If an HTTP request is performed and that
% returns an error code.

rdf_call_on_stream(Source, Goal_3) :-
  rdf_call_on_stream(Source, Goal_3, []).


rdf_call_on_stream(Source, Goal_3, Opts1) :-
  rdf_update_options(Opts1, Opts2),
  call_on_stream(Source, rdf_call_on_stream0(Goal_3, Opts2), Opts2).

rdf_call_on_stream0(Goal_3, Opts, In, Path1, Path3) :-
  set_rdf_media_type_and_encoding(In, Path1, Path2, Opts),
  call(Goal_3, In, Path2, Path3).



%! rdf_call_on_tuples(+Source, :Goal_5) is nondet.
%! rdf_call_on_tuples(+Source, :Goal_5, +Opts) is nondet.
%
% Probably the most intricate way in which anyone has even read in RDF
% statements...
%
% The following call is made: `call(:Goal_5, +M, +S, +P, +O, +G)`.
%
% Options are passed to:
%
%   * rdf_call_on_stream/3
%   * rdf_process_ntriples/3
%   * rdf_process_turtle/3,
%   * process_rdf/3
%   * read_rdfa/3.

rdf_call_on_tuples(Source, Goal_5) :-
  rdf_call_on_tuples(Source, Goal_5, []).


rdf_call_on_tuples(Source, Goal_5, Opts) :-
  rdf_call_on_stream(Source, rdf_call_on_tuples_stream0(Goal_5, Opts), Opts).


rdf_call_on_tuples_stream(In, Goal_5, Path, Opts) :-
  rdf_call_on_tuples_stream0(Goal_5, Opts, In, Path, Path).

rdf_call_on_tuples_stream0(Goal_5, Opts1, In, Path, Path) :-
  % Library Semweb uses option base_uri/1.  We use option base_iri/1
  % instead.
  get_base_iri(BaseIri, Path, Opts1),
  once(get_dicts(rdf_media_type, Path, MT)),
  Opts2 = [
    anon_prefix(node(_)),
    base(BaseIri),
    base_uri(BaseIri),
    max_errors(-1),
    rdf_media_type(MT),
    syntax(style)
  ],
  merge_options(Opts1, Opts2, Opts3),
  (   % N-Quads & N-Triples
      is_of_type(ntuples_media_type, MT)
  ->  rdf_process_ntriples(In, rdf_call_on_quads0(Goal_5, Path), Opts3)
  ;   % Trig & Turtle
      is_of_type(turtle_media_type, MT)
  ->  rdf_process_turtle(In, rdf_call_on_quads0(Goal_5, Path), Opts3)
  ;   % JSON-LD
      MT == application/'ld+json'
  ->  json_read_dict(In, Json),
      forall(
        jsonld_tuple(Json, Tuple, Opts3),
        rdf_call_on_quad0(Goal_5, Path, Tuple)
      )
  ;   % RDF/XML
      MT == application/'rdf+xml'
  ->  process_rdf(In, rdf_call_on_quads0(Goal_5, Path), Opts3)
  ;   % RDFa
      is_of_type(rdfa_media_type, MT)
  ->  read_rdfa(In, Triples, Opts3),
      rdf_call_on_quads0(Goal_5, Path, Triples)
  ).

rdf_call_on_quad0(Goal_5, Path, rdf(S,P,O1,G1)) :- !,
  rdf11:post_graph(G2, G1),
  (G2 == user -> rdf_default_graph(G3) ; G3 = G2),
  (   gen_is_term(O1)
  ->  call(Goal_5, Path, S, P, O1, G3)
  ;   q_legacy_literal(O1, D, Lex0, LTag1),
      (   rdf_equal(rdf:'HTML', D)
      ->  rdf11:write_xml_literal(html, Lex0, Lex1)
      ;   rdf_equal(rdf:'XMLLiteral', D)
      ->  rdf11:write_xml_literal(xml, Lex0, Lex1)
      ;   Lex1 = Lex0
      ),
      catch((
        rdf11:post_object(O2, O1),
        rdf11:pre_object(O2, O3),
        q_legacy_literal(O3, D, Lex3, LTag3)
      ), E, true),
      % Non-canonical lexical form.
      (   Lex1 \== Lex3
      ->  print_message(warning, non_canonical_lexical_form(D,Lex1))
      ;   true
      ),
      % Non-canonical language tag.
      (   ground(LTag1),
          LTag1 \== LTag3
      ->  print_message(warning, non_canonical_language_tag(LTag1))
      ;   true
      ),
      % Incorrect lexical form.
      (   var(E)
      ->  call(Goal_5, Path, S, P, O2, G3)
      ;   print_message(warning, E)
      )
  ).
rdf_call_on_quad0(Goal_5, Path, rdf(S,P,O)) :-
  rdf_default_graph(G),
  rdf_call_on_quad0(Goal_5, Path, rdf(S,P,O,G)).

% Use this to debug bugs in statement calls.
rdf_call_on_quad0_debug(Goal_5, Path, Tuple) :-
  %flag(rdf_call_on_quad0_debug, N, N + 1),
  %format(user_output, "~D~n", [N]),
  %(N =:= 715769 -> gtrace ; true),
  catch(rdf_call_on_quad0(Goal_5, Path, Tuple), E, true),
  (var(E) -> true ; gtrace, rdf_call_on_quad0_debug(Goal_5, Path, Tuple)).

rdf_call_on_quads0(Goal_5, Path, Tuples) :-
  maplist(rdf_call_on_quad0(Goal_5, Path), Tuples).

rdf_call_on_quads0(Goal_5, Path, Tuples, _) :-
  rdf_call_on_quads0(Goal_5, Path, Tuples).



%! rdf_call_onto_stream(+Source, +Sink, :Goal_4) is det.
%! rdf_call_onto_stream(+Source, +Sink, :Goal_4, +SourceOpts, +SinkOpts) is det.

rdf_call_onto_stream(Source, Sink, Goal_4) :-
  rdf_call_onto_stream(Source, Sink, Goal_4, [], []).


rdf_call_onto_stream(Source, Sink, Goal_4, SourceOpts, SinkOpts) :-
  rdf_update_options(SourceOpts, RdfOpts),
  call_onto_stream(
    Source,
    Sink,
    rdf_call_onto_stream0(Goal_4, RdfOpts),
    SourceOpts,
    SinkOpts
  ).

rdf_call_onto_stream0(Goal_4, Opts, In, Path1, Path3, Out) :-
  set_rdf_media_type_and_encoding(In, Path1, Path2, Opts),
  call(Goal_4, In, Path2, Path3, Out).



%! rdf_call_to_graph(?Sink, :Goal_1) is det.
%! rdf_call_to_graph(?Sink, :Goal_1, +Opts) is det.
%
% Writes results of Goal_1, as asserted into a single graph, to Sink.
% The following call is made: `call(Goal_1, G)`.
%
% Options are passed to rdf_write_to_sink/7.

rdf_call_to_graph(Sink, Goal_1) :-
  rdf_call_to_graph(Sink, Goal_1, []).


rdf_call_to_graph(Sink, Goal_1, Opts) :-
  setup_call_cleanup(
    rdf_tmp_graph(G),
    (
      call(Goal_1, G),
      rdf_write_to_sink(Sink, trp, _, _, _, G, Opts)
    ),
    rdf_unload_graph(G)
  ).



%! rdf_reserialize(+Source, +Sink) is det.
%! rdf_reserialize(+Source, +Sink, +SourceOpts, +SinkOpts) is det.
%
% The following options are supported:
%
%   - rdf_media_type(+rdf_media_type)
%
%     The old RDF Media Type (in SourceOpts) and the new RDF Media
%     Type (in SinkOpts).
%
%   - Other options are passed to:
%
%     - rdf_call_on_tuples/3
%
%     - rdf_write_to_sink/7

rdf_reserialize(Source, Sink) :-
  rdf_reserialize(Source, Sink, [], []).


rdf_reserialize(Source, Sink, SourceOpts, SinkOpts) :-
  call_to_ntuples(Sink, rdf_change_media_type0(Source, SourceOpts), SinkOpts).

rdf_change_media_type0(Source, SourceOpts, State, Out) :-
  indent_debug(io, "» RDF → RDF"),
  rdf_call_on_tuples(
    Source,
    {State,Out}/[_,S,P,O,G]>>gen_ntuple(S, P, O, G, State, Out),
    SourceOpts
  ),
  indent_debug(io, "« RDF → RDF").



%! rdf_reserialize_legacy(+Source, +Sink) is det.
%! rdf_reserialize_legacy(+Source, +Sink, +Opts) is det.

rdf_reserialize_legacy(Source, Sink) :-
  rdf_reserialize_legacy(Source, Sink, []).


rdf_reserialize_legacy(Source, Sink, Opts) :-
  rdf_load_file(Source),
  rdf_write_to_sink_legacy(Sink, trp, Opts).



%! rdf_download_to_file(+Iri, +File) is det.
%! rdf_download_to_file(+Iri, ?File, +InOpts, +OutOpts) is det.
%
% Options are passed to rdf_call_onto_stream/5.

rdf_download_to_file(Iri, File) :-
  rdf_download_to_file(Iri, File, [], []).


rdf_download_to_file(Iri, File, InOpts, OutOpts) :-
  thread_file(File, TmpFile),
  call_onto_stream(Iri, TmpFile, copy_stream_data0, InOpts, OutOpts),
  rename_file(TmpFile, File).

copy_stream_data0(In, Path, Path, Out) :-
  copy_stream_data(In, Out).



%! rdf_graph_is_fresh(+G) is semidet.

rdf_graph_is_fresh(G) :-
  rdf_graph_property(G, source_last_modified(Time1)),
  rdf_graph_property(G, source(Iri)),
  uri_file_name(Iri, File),
  time_file(File, Time2),
  Time1 >= Time2.



%! rdf_load_file(+Source) is det.
%! rdf_load_file(+Source, +Opts) is det.
%
% The following options are supported:
%
%   * base_iri(+atom)
%
%   * force_graph(+rdf_graph) Forces all tuples to be loaded as
%   triples in this graph.
%
%   * graph(+rdf_graph) The default graph.  Default is `default'.
%
%   * quads(-nonneg)
%
%   * triples(-nonneg)
%
%   * tuples(-nonneg)
%
%   * Other options are passed to rdf_call_on_tuples/3.
%
% The following code cannot be used because we are not sure whether
% RDF data is loaded into _one_ graph:
%
% ```prolog
% % (iii) The file was not modified after the graph was loaded.
% rdf_graph_property(G, source_last_modified(LMod)),
% exists_file(File),
% time_file(File, LMod), !.
% ```

rdf_load_file(Source) :-
  rdf_load_file(Source, []).


rdf_load_file(Source, Opts) :-
  State = _{quads: 0, triples: 0},
  (   option(force_graph(ToG), Opts)
  ->  Goal_5 = rdf_force_load_tuple0(State, ToG)
  ;   rdf_default_graph(DefG),
      option(graph(ToG), Opts, DefG),
      Goal_5 = rdf_load_tuple0(State, ToG)
  ),
  rdf_call_on_tuples(Source, Goal_5, Opts),
  NumQuads = State.quads,
  NumTriples = State.triples,
  option(quads(NumQuads), Opts, _),
  option(triples(NumTriples), Opts, _),
  NumTuples is NumQuads + NumTriples,
  option(tuples(NumTuples), Opts, _),
  debug(
    rdf__io,
    "Loaded ~D tuples from ~w (~D triples and ~D quads).~n",
    [NumTuples,Source,State.triples,State.quads]
  ).

rdf_force_load_tuple0(State, ToG, _, S, P, O, FromG) :-
  count_tuple0(State, FromG),
  % @tbd IRI normalization.
  rdf_assert(S, P, O, ToG).

rdf_load_tuple0(State, ToG, _, S, P, O, FromG) :-
  count_tuple0(State, FromG),
  (rdf_default_graph(FromG) -> G = ToG ; G = FromG),
  % @tbd IRI normalization.
  rdf_assert(S, P, O, G).

count_tuple0(State, G) :-
  rdf_default_graph(G), !,
  dict_inc(triples, State).
count_tuple0(State, _) :-
  dict_inc(quads, State).



%! rdf_load_quads(+Source, -Quads) is det.
%! rdf_load_quads(+Source, -Quads, +Opts) is det.
%
% Options are passed to rdf_load_file/2.

rdf_load_quads(Source, Quads) :-
  rdf_load_quads(Source, Quads, []).


rdf_load_quads(Source, Quads, Opts) :-
  q_snap((
    rdf_retractall(_, _, _),
    rdf_load_file(Source, Opts),
    q_quads(trp, Quads),
    rdf_retractall(_, _, _),
    rdf_unload_empty_graphs
  )).



%! rdf_load_triples(+Source, -Triples) is det.
%! rdf_load_triples(+Source, -Triples, +Opts) is det.
%
% Options are passed to rdf_load_file/2.

rdf_load_triples(Source, Triples) :-
  rdf_load_triples(Source, Triples, []).


rdf_load_triples(Source, Triples, Opts) :-
  q_snap((
    rdf_retractall(_, _, _),
    rdf_load_file(Source, Opts),
    q_triples(trp, Triples)
  )).



%! rdf_media_type(?MT, ?Iri, ?DefExt) is nondet.
%
% @see https://www.w3.org/ns/formats/

rdf_media_type(application/'ld+json', formats:'JSON-LD', jsonld).
rdf_media_type(application/'n-quads', formats:'N-Quads', nq).
rdf_media_type(application/'n-triples', formats:'N-Triples', nt).
rdf_media_type(application/'rdf+xml', formats:'RDF_XML', rdf).
rdf_media_type(application/turtle, formats:'Turtle', ttl).
rdf_media_type(application/trig, formats:'TriG', trig).
rdf_media_type(application/'xhtml+xml', formats:'RDFa', xhtml).
rdf_media_type(text/html, formats:'RDFa', html).
%rdf_media_type(json, formats:'RDF_JSON').
%rdf_media_type(ldpatch, formats:'LD_Patch').
%rdf_media_type(micro, formats:'microdata').
%rdf_media_type(n3, formats:'N3').
%rdf_media_type(owlx, formats:'OWL_XML').
%rdf_media_type(owlf, formats:'OWL_Functional').
%rdf_media_type(owlm, formats:'OWL_Manchester').
%rdf_media_type(powder, formats:'POWDER').
%rdf_media_type(powders, formats:'POWDER-S').
%rdf_media_type(provn, formats:'PROV-N').
%rdf_media_type(provx, formats:'PROV-XML').
%rdf_media_type(rif, formats:'RIF_XML').
%rdf_media_type(sparqlx, formats:'SPARQL_Results_XML').
%rdf_media_type(sparqlj, formats:'SPARQL_Results_JSON').
%rdf_media_type(sparqlc, formats:'SPARQL_Results_CSV').
%rdf_media_type(sparqlt, formats:'SPARQL_Results_TSV').



%! rdf_write_to_sink(?Sink, ?M,             ?G       ) is det.
%! rdf_write_to_sink(?Sink, ?M,             ?G, +Opts) is det.
%! rdf_write_to_sink(?Sink, ?M, ?S, ?P, ?O           ) is det.
%! rdf_write_to_sink(?Sink, ?M, ?S, ?P, ?O, ?G       ) is det.
%! rdf_write_to_sink(?Sink, ?M, ?S, ?P, ?O, ?G, +Opts) is det.
%
% Writes N-Tuples from backend M in an RDF serialization to Sink.
%
% If no Sink is given the file from which graph G, if instantiated,
% was loaded is used.
%
% @tbd Check whether HDT file already exists.
%
% In line with module `io`, the following call is made:
% `call(Goal_3, +Out, +Path1, -Path2)`.
%
% The following options are supported:
%
%   * `rdf_media_type(+rdf_media_type)`
%
% The following RDF Media Types are supported:
%
%   * application/'n-quads' (N-Quads)
%
%   * application/'n-triples' (N-Triples)
%
% For other RDF Media Types rdf_write_to_sink_legacy/[2,3] should be
% used.

rdf_write_to_sink(Sink, M, G) :-
  rdf_write_to_sink(Sink, M, G, []).


rdf_write_to_sink(Sink, M, G, Opts) :-
   rdf_write_to_sink(Sink, M, _, _, _, G, Opts).


rdf_write_to_sink(Sink, M, S, P, O) :-
  rdf_write_to_sink(Sink, M, S, P, O, _).


rdf_write_to_sink(Sink, M, S, P, O, G) :-
  rdf_write_to_sink(Sink, M, S, P, O, G, []).


rdf_write_to_sink(File, M, S, P, O, G, Opts) :-
  var(File), !,
  (   % A file is already associated with the given graph G.
      rdf_graph_property(G, source(File))
  ->  true
  ;   rdf_file_name0(File, Opts)
  ),
  create_file_directory(File),
  rdf_write_to_sink(File, M, S, P, O, G, Opts).
rdf_write_to_sink(Sink, M, S, P, O, G, Opts1) :-
  rdf_default_graph(DefG),
  defval(DefG, G),
  rdf_write_media_type0(Sink, Opts1, MT),
  merge_options(Opts1, [rdf_media_type(MT)], Opts2),
  dcg_with_output_to(string(Msg), (
    "TRP → ",
    dcg_q_print_graph_term(G),
    " → ",
    atom(MT)
  )),
  indent_debug_call(io, Msg,
    call_to_ntuples(Sink, gen_ntuples(M, S, P, O, G), Opts2)
  ).

rdf_file_name0(File, Opts) :-
  uuid(Base),
  rdf_file_name0(Base, File, Opts).

rdf_file_name0(Base, File, Opts) :-
  rdf_write_media_type0(_, Opts, MT),
  rdf_media_type(MT, _, Ext),
  file_name_extension(Base, Ext, File).

rdf_write_media_type0(_, Opts, MT) :-
  option(rdf_media_type(MT), Opts), !.
rdf_write_media_type0(File, _, MT) :-
  is_absolute_file_name(File),
  file_name_extension(_, Ext, File),
  rdf_media_type(MT, _, Ext), !.
rdf_write_media_type0(_, _, application/'n-quads').



%! rdf_write_to_sink_legacy(?Sink, ?M) is det.
%! rdf_write_to_sink_legacy(?Sink, ?M, +Opts) is det.
%
% Legacy RDF writers that can only dump everything or a given graph to
% a file (i.e., no triple pattern support).
%
% The following RDF Media Types are supported:
%
%   - application/'n-quads' (N-Quads)
%
%   - application/'n-triples' (N-Triples)
%
%   - application/trig (TRiG)
%
%   - application/turtle (Turtle)
%
%   - application/rdf+xml (RDF/XML)
%
% Other options are passed to call_to_stream/3.

rdf_write_to_sink_legacy(Sink, M) :-
  rdf_write_to_sink_legacy(Sink, M, []).


rdf_write_to_sink_legacy(Sink, M, Opts) :-
  rdf_write_media_type0(Sink, Opts, MT),
  (   is_of_type(rdf_ntuples, MT)
  ->  rdf_write_to_sink(Sink, M, Opts)
  ;   MT == application/trig
  ->  call_to_stream(Sink, {Opts}/[Out]>>rdf_save_trig(Out, Opts), Opts)
  ;   MT == application/turtle
  ->  merge_options(
        [
          a(true),
          align_prefixes(true),
          comment(false),
          group(true),
          indent(4),
          only_known_prefixes(true),
          subject_white_lines(1),
          tab_distance(0),
          user_prefixes(true)
        ],
        Opts,
        TurtleOpts
      ),
      call_to_stream(
	Sink,
	{TurtleOpts}/[Out]>>rdf_save_turtle(Out, TurtleOpts),
        Opts
      )
  ;   MT == application/'rdf+xml'
  ->  call_to_stream(Sink, {Opts}/[Out]>>rdf_save_xmlrdf(Out, Opts), Opts)
  ;   domain_error(rdf_media_type, MT)
  ).





% HELPERS %

%! get_base_iri(-BaseIri, +Path, +Opts) is det.

% Option base_iri/1 overrides everything else.
get_base_iri(BaseIri, _, Opts) :-
  option(base_iri(BaseIri), Opts), !.
get_base_iri(BaseIri, Ds, _) :-
  member(D, Ds),
  (   get_dict(iri, D, Iri)
  ->  iri_remove_fragment(Iri, BaseIri)
  ;   get_dict(file, D, File)
  ->  uri_file_name(BaseIri, File)
  ), !.



%! set_rdf_media_type_and_encoding(+In, +Path1, -Path2, +Opts) is det.
%
% # GUESS ENCODING
%
%   1. Text?
%   2. Unicode?
%   3. Format?
%
% @tbd Archive entries are always encoded as octet.  We change this to
%      UTF-8.

% Option rdf_media_type/1 overrides everything else.
set_rdf_media_type_and_encoding(_, [H1|T], [H2|T], Opts) :-
  option(rdf_media_type(MT), Opts), !,
  put_dict(rdf_media_type, H1, MT, H2).
set_rdf_media_type_and_encoding(_, [H|T], [H|T], _) :-
  dict_has_key(rdf_media_type, H), !.
set_rdf_media_type_and_encoding(In, [H1|T], [H2|T], Opts) :-
  (   get_base_iri(BaseIri, [H1|T], Opts),
      iri_file_extensions(BaseIri, Exts),
      member(Ext, Exts),
      rdf_media_type(MT, _, Ext)
  ->  GuessOpts = [default_rdf_media_type(MT)]
  ;   GuessOpts = []
  ),
  % Notice that the metadata option of the original options list does
  % not get overwritten when opening the stream for guessing the RDF
  % serialization format.
  rdf_guess_media_type(In, MT, GuessOpts),
  % JSON-LD _must_ be encoded in UTF-8.
  % @tbd Turtle-family formats as well?
  (MT == application/'ld+json' -> set_stream(In, encoding(utf8)) ; true),
  put_dict(rdf_media_type, H1, MT, H2).



%! rdf_update_options(+Opts1, -Opts2) is det.

rdf_update_options(Opts1, Opts2) :-
  rdf_http_plugin:rdf_extra_headers(DefOpts, Opts1),
  merge_options(DefOpts, Opts1, Opts2).
