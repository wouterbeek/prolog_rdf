:- module(
  rdfio,
  [
    rdf_call_on_graph/2,        % +Source, :Goal_3
    rdf_call_on_graph/3,        % +Source, :Goal_3,      +Opts
    rdf_call_on_stream/2,       % +Source, :Goal_3       
    rdf_call_on_stream/3,       % +Source, :Goal_3,      +Opts
    rdf_call_on_tuples/2,       % +Source, :Goal_5       
    rdf_call_on_tuples/3,       % +Source, :Goal_5,      +Opts
    rdf_call_to_graph/2,        % +Sink,   :Goal_1       
    rdf_call_to_graph/3,        % +Sink,   :Goal_1,      +Opts
    rdf_change_format/2,        % +Source, -Sink
    rdf_change_format/3,        % +Source, -Sink,        +Opts
    rdf_change_format_legacy/2, % +Source, -Sink
    rdf_change_format_legacy/3, % +Source, -Sink,        +Opts
    rdf_download_to_file/2,     % +Iri,    +File         
    rdf_download_to_file/3,     % +Iri,    +File,        +Opts
    rdf_load_file/1,            % +Source                
    rdf_load_file/2,            % +Source,               +Opts
    rdf_load_tuples/2,          % +Source, -Triples
    rdf_load_tuples/3,          % +Source, -Triples,     +Opts
    rdf_write_to_sink/1,        % +Sink                  
    rdf_write_to_sink/2,        % +Sink,             ?G
    rdf_write_to_sink/3,        % +Sink,             ?G, +Opts
    rdf_write_to_sink/4,        % +Sink, ?S, ?P, ?O
    rdf_write_to_sink/5,        % +Sink, ?S, ?P, ?O, ?G
    rdf_write_to_sink/6,        % +Sink, ?S, ?P, ?O, ?G, +Opts
    rdf_write_to_sink_legacy/1, % +Sink
    rdf_write_to_sink_legacy/2  % +Sink,                 +Opts
  ]
).

/** <module> RDF input/output

This module is not named `rdf_io` since a module with that name is
already part of ClioPatria.

@author Wouter Beek
@version 2015/08-2016/02, 2016/04-2016/06
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug)).
:- use_module(library(debug_ext)).
:- use_module(library(default)).
:- use_module(library(dict_ext)).
:- use_module(library(error)).
:- use_module(library(gen/gen_ntuples)).
:- use_module(library(http/http_ext)).
:- use_module(library(http/json)).
:- use_module(library(iostream)).
:- use_module(library(iri/iri_ext)).
:- use_module(library(jsonld/jsonld_metadata)).
:- use_module(library(jsonld/jsonld_read)).
:- use_module(library(lists)).
:- use_module(library(option_ext)).
:- use_module(library(os/file_ext)).
:- use_module(library(os/io_ext)).
:- use_module(library(os/open_any2)).
:- use_module(library(rdf), [process_rdf/3]).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(rdf/rdf_file)).
:- use_module(library(rdf/rdf_graph)).
:- use_module(library(rdf/rdf_guess)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfa), [read_rdfa/3]).
:- use_module(library(semweb/rdf_db), [rdf_save/2 as rdf_save_xmlrdf]).
:- use_module(library(semweb/rdf_ntriples), [rdf_process_ntriples/3]).
:- use_module(library(semweb/rdf_turtle_write)).
:- use_module(library(semweb/turtle), [rdf_process_turtle/3]).
:- use_module(library(typecheck)).
:- use_module(library(uri)).
:- use_module(library(uuid_ext)).
:- use_module(library(yall)).
:- use_module(library(zlib)).
:- use_module(library(z/z_ext)).
:- use_module(library(z/z_stmt)).
:- use_module(library(z/z_term)).

:- meta_predicate
    rdf_call_on_graph(+, 3),
    rdf_call_on_graph(+, 3, +),
    rdf_call_on_quad0(5, +, +),
    rdf_call_on_quads0(5, +, +),
    rdf_call_on_quads0(5, +, +, +),
    rdf_call_on_stream(+, 3),
    rdf_call_on_stream(+, 3, +),
    rdf_call_on_stream0(3, +, +, +, -),
    rdf_call_on_tuples(+, 5),
    rdf_call_on_tuples(+, 5, +),
    rdf_call_on_tuples0(5, +, +, +, -),
    rdf_call_to_graph(+, 1),
    rdf_call_to_graph(+, 1, +).

:- rdf_meta
   rdf_call_on_graph(+, :, t),
   rdf_call_on_tuples(+, :, t),
   rdf_download_to_file(+, +, t),
   rdf_load_file(+, t),
   rdf_load_tuples(+, -, t),
   rdf_write_to_sink(+, r),
   rdf_write_to_sink(+, r, +),
   rdf_write_to_sink(+, r, r, o),
   rdf_write_to_sink(+, r, r, o, r),
   rdf_write_to_sink(+, r, r, o, r, +).

:- debug(rdf(io)).





%! rdf_call_on_graph(+Source, :Goal_3) .
%! rdf_call_on_graph(+Source, :Goal_3, +Opts) .
%
% Loads Source into a graph and call Goal_3 on it.
%
% The following call is made: `call(:Goal_3, +G, +M1, -M2)`.
%
% Options are passed to rdf_load_file/2.

rdf_call_on_graph(Source, Goal_3) :-
  rdf_call_on_graph(Source, Goal_3, []).


rdf_call_on_graph(Source, Goal_3, Opts1) :-
  remove_option(Opts1, metadata(M2), Opts2),
  setup_call_cleanup(
    rdf_tmp_graph(G),
    (
      merge_options([force_graph(G),metadata(M1)], Opts2, Opts3),
      rdf_load_file(Source, Opts3),
      call(Goal_3, G, M1, M2)
    ),
    rdf_unload_graph(G)
  ).



%! rdf_call_on_stream(+Source, :Goal_3) is det.
%! rdf_call_on_stream(+Source, :Goal_3, +Opts) is det.
%
% The following call is made: `call(Goal_3, In, M1, M2)`.
%
% Options are passed to read_from_stream/3 and rdf_guess_fomat/3
%
% @throws existence_error If an HTTP request is performed and that
% returns an error code.

rdf_call_on_stream(Source, Goal_3) :-
  rdf_call_on_stream(Source, Goal_3, []).


rdf_call_on_stream(Source, Goal_3, Opts1) :-
  % Accept headers for RDF requests are specified in
  % `library(semweb/rdf_http_plugin))'.
  rdf_http_plugin:rdf_extra_headers(DefRdfOpts, Opts1),
  merge_options(DefRdfOpts, Opts1, Opts2),
  call_on_stream(Source, rdf_call_on_stream0(Goal_3, Opts2), Opts2).


rdf_call_on_stream0(Goal_3, Opts, In, M1, M3) :-
  (   option(rdf_format(Format1), Opts),
      ground(Format1)
  ->  true
  ;   rdf_guess_format_options0(M1, Opts, GuessOpts),
      % @note Make sure the metadata option of the RDF source does not get
      % overwritten when opening the stream for guessing the RDF serialization
      % format.
      rdf_guess_format(In, Format1, GuessOpts)
  ->  true
  ;   Format1 = unrecognized
  ),
  % JSON-LD _must_ be encoded in UTF-8.
  (Format1 == jsonld -> set_stream(In, encoding(utf8)) ; true),
  (rdf_format_iri(Format1, Format2) -> true ; domain_error(rdf_format, Format1)),
  jsonld_metadata_abbreviate_iri(Format2, Format3),
  M2 = M1.put(_{'llo:rdf_format': Format3}),
  call(Goal_3, In, M2, M3).


rdf_guess_format_options0(M, Opts1, Opts2) :-
  iri_file_extensions(M.'llo:base_iri', Exts1),
  reverse(Exts1, Exts2),
  member(Ext, Exts2),
  rdf_file_extension(Ext, Format), !,
  merge_options(Opts1, [default_rdf_format(Format)], Opts2).
rdf_guess_format_options0(_, Opts, Opts).



%! rdf_call_on_tuples(+Source, :Goal_5) is nondet.
%! rdf_call_on_tuples(+Source, :Goal_5, +Opts) is nondet.
%
% Probably the most intricate way in which anyone has even read in RDF
% statements...
%
% The following call is made: `call(:Goal_5, +M, +S, +P, +O, +G)`.
%
% Options are passed to:
%   * rdf_call_on_stream/3
%   * rdf_process_ntriples/3
%   * rdf_process_turtle/3,
%   * process_rdf/3
%   * read_rdfa/3.

rdf_call_on_tuples(Source, Goal_5) :-
  rdf_call_on_tuples(Source, Goal_5, []).


rdf_call_on_tuples(Source, Goal_5, Opts) :-
  rdf_call_on_stream(Source, rdf_call_on_tuples0(Goal_5, Opts), Opts).


rdf_call_on_tuples0(Goal_5, Opts1, In, M, M) :-
  % Library Semweb uses option base_uri/1.  We use option base_iri/1 instead.
  get_dict('llo:base_iri', M, BaseIri),
  jsonld_metadata_expand_iri(M.'llo:rdf_format', FormatIri),
  rdf_format_iri(Format, FormatIri),
  %uuid_no_hyphen(Uuid),
  %atomic_list_concat(['_',Uuid,''], :, BPrefix),
  Opts2 = [
    %anon_prefix(BPrefix),
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
  ->  rdf_process_ntriples(In, rdf_call_on_quads0(Goal_5, M), Opts3)
  ;   % Trig & Turtle.
      memberchk(Format, [trig,turtle])
  ->  rdf_process_turtle(In, rdf_call_on_quads0(Goal_5, M), Opts3)
  %;   % JSON-LD.
  %    Format == jsonld
  %->  json_read_dict(In, Json),
  %    forall(jsonld_tuple(Json, Tuple, Opts3),
  %      rdf_call_on_quad0(Goal_5, M, Tuple)
  %    )
  ;   % RDF/XML.
      Format == xml
  ->  process_rdf(In, rdf_call_on_quads0(Goal_5, M), Opts3)
  ;   % RDFa.
      Format == rdfa
  ->  read_rdfa(In, Triples, Opts3),
      rdf_call_on_quads0(Goal_5, M, Triples)
  ;   domain_error(rdf_format, Format)
  ).


rdf_call_on_quad0(Goal_5, M, rdf(S,P,O1,G1)) :- !,
  rdf11:post_graph(G2, G1),
  (G2 == user -> rdf_default_graph(G3) ; G3 = G2),
  (   rdf_is_term(O1)
  ->  call(Goal_5, M, S, P, O1, G3)
  ;   z_legacy_literal(O1, D, Lex0, LTag1),
      (   rdf_equal(rdf:'HTML', D)
      ->  rdf11:write_xml_literal(html, Lex0, Lex1)
      ;   rdf_equal(rdf:'XMLLiteral', D)
      ->  rdf11:write_xml_literal(xml, Lex0, Lex1)
      ;   Lex1 = Lex0
      ),
      catch((
        rdf11:post_object(O2, O1),
        rdf11:pre_object(O2, O3),
        z_legacy_literal(O3, D, Lex3, LTag3)
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
      ->  call(Goal_5, M, S, P, O2, G3)
      ;   print_message(warning, E)
      )
  ).
rdf_call_on_quad0(Goal_5, M, rdf(S,P,O)) :-
  rdf_default_graph(G),
  rdf_call_on_quad0(Goal_5, M, rdf(S,P,O,G)).


rdf_call_on_quads0(Goal_5, M, Tuples) :-
  maplist(rdf_call_on_quad0(Goal_5, M), Tuples).


rdf_call_on_quads0(Goal_5, M, Tuples, _) :-
  rdf_call_on_quads0(Goal_5, M, Tuples).



%! rdf_call_to_graph(+Sink, :Goal_1) is det.
%! rdf_call_to_graph(+Sink, :Goal_1, +Opts) is det.
%
% Writes results of Goal_1, as asserted into a single graph, to Sink.
% The following call is made: `call(Goal_1, G)`.
%
% Options are passed to rdf_write_to_sink/6.

rdf_call_to_graph(Sink, Goal_1) :-
  rdf_call_to_graph(Sink, Goal_1, []).


rdf_call_to_graph(Sink, Goal_1, Opts) :-
  setup_call_cleanup(
    rdf_tmp_graph(G),
    (
      call(Goal_1, G),
      rdf_write_to_sink(Sink, _, _, _, G, Opts)
    ),
    rdf_unload_graph(G)
  ).



%! rdf_change_format(+Source, +Sink) is det.
%! rdf_change_format(+Source, +Sink, +Opts) is det.

rdf_change_format(Source, Sink) :-
  rdf_change_format(Source, Sink, []).


rdf_change_format(Source, Sink, Opts) :-
  rdf_call_on_tuples(
    Source,
    {Sink,Opts}/[_M,S,P,O,G]>>rdf_write_to_sink(Sink, S, P, O, G, Opts)
  ).



%! rdf_change_format_legacy(+Source, +Sink) is det.
%! rdf_change_format_legacy(+Source, +Sink, +Opts) is det.

rdf_change_format_legacy(Source, Sink) :-
  rdf_change_format_legacy(Source, Sink, []).


rdf_change_format_legacy(Source, Sink, Opts) :-
  rdf_load_file(Source),
  rdf_write_to_sink_legacy(Sink, Opts).



%! rdf_download_to_file(+Iri, +File) is det.
%! rdf_download_to_file(+Iri, ?File, +Opts) is det.
% Options are passed to rdf_call_on_stream/4 and write_stream_to_file/3.

rdf_download_to_file(Iri, File) :-
  rdf_download_to_file(Iri, File, []).

rdf_download_to_file(Iri, File, Opts) :-
  thread_file(File, TmpFile),
  rdf_call_on_stream(
    Iri,
    {TmpFile,Opts}/[In,M,M]>>write_stream_to_file(In, TmpFile, Opts),
    Opts
  ),
  rename_file(TmpFile, File).



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
%  ```prolog
%  % (iii) The file was not modified after the graph was loaded.
%  rdf_graph_property(G, source_last_modified(LMod)),
%  exists_file(File),
%  time_file(File, LMod), !.
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
  option(quads(State.quads), Opts, _),
  option(triples(State.triples), Opts, _),
  NumTuples is State.triples + State.quads,
  option(tuples(NumTuples), Opts, _),
  debug(
    rdf(stream),
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



%! rdf_load_tuples(+Source, -Triples) is det.
%! rdf_load_tuples(+Source, -Triples, +Opts) is det.
%
% Options are passed to rdf_load_file/2.

rdf_load_tuples(Source, Triples) :-
  rdf_load_tuples(Source, Triples, []).


rdf_load_tuples(Source, Triples, Opts) :-
  rdf_snap((
    rdf_retractall(_, _, _),
    rdf_load_file(Source, Opts),
    z_triples(Triples)
  )).



%! rdf_write_to_sink(+Sink                       ) is det.
%! rdf_write_to_sink(+Sink,             ?G       ) is det.
%! rdf_write_to_sink(+Sink,             ?G, +Opts) is det.
%! rdf_write_to_sink(+Sink, ?S, ?P, ?O           ) is det.
%! rdf_write_to_sink(+Sink, ?S, ?P, ?O, ?G       ) is det.
%! rdf_write_to_sink(+Sink, ?S, ?P, ?O, ?G, +Opts) is det.
%
% Writes output to an RDF stream.
%
% File sinks are treated specially.
%
% The following call is made: `call(Goal_3, Out, M1, M2)`.
%
% Options are passed to call_to_stream/3 and ...
%
% The following formats are supported:
%
%   * `nquads` (N-Quads)
%
%   * `ntriples` (N-Triples)

rdf_write_to_sink(Sink) :-
  rdf_write_to_sink(Sink, _).


rdf_write_to_sink(Sink, G) :-
  rdf_write_to_sink(Sink, G, []).


rdf_write_to_sink(Sink, G, Opts) :-
   rdf_write_to_sink(Sink, _, _, _, G, Opts).


rdf_write_to_sink(Sink, S, P, O) :-
  rdf_write_to_sink(Sink, S, P, O, _).


rdf_write_to_sink(Sink, S, P, O, G) :-
  rdf_write_to_sink(Sink, S, P, O, G, []).


% We have to come up with a good file name.
rdf_write_to_sink(File, S, P, O, G, Opts) :-
  var(File), !,
  (   % A file is already associated with the given graph G.
      rdf_graph_property(G, source(File))
  ->  true
  ;   rdf_file_name0(File, Opts)
  ),
  create_file_directory(File),
  rdf_write_to_sink(File, S, P, O, G, Opts).
rdf_write_to_sink(Sink, S, P, O, G, Opts) :-
  rdf_default_graph(DefG),
  defval(DefG, G),
  rdf_write_format0(Sink, Opts, Format),
  call_to_stream(Sink, rdf_write_to_sink0(S, P, O, G, Format), Opts),
  debug(rdf(io), "RDF was written to sink ~w.", [Sink]).


rdf_file_name0(File, Opts) :-
  uuid_no_hyphen(Base),
  rdf_file_name0(Base, File, Opts).

rdf_file_name0(Base, File, Opts) :-
  rdf_write_format0(_, Opts, Format),
  rdf_file_extension(Ext, Format),
  file_name_extension(Base, Ext, File).


rdf_write_format0(_, Opts, Format) :-
  option(rdf_format(Format), Opts), !.
rdf_write_format0(File, _, Format) :-
  is_absolute_file_name(File),
  file_name_extension(_, Ext, File),
  rdf_file_extension(Ext, Format), !.
rdf_write_format0(_, _, nquads).


rdf_write_to_sink0(S, P, O, G, nquads, Out, M, M) :- !,
  with_output_to(Out, gen_nquads(S, P, O, G)).
rdf_write_to_sink0(S, P, O, G, ntriples, Out, M, M) :- !,
  with_output_to(Out, gen_ntriples(S, P, O, G)).
rdf_write_to_sink0(_, _, _, _, Format, _, M, M) :-
  domain_error(rdf_format, Format).



%! rdf_write_to_sink_legacy(+Sink) is det.
%! rdf_write_to_sink_legacy(+Sink, +Opts) is det.
%
% Legacy RDF writers that can only dump everything or a given graph to
% a file (no triple pattern support).
%
% The following formats are supported:
%
%   * `nquads` (N-Quads)
%
%   * `ntriples` (N-Triples)
%
%   * `trig` (TRiG)
%
%   * `turtle` (Turtle)
%
%   * `xml` (RDF/XML)

rdf_write_to_sink_legacy(Sink) :-
  rdf_write_to_sink_legacy(Sink, []).


rdf_write_to_sink_legacy(Sink, Opts) :-
  rdf_write_format0(Sink, Opts, Format),
  (   memberchk(Format, [nquads,ntriples])
  ->  rdf_write_to_sink(Sink, Opts)
  ;   Format == trig
  ->  call_to_stream(Sink, {Opts}/[Out,M,M]>>rdf_save_trig(Out, Opts))
  ;   Format == turtle
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
      call_to_stream(Sink, {TurtleOpts}/[Out,M,M]>>rdf_save_turtle(Out, TurtleOpts))
  ;   Format == xml
  ->  call_to_stream(Sink, {Opts}/[Out,M,M]>>rdf_save_xmlrdf(Out, Opts))
  ;   domain_error(rdf_format, Format)
  ).
