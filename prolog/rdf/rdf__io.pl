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
    rdf_call_to_graph/2,         % ?Sink, :Goal_1
    rdf_call_to_graph/3,         % ?Sink, :Goal_1, +Opts
    rdf_change_format/2,         % +Source, +Sink
    rdf_change_format/3,         % +Source, +Sink, +Opts
    rdf_change_format_legacy/2,  % +Source, +Sink
    rdf_change_format_legacy/3,  % +Source, +Sink, +Opts
    rdf_download_to_file/2,      % +Iri, +File
    rdf_download_to_file/4,      % +Iri, +File, +InOpts, +OutOpts
    rdf_load_file/1,             % +Source
    rdf_load_file/2,             % +Source,           +Opts
    rdf_load_quads/2,            % +Source, -Quads
    rdf_load_quads/3,            % +Source, -Quads,   +Opts
    rdf_load_triples/2,          % +Source, -Triples
    rdf_load_triples/3,          % +Source, -Triples, +Opts
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

The following debug flags are used:

  * rdf(rdf__io)

@author Wouter Beek
@version 2015/08-2016/02, 2016/04-2016/07
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug)).
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
:- use_module(library(q/q_io)).
:- use_module(library(q/q_stmt)).
:- use_module(library(q/q_term)).
:- use_module(library(q/qb)).
:- use_module(library(rdf), [process_rdf/3]).
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
    rdf_call_to_graph(?, 1),
    rdf_call_to_graph(?, 1, +).

:- rdf_meta
   rdf_call_on_graph(+, :, t),
   rdf_call_on_tuples(+, :, t),
   rdf_load_file(+, t),
   rdf_load_quads(+, -, t),
   rdf_load_triples(+, -, t),
   rdf_write_to_sink(+, ?, r),
   rdf_write_to_sink(+, ?, r, +),
   rdf_write_to_sink(+, ?, r, r, o),
   rdf_write_to_sink(+, ?, r, r, o, r),
   rdf_write_to_sink(+, ?, r, r, o, r, +).





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
  % Accept headers for RDF requests are specified in library
  % `semweb/rdf_http_plugin'.
  rdf_http_plugin:rdf_extra_headers(DefRdfOpts, Opts1),
  merge_options(DefRdfOpts, Opts1, Opts2),
  call_on_stream(Source, rdf_call_on_stream0(Goal_3, Opts2), Opts2).


rdf_call_on_stream0(Goal_3, Opts, In, L1, L3) :-
  set_rdf_format(In, L1, L2, Opts),
  call(Goal_3, In, L2, L3).



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
  qb_bnode_prefix(BPrefix),
  % Library Semweb uses option base_uri/1.  We use option base_iri/1
  % instead.
  get_base_iri(BaseIri, Path, Opts1),
  once(get_dicts(rdf_format, Path, Format)),
  Opts2 = [
    anon_prefix(BPrefix),
    base(BaseIri),
    base_uri(BaseIri),
    format(Format),
    max_errors(-1),
    syntax(style)
  ],
  merge_options(Opts1, Opts2, Opts3),
  (   % N-Quads & N-Triples
      memberchk(Format, [nquads,ntriples])
  ->  rdf_process_ntriples(In, rdf_call_on_quads0(Goal_5, Path), Opts3)
  ;   % Trig & Turtle
      memberchk(Format, [trig,turtle])
  ->  rdf_process_turtle(In, rdf_call_on_quads0(Goal_5, Path), Opts3)
  ;   % JSON-LD
      Format == jsonld
  ->  json_read_dict(In, Json),
      forall(
	jsonld_tuple(Json, Tuple, Opts3),
        rdf_call_on_quad0(Goal_5, Path, Tuple)
      )
  ;   % RDF/XML
      Format == xml
  ->  process_rdf(In, rdf_call_on_quads0(Goal_5, Path), Opts3)
  ;   % RDFa
      Format == rdfa
  ->  read_rdfa(In, Triples, Opts3),
      rdf_call_on_quads0(Goal_5, Path, Triples)
  ).


rdf_call_on_quad0(Goal_5, L, rdf(S,P,O1,G1)) :- !,
  rdf11:post_graph(G2, G1),
  (G2 == user -> q_default_graph(G3) ; G3 = G2),
  (   rdf_is_term(O1)
  ->  call(Goal_5, L, S, P, O1, G3)
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
      ->  call(Goal_5, L, S, P, O2, G3)
      ;   print_message(warning, E)
      )
  ).
rdf_call_on_quad0(Goal_5, L, rdf(S,P,O)) :-
  q_default_graph(G),
  rdf_call_on_quad0(Goal_5, L, rdf(S,P,O,G)).


rdf_call_on_quads0(Goal_5, L, Tuples) :-
  maplist(rdf_call_on_quad0(Goal_5, L), Tuples).


rdf_call_on_quads0(Goal_5, L, Tuples, _) :-
  rdf_call_on_quads0(Goal_5, L, Tuples).



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
      rdf_write_to_sink(Sink, rdf, _, _, _, G, Opts)
    ),
    rdf_unload_graph(G)
  ).



%! rdf_change_format(+Source, +Sink) is det.
%! rdf_change_format(+Source, +Sink, +Opts) is det.
%
% The following options are supported:
%
%   * from_format(+atom)
%
%   * to_format(+atom)
%
%   * Other options are passed to:
%
%     * rdf_call_on_tuples/3
%     * rdf_write_to_sink/7

rdf_change_format(Source, Sink) :-
  rdf_change_format(Source, Sink, []).


rdf_change_format(Source, Sink, Opts) :-
  (   option(from_format(FromFormat), Opts)
  ->  merge_options(Opts, [rdf_format(FromFormat)], SourceOpts)
  ;   SourceOpts = Opts
  ),
  (   option(to_format(ToFormat), Opts)
  ->  merge_options(Opts, [rdf_format(ToFormat)], SinkOpts)
  ;   SinkOpts = Opts
  ),
  call_to_ntuples(Sink, rdf_change_format0(Source, SourceOpts), SinkOpts).


rdf_change_format0(Source, SourceOpts, State, Out) :-
  rdf_call_on_tuples(
    Source,
    {State,Out}/[_,S,P,O,G]>>gen_ntuple(S, P, O, G, State, Out),
    SourceOpts
  ).



%! rdf_change_format_legacy(+Source, +Sink) is det.
%! rdf_change_format_legacy(+Source, +Sink, +Opts) is det.

rdf_change_format_legacy(Source, Sink) :-
  rdf_change_format_legacy(Source, Sink, []).


rdf_change_format_legacy(Source, Sink, Opts) :-
  rdf_load_file(Source),
  rdf_write_to_sink_legacy(Sink, rdf, Opts).



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

copy_stream_data0(In, L, L, Out) :-
  copy_stream_data(In, Out).



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
  ;   q_default_graph(DefG),
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
    rdf(rdf__io),
    "Loaded ~D tuples from ~w (~D triples and ~D quads).~n",
    [NumTuples,Source,State.triples,State.quads]
  ).


rdf_force_load_tuple0(State, ToG, _, S, P, O, FromG) :-
  count_tuple0(State, FromG),
  % @tbd IRI normalization.
  rdf_assert(S, P, O, ToG).


rdf_load_tuple0(State, ToG, _, S, P, O, FromG) :-
  count_tuple0(State, FromG),
  (q_default_graph(FromG) -> G = ToG ; G = FromG),
  % @tbd IRI normalization.
  rdf_assert(S, P, O, G).


count_tuple0(State, G) :-
  q_default_graph(G), !,
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
    q_quads(rdf, Quads)
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
    q_triples(rdf, Triples)
  )).



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
%   * `rdf_format(+atom)`
%
% The following formats are supported:
%
%   * `nquads` (N-Quads)
%
%   * `ntriples` (N-Triples)
%
% For other formats use rdf_write_to_sink_legacy/[2,3].

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
  q_default_graph(DefG),
  defval(DefG, G),
  rdf_write_format0(Sink, Opts1, Format),
  merge_options(Opts1, [format(Format)], Opts2),
  call_to_ntuples(Sink, gen_ntuples(M, S, P, O, G), Opts2).


rdf_file_name0(File, Opts) :-
  uuid(Base),
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



%! rdf_write_to_sink_legacy(?Sink, ?M) is det.
%! rdf_write_to_sink_legacy(?Sink, ?M, +Opts) is det.
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

rdf_write_to_sink_legacy(Sink, M) :-
  rdf_write_to_sink_legacy(Sink, M, []).


rdf_write_to_sink_legacy(Sink, M, Opts) :-
  rdf_write_format0(Sink, Opts, Format),
  (   memberchk(Format, [nquads,ntriples])
  ->  rdf_write_to_sink(Sink, M, Opts)
  ;   Format == trig
  ->  call_to_stream(Sink, {Opts}/[Out]>>rdf_save_trig(Out, Opts))
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
      call_to_stream(
	Sink,
	{TurtleOpts}/[Out]>>rdf_save_turtle(Out, TurtleOpts)
      )
  ;   Format == xml
  ->  call_to_stream(Sink, {Opts}/[Out]>>rdf_save_xmlrdf(Out, Opts))
  ;   domain_error(rdf_format, Format)
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



%! set_rdf_format(+In, +Path1, -Path2, +Opts) is det.

% Option rdf_format/1 overrides everything else.
set_rdf_format(_, [H1|T], [H2|T], Opts) :-
  option(rdf_format(Format), Opts), !,
  put_dict(rdf_format, H1, Format, H2).
set_rdf_format(_, [H|T], [H|T], _) :-
  dict_has_key(rdf_format, H), !.
set_rdf_format(In, [H1|T], [H2|T], Opts) :-
  (   get_base_iri(BaseIri, [H1|T], Opts),
      iri_file_extensions(BaseIri, Exts),
      member(Ext, Exts),
      rdf_file_extension(Ext, Format)
  ->  FormatOpts = [default_rdf_format(Format)]
  ;   FormatOpts = []
  ),
  % Notice that the metadata option of the original options list does
  % not get overwritten when opening the stream for guessing the RDF
  % serialization format.
  rdf_guess_format(In, Format, FormatOpts),
  % JSON-LD _must_ be encoded in UTF-8.
  (Format == jsonld -> set_stream(In, encoding(utf8)) ; true),
  put_dict(rdf_format, H1, Format, H2).
