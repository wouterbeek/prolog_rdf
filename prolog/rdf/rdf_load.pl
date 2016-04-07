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

/** <module> RDF load

Support for loading RDF data.

@author Wouter Beek
@version 2015/08, 2015/10-2016/04
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(dict_ext)).
:- use_module(library(error)).
:- use_module(library(http/json)).
:- use_module(library(jsonld/jsonld_metadata)).
:- use_module(library(jsonld/jsonld_read)).
:- use_module(library(option)).
:- use_module(library(os/file_ext)).
:- use_module(library(os/io_ext)).
:- use_module(library(rdf), [process_rdf/3]).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(rdf/rdf_file)). % Type definition.
:- use_module(library(rdf/rdf_graph)).
:- use_module(library(rdf/rdf_stream)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfa), [read_rdfa/3]).
:- use_module(library(semweb/rdf_ntriples), [rdf_process_ntriples/3]).
:- use_module(library(semweb/turtle), [rdf_process_turtle/3]).
:- use_module(library(uuid_ext)).

:- meta_predicate 
    rdf_call_on_graph(+, 1),
    rdf_call_on_graph(+, 1, +),
    rdf_call_on_quad(4, +),
    rdf_call_on_quads(4, +),
    rdf_call_on_quads(4, +, +),
    rdf_call_on_tuples(+, 4),
    rdf_call_on_tuples(+, 4, +),
    rdf_call_on_tuples_stream(4, +, +, +).

:- rdf_meta
   rdf_call_on_graph(+, :, t),
   rdf_call_on_tuples(+, :, t),
   rdf_download_to_file(+, +, t),
   rdf_load_file(+, t),
   rdf_load_tuples(+, -, t).

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

rdf_call_on_tuples(Source, Goal_4) :-
  rdf_call_on_tuples(Source, Goal_4, []).

rdf_call_on_tuples(Source, Goal_4, Opts) :-
  rdf_read_from_stream(Source, rdf_call_on_tuples_stream(Goal_4, Opts), Opts).


%! rdf_call_on_tuples_stream(:Goal_4, +Opts, +Metadata, +Source) is det.
% The following call is made: `call(:Goal_4, +S, +P, +O, +G)`.

rdf_call_on_tuples_stream(Goal_4, Opts1, M, Source) :-
  % Library Semweb uses option base_uri/1.  We use option base_iri/1.
  BaseIri = M.'llo:base_iri'.'@value',
  jsonld_metadata_expand_iri(M.'llo:rdf_format', FormatIri),
  rdf_format_iri(Format, FormatIri),
  uuid_no_hyphen(Uuid),
  atomic_list_concat(['_',Uuid,''], :, BPrefix),
  Opts2 = [
    anon_prefix(BPrefix),
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
  %;   % JSON-LD.
  %    Format == jsonld
  %->  json_read_dict(Source, Json),
  %    forall(jsonld_tuple(Json, Tuple, Opts3), rdf_call_on_quad(Goal_4, Tuple))
  ;   % RDF/XML.
      Format == xml
  ->  process_rdf(Source, rdf_call_on_quads(Goal_4), Opts3)
  ;   % RDFa.
      Format == rdfa
  ->  read_rdfa(Source, Triples, Opts3),
      rdf_call_on_quads(Goal_4, Triples)
  ;   existence_error(rdf_format, [Format])
  ).

rdf_call_on_quad(Goal_4, rdf(S,P,O1,G1)) :- !,
  rdf11:post_graph(G2, G1),
  (G2 == user -> rdf_default_graph(G3) ; G3 = G2),
  (   rdf_is_term(O1)
  ->  call(Goal_4, S, P, O1, G3)
  ;   rdf_legacy_literal_components(O1, D, Lex1, LTag1),
      catch(rdf11:post_object(O2, O1), E, true),
      rdf_literal_components(O2, D, Lex2, LTag2),
      % Non-canonical lexical form.
      (   Lex1 \== Lex2
      ->  print_message(warning, non_canonical_lexical_form(D,Lex1))
      ;   true
      ),
      % Non-canonical language tag.
      (   ground(LTag1),
          LTag1 \== LTag2
      ->  print_message(warning, non_canonical_language_tag(LTag1))
      ;   true
      ),
      % Incorrect lexical form.
      (   var(E)
      ->  call(Goal_4, S, P, O2, G3)
      ;   print_message(warning, E)
      )
  ).
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
%     The default graph.
%     Default is `default'.
%   * quads(-nonneg)
%   * triples(-nonneg)
%   * tuples(-nonneg)

rdf_load_file(Source) :-
  rdf_load_file(Source, []).


rdf_load_file(Source, Opts) :-
  % Allow statistics about the number of tuples to be returned.
  rdf_default_graph(DefG),
  option(graph(ToG), Opts, DefG),
  State = _{quads: 0, triples: 0},
  rdf_call_on_tuples(Source, rdf_load_tuple(State, ToG), Opts),
  NoTuples is State.triples + State.quads,
  option(quads(State.quads), Opts, _),
  option(triples(State.triples), Opts, _),
  option(tuples(NoTuples), Opts, _),
  debug(
    rdf(load),
    "Loaded ~D tuples from ~w (~D triples and ~D quads).~n",
    [NoTuples,Source,State.triples,State.quads]
  ).

% @tbd IRI normalization.
rdf_load_tuple(State, ToG, S, P, O, FromG) :-
  %maplist(bnodify(State), [S0,P0,O0], [S,P,O]),
  (   rdf_default_graph(FromG)
  ->  G = ToG,
      dict_inc(triples, State)
  ;   G = FromG,
      dict_inc(quads, State)
  ),
  rdf_assert(S, P, O, G).

bnodify(State, X, Y) :-
  rdf_is_bnode(X),
  atom_concat('_:', Postfix, X), !,
  atomic_list_concat(['_',State.bnode,Postfix], :, Y).
bnodify(_, X, X).



%! rdf_load_tuples(+Source, -Tuples) is det.
%! rdf_load_tuples(+Source, -Tuples, +Opts) is det.

rdf_load_tuples(Source, Tuples) :-
  rdf_load_tuples(Source, Tuples, []).

rdf_load_tuples(Source, Tuples, Opts) :-
  rdf_snap((
    rdf_retractall(_, _, _),
    rdf_load_file(Source, Opts),
    aggregate_all(set(Tuple), rdf_tuple(Tuple), Tuples)
  )).
