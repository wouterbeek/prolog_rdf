:- module(
  rdf_load,
  [
    rdf_call_on_graph/2, % +Source, :Goal_1
    rdf_call_on_graph/3, % +Source
                         % :Goal_1
                         % +Options:list(compound)
    rdf_call_on_statements/2, % +Source, :Goal_2
    rdf_call_on_statements/3, % +Source
                              % :Goal_2
                              % +Options:list(compound)
    rdf_load_file/1, % +Source
    rdf_load_file/2 % +Source
                    % +Options:list(compound)
  ]
).
:- reexport(library(semweb/rdf_db), [
     rdf_make/0,
     rdf_source_location/2 % +Subject
                           % -Location
   ]).

/** <module> RDF load

Support for loading RDF data.

@author Wouter Beek
@license MIT license
@version 2015/08, 2015/10-2015/12
*/

:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(msg_ext)).
:- use_module(library(option)).
:- use_module(library(os/thread_counter)).
:- use_module(library(rdf), [process_rdf/3]).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_graph)).
:- use_module(library(rdf/rdf_stream)).
:- use_module(library(semweb/rdfa), [read_rdfa/3]).
:- use_module(library(semweb/rdf_ntriples), [rdf_process_ntriples/3]).
:- use_module(library(semweb/turtle), [rdf_process_turtle/3]).
:- use_module(library(uuid_ext)).

:- meta_predicate(rdf_call_on_graph(+,1)).
:- meta_predicate(rdf_call_on_graph(+,1,+)).
:- meta_predicate(rdf_call_on_statements(+,2)).
:- meta_predicate(rdf_call_on_statements(+,2,+)).
:- meta_predicate(rdf_call_on_statements_stream(+,2,+,+)).

:- predicate_options(rdf_call_on_graph/3, 3, [
     pass_to(rdf_load_file/2)
   ]).
:- predicate_options(rdf_call_on_statements/3, 3, [
     pass_to(rdf_read_from_stream/3, 3)
   ]).
:- predicate_options(rdf_load_file/2, 2, [
     quadruples(-nonneg),
     statements(-nonneg),
     triples(-nonneg),
     pass_to(rdf_call_on_statements/3, 3)
   ]).





%! rdf_call_on_graph(+Source, :Goal_1) is det.
% Wrapper around rdf_call_on_graph/3 with default options.

rdf_call_on_graph(In, Goal_1):-
  rdf_call_on_graph(In, Goal_1, []).


%! rdf_call_on_graph(+Source, :Goal_1, +Options:list(compound)) is det.

rdf_call_on_graph(In, Goal_1, Opts0):-
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

rdf_call_on_statements(In, Goal_2):-
  rdf_call_on_statements(In, Goal_2, []).


%! rdf_call_on_statements(+Source, :Goal_2, +Options:list(compound)) is nondet.

rdf_call_on_statements(In, Goal_2, Opts):-
  option(graph(G), Opts, default),
  catch(
    rdf_read_from_stream(In, rdf_call_on_statements_stream(G, Goal_2), Opts),
    E,
    (writeln(Goal_2), print_message(warning, E))
  ).


%! rdf_call_on_statements_stream(
%!   ?Graph:atom,
%!   :Goal_2,
%!   +Metadata:dict,
%!   +Read:stream
%! ) is det.
% The following call is made:
% `call(:Goal_2, +Statements:list(compound), ?Graph:atom)'

rdf_call_on_statements_stream(G, Goal_2, M, Read):-
  memberchk(M.rdf.format, [nquads,ntriples]), !,
  rdf_process_ntriples(
    Read,
    Goal_2,
    [base_uri(M.base_iri),format(M.rdf.format),graph(G)]
  ).
rdf_call_on_statements_stream(G, Goal_2, M, Read):-
  memberchk(M.rdf.format, [trig,turtle]), !,
  uuid_no_hyphen(UniqueId),
  atomic_list_concat(['__',UniqueId,:], BNodePrefix),
  Opts = [anon_prefix(BNodePrefix),base_uri(M.base_iri),graph(G)],
  rdf_process_turtle(Read, Goal_2, Opts).
rdf_call_on_statements_stream(G, Goal_2, M, Read):-
  xml == M.rdf.format, !,
  process_rdf(Read, Goal_2, [base_uri(M.base_iri),graph(G)]).
rdf_call_on_statements_stream(G, Goal_2, M, Read):-
  rdfa == M.rdf.format, !,
  read_rdfa(Read, Ts, [max_errors(-1),syntax(style)]),
  call(Goal_2, Ts, G).
rdf_call_on_statements_stream(_, _, _, M):-
  msg_warning("Unrecognized RDF serialization format: ~a~n", [M.rdf.format]).



%! rdf_load_file(+Source) is det.
% Wrapper around rdf_load_file/2 with default options.

rdf_load_file(In):-
  rdf_load_file(In, []).


%! rdf_load_file(+Source, +Options:list(compound)) is det.
% The following options are supported:
%   * base_iri(+atom)
%   * graph(+atom)
%   * triples(-nonneg)
%   * quadruples(-nonneg)
%   * statements(-nonneg)

rdf_load_file(In, Opts):-
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
      msg_notification(
        "Loaded ~D statements from ~w (~D triples and ~D quadruples).~n",
        [NS,In,NT,NQ]
      )
    )
  ).


rdf_load_statements(CT, CQ, Stmts, G):-
  maplist(rdf_load_statement(CT, CQ, G), Stmts).


% Load a triple.
rdf_load_statement(CT, _, G, rdf(S,P,O)):- !,
  increment_thread_counter(CT),
  rdf_assert(S, P, O, G).
% Load a quadruple.
rdf_load_statement(_, CQ, _, rdf(S,P,O,G)):- !,
  increment_thread_counter(CQ),
  rdf_assert(S, P, O, G).
