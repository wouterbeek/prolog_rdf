:- module(
  rdf_load,
  [
    rdf_call_on_graph/2, % +Input, :Goal_1
    rdf_call_on_graph/3, % +Input
                         % :Goal_1
                         % +Options:list(compound)
    rdf_call_on_triples/2, % +Input, :Goal_2
    rdf_call_on_triples/3, % +Input
                           % :Goal_2
                           % +Options:list(compound)
    rdf_load_file/1, % +Input
    rdf_load_file/2 % +Input
                    % +Options:list(compound)
  ]
).

/** <module> RDF load

Support for loading RDF data.

@author Wouter Beek
@license MIT license
@version 2015/08, 2015/10
*/

:- use_module(library(apply)).
:- use_module(library(count_ext)).
:- use_module(library(option)).
:- use_module(library(msg_ext)).
:- use_module(library(option)).
:- use_module(library(rdf)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_graph)).
:- use_module(library(rdf/rdf_stream)).
:- use_module(library(semweb/rdfa)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_ntriples)).
:- use_module(library(semweb/turtle)).
:- use_module(library(uuid_ext)).

:- meta_predicate(rdf_call_on_graph(+,1)).
:- meta_predicate(rdf_call_on_graph(+,1,+)).
:- meta_predicate(rdf_call_on_triples(+,2)).
:- meta_predicate(rdf_call_on_triples(+,2,+)).
:- meta_predicate(rdf_call_on_triples_stream(+,2,+,+)).

:- predicate_options(rdf_call_on_graph/3, 3, [
     pass_to(rdf_load_file/2)
   ]).
:- predicate_options(rdf_call_on_triples/3, 3, [
     pass_to(rdf_call_on_stream/4, 4)
   ]).
:- predicate_options(rdf_load_file/2, 2, [
     pass_to(rdf_call_on_triples/3, 3)
   ]).





%! rdf_call_on_graph(+Input, :Goal_1) is det.
% Wrapper around rdf_call_on_graph/3 with default options.

rdf_call_on_graph(In, Goal_1):-
  rdf_call_on_graph(In, Goal_1, []).


%! rdf_call_on_graph(+Input, :Goal_1, +Options:list(compound)) is det.

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



%! rdf_call_on_triples(+Input, :Goal_2) is nondet.
% Wrapper around rdf_call_on_triples/3 with default options.

rdf_call_on_triples(In, Goal_2):-
  rdf_call_on_triples(In, Goal_2, []).


%! rdf_call_on_triples(+Input, :Goal_2, +Options:list(compound)) is nondet.

rdf_call_on_triples(In, Goal_2, Opts):-
  option(graph(G), Opts, _),
  rdf_call_on_stream(In, read, rdf_call_on_triples_stream(G, Goal_2), Opts).


%! rdf_call_on_triples_stream(
%!   ?Graph:atom,
%!   :Goal_2,
%!   +Metadata:dict,
%!   +Read:stream
%! ) is det.
% The following call is made:
% `call(:Goal_2, +Statements:list(compound), ?Graph:atom)'

rdf_call_on_triples_stream(G, Goal_2, M, Read):-
  memberchk(M.rdf.format, [nquads,ntriples]), !,
  rdf_process_ntriples(Read, Goal_2, [base_uri(M.base_iri),graph(G)]).
rdf_call_on_triples_stream(G, Goal_2, M, Read):-
  memberchk(M.rdf.format, [trig,turtle]), !,
  uuid_no_hyphen(UniqueId),
  atomic_list_concat(['__',UniqueId,:], BNodePrefix),
  Opts = [anon_prefix(BNodePrefix),base_uri(M.base_iri),graph(G)],
  rdf_process_turtle(Read, Goal_2, Opts).
rdf_call_on_triples_stream(G, Goal_2, M, Read):-
  xml == M.rdf.format, !,
  process_rdf(Read, Goal_2, [base_uri(M.base_iri),graph(G)]).
rdf_call_on_triples_stream(G, Goal_2, M, Read):-
  rdfa == M.rdf.format, !,
  read_rdfa(Read, Ts, [max_errors(-1),syntax(style)]),
  call(Goal_2, Ts, G).
rdf_call_on_triples_stream(_, _, _, M):-
  msg_warning("Unrecognized RDF serialization format: ~a~n", [M.rdf.format]).



%! rdf_load_file(+Input) is det.
% Wrapper around rdf_load_file/2 with default options.

rdf_load_file(In):-
  rdf_load_file(In, []).


%! rdf_load_file(+Input, +Options:list(compound)) is det.
% The following options are supported:
%   * base_iri(+atom)
%   * graph(+atom)
%   * triples(-nonneg)
%   * quadruples(-nonneg)
%   * statements(-nonneg)

rdf_load_file(In, Opts):-
  % Allow statistics about the number of statements to be returned.
  option(triples(N1), Opts, _),
  option(quadruples(N2), Opts, _),
  option(statements(N3), Opts, _),

  % In the absence of a graph name use the base IRI.
  (   option(graph(G), Opts, _),
      var(G),
      option(base_iri(BaseIri), Opts)
  ->  G = BaseIri
  ;   true
  ),

  setup_call_cleanup(
    (
      create_thread_counter(number_of_triples,    C1),
      create_thread_counter(number_of_quadruples, C2)
    ),
    rdf_call_on_triples(In, rdf_load_triples(C1, C2), Opts),
    (
      delete_counter(C1, N1),
      delete_counter(C2, N2),
      N3 is N1 + N2,
      msg_notification(
        "Loaded ~D statements from ~w (~D triples and ~D quadruples).~n",
        [N3,In,N1,N2]
      )
    )
  ).

% C1:  Number of triples.
% C2:  Number of quadruples.

rdf_load_triples(C1, C2, Stmts, G):-
  maplist(rdf_load_triple(C1, C2, G), Stmts).

rdf_load_triple(C1, _, G:_, rdf(S,P,O)):- !,
  increment_counter(C1),
  user:rdf_assert(S, P, O, G).
rdf_load_triple(_, C2, _, rdf(S,P,O,G)):- !,
  increment_counter(C2),
  user:rdf_assert(S, P, O, G).
