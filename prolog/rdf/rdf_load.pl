:- module(
  rdf_load,
  [
    rdf_load_file/2, % +Input
                     % +Options:list(compound)
    rdf_call_on_triple/3, % +Input
                          % :Goal_2
                          % +Options:list(compound)
    rdf_read_from_graph/3 % +Input
                          % :Goal_1
                          % +Options:list(compound)
  ]
).

/** <module> RDF load

Support for loading RDF data.

@author Wouter Beek
@version 2015/08, 2015/10
*/

:- use_module(library(apply)).
:- use_module(library(debug_ext)).
:- use_module(library(option)).
:- use_module(library(lambda)).
:- use_module(library(lists)).
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

:- meta_predicate(rdf_call_on_triple(+,2)).
:- meta_predicate(rdf_call_on_triple(2,+,+)).
:- meta_predicate(rdf_read_from_graph(+,1)).
:- meta_predicate(rdf_read_from_graph(+,1,+)).

:- predicate_options(rdf_load_file/2, 2, [
     pass_to(rdf_load_file/3, 3),
     pass_to(rdf_call_on_stream/4, 4)
   ]).
:- predicate_options(rdf_load_file/3, 3, [
     pass_to(rdf_load/2, 2)
   ]).
:- predicate_options(rdf_call_on_triple/3, 3, [
     pass_to(rdf_call_on_stream/4, 4)
   ]).
:- predicate_options(rdf_read_from_graph/3, 3, [
     pass_to(rdf_load_file/2)
   ]).





%! rdf_load_file(+Input, +Options:list(compound)) is det.

rdf_load_file(In, Opts):-
  option(graph(G), Opts, _VAR),
  % In the absence of a graph name use the base IRI.
  (var(G), option(base_iri(BaseIri), Opts) -> G = BaseIri ; true),
  rdf_call_on_triple(In, rdf_load_triple0, Opts).

rdf_load_triple0(Stmts, _):-
  maplist(user:rdf_assert, Stmts).  



%! rdf_call_on_triple(+Input, :Goal_2, +Options:list(compound)) is nondet.

rdf_call_on_triple(In, Goal_2, Opts):-
  rdf_call_on_stream(In, read, rdf_call_on_triple0(Goal_2), Opts).

%! rdf_call_on_triple0(:Goal_2, +Metadata:dict, +Read:stream) is det.
% call(:Goal_2, +Statements:list(compound), ?Graph:atom)

rdf_call_on_triple0(Goal_2, M, Read):-
  memberchk(M.rdf.format, [nquads,ntriples]), !,
  rdf_process_ntriples(Read, Goal_2, [base_uri(M.base_iri)]).
rdf_call_on_triple0(Goal_2, M, Read):-
  memberchk(M.rdf.format, [trig,turtle]), !,
  uuid_no_hyphen(UniqueId),
  atomic_list_concat(['__',UniqueId,:], BNodePrefix),
  Opts = [anon_prefix(BNodePrefix),base_uri(M.base_iri)],
  rdf_process_turtle(Read, Goal_2, Opts).
rdf_call_on_triple0(Goal_2, M, Read):-
  xml == M.rdf.format, !,
  process_rdf(Read, Goal_2, [base_uri(M.base_iri)]).
rdf_call_on_triple0(Goal_2, M, Read):-
  rdfa == M.rdf.format, !,
  read_rdfa(Read, Ts, [max_errors(-1),syntax(style)]),
  call(Goal_2, Ts, _).
rdf_call_on_triple0(_, _, M):-
  msg_warning("Unrecognized RDF serialization format: ~a~n", [M.rdf.format]).



%! rdf_read_from_graph(+Input, :Goal_1) is det.
% Wrapper around rdf_read_from_graph/3 with default options.

rdf_read_from_graph(In, Goal_1):-
  rdf_read_from_graph(In, Goal_1, []).


%! rdf_read_from_graph(+Input, :Goal_1, +Options:list(compound)) is det.

rdf_read_from_graph(In, Goal_1, Opts0):-
  setup_call_cleanup(
    rdf_tmp_graph(G),
    (
      merge_options([graph(G)], Opts0, Opts),
      rdf_load_file(In, Opts),
      call(Goal_1, G)
    ),
    rdf_unload_graph(G)
  ).
