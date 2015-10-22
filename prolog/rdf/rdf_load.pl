:- module(
  rdf_load,
  [
    rdf_load_file/1, % +Input
    rdf_load_file/2, % +Input
                     % +Options:list(compound)
    rdf_call_on_triple/2, % +Input
                          % :Goal_2
    rdf_read_from_graph/2, % +Input
                           % :Goal_1
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

:- use_module(library(debug_ext)).
:- use_module(library(option)).
:- use_module(library(lambda)).
:- use_module(library(lists)).
:- use_module(library(msg_ext)).
:- use_module(library(option)).
:- use_module(library(rdf)).
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
:- predicate_options(rdf_read_from_graph/3, 3, [
     pass_to(rdf_load_file/2)
   ]).





%! rdf_load_file(+Input) is det.
% Wrapper around rdf_load_file/2 with default options.

rdf_load_file(In):-
  rdf_load_file(In, []).


%! rdf_load_file(+Input, +Options:list(compound)) is det.

rdf_load_file(In, Opts1):-
  % Handle the graph name option.
  select_option(graph(G), Opts1, Opts2, _VAR),

  % In the absence of a graph name use the base IRI.
  (var(G), option(base_iri(BaseIri), Opts1) -> G = BaseIri ; true),
  
  merge_options([graph(G)], Opts2, Opts3),
  rdf_call_on_stream(In, read, \M^Read^rdf_load_file(M, Read, Opts3), Opts2).

rdf_load_file(M, Read, Opts0):-
  merge_options([base_iri(M.base_iri),format(M.rdf.format)], Opts0, Opts),
  rdf_load(Read, Opts).



%! rdf_call_on_triple(+Input, :Goal_2) is nondet.

rdf_call_on_triple(In, Goal_2):-
  rdf_call_on_stream(In, read, rdf_call_on_triple(Goal_2), []).


%! rdf_call_on_triple(:Goal_2, +Metadata:dict, +Read:stream) is det.
% call(:Goal_2, +Statements:list(compound), ?Graph:atom)

rdf_call_on_triple(Goal_2, M, Read):-
  memberchk(M.rdf.format, [nquads,ntriples]), !,
  rdf_process_ntriples(Read, Goal_2, [base_uri(M.base_iri)]).
rdf_call_on_triple(Goal_2, M, Read):-
  memberchk(M.rdf.format, [trig,turtle]), !,
  uuid_no_hyphen(UniqueId),
  atomic_list_concat(['__',UniqueId,:], BNodePrefix),
  Opts = [anon_prefix(BNodePrefix),base_uri(M.base_iri)],
  rdf_process_turtle(Read, Goal_2, Opts).
rdf_call_on_triple(Goal_2, M, Read):-
  xml == M.rdf.format, !,
  process_rdf(Read, Goal_2, [base_uri(M.base_iri)]).
rdf_call_on_triple(Goal_2, M, Read):-
  rdfa == M.rdf.format, !,
  read_rdfa(Read, Ts, [max_errors(-1),syntax(style)]),
  call(Goal_2, Ts, _).
rdf_call_on_triple(_, _, M):-
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
