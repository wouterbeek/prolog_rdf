:- module(
  rdf_load,
  [
    rdf_load_file/1, % +In
    rdf_load_file/2, % +In
                     % +Options:list(compound)
    rdf_load_triple/2, % +In
                       % :Goal_2
    rdf_read_from_graph/2, % +In
                           % :Goal_1
    rdf_read_from_graph/3 % +In
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
:- use_module(library(lists)).
:- use_module(library(msg_ext)).
:- use_module(library(option)).
:- use_module(library(rdf)).
:- use_module(library(rdf/rdf_graph)).
:- use_module(library(rdf/rdf_stream)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_ntriples)).
:- use_module(library(semweb/turtle)).
:- use_module(library(uuid_ext)).

:- meta_predicate(rdf_load_triple(+,2)).
:- meta_predicate(rdf_read_from_graph(+,1)).
:- meta_predicate(rdf_read_from_graph(+,1,+)).

:- predicate_options(rdf_load_file/2, 2, [
     pass_to(rdf_load_file0/4, 1),
     pass_to(rdf_stream_read/3, 3)
   ]).
:- predicate_options(rdf_load_file0/4, 1, [
     pass_to(rdf_load/2, 2)
   ]).
:- predicate_options(rdf_read_from_graph/3, 3, [
     pass_to(rdf_load_file/2)
   ]).





%! rdf_load_file(+In) is det.
% Wrapper around rdf_load_file/2 with default options.

rdf_load_file(In):-
  rdf_load_file(In, []).


%! rdf_load_file(+In, +Options:list(compound)) is det.

rdf_load_file(In, Opts1):-
  % Handle the graph name option.
  select_option(graph(G), Opts1, Opts2, _VAR),

  % In the absence of a graph name use the base IRI.
  (var(G), option(base_iri(BaseIri), Opts1) -> G = BaseIri ; true),
  
  merge_options([graph(G)], Opts2, Opts3),
  rdf_stream_read(In, rdf_load_file0(Opts3), Opts2).

rdf_load_file0(Opts0, Read, M):-
  merge_options([base_iri(M.base_iri),format(M.rdf.format)], Opts0, Opts),
  rdf_load(Read, Opts).



%! rdf_load_triple(+In, :Goal_2) is nondet.

rdf_load_triple(In, Goal_2):-
  rdf_stream_read(In, rdf_load_triple0(Goal_2), []).


%! rdf_load_triple0(:Goal_2, +Metadata:dict, +Read:stream) is det.

rdf_load_triple0(Goal_2, M, Read):-
  memberchk(M.rdf.format, [nquads,ntriples]), !,
  rdf_process_ntriples(Read, Goal_2, [base_uri(M.base_iri)]).
rdf_load_triple0(Goal_2, M, Read):-
  memberchk(M.rdf.format, [trig,turtle]), !,
  uuid_no_hyphen(UniqueId),
  atomic_list_concat(['__',UniqueId,:], BNodePrefix),
  Opts = [anon_prefix(BNodePrefix),base_uri(M.base_iri)],
  rdf_process_turtle(Read, Goal_2, Opts).
rdf_load_triple0(Goal_2, M, Read):-
  xml == M.rdf.format, !,
  process_rdf(Read, Goal_2, [base_uri(M.base_iri)]).
rdf_load_triple0(_, _, M):-
  msg_warning("Unrecognized RDF serialization format: ~a~n", [M.format]).




%! rdf_read_from_graph(+In, :Goal_1) is det.
% Wrapper around rdf_read_from_graph/3 with default options.

rdf_read_from_graph(In, Goal_1):-
  rdf_read_from_graph(In, Goal_1, []).


%! rdf_read_from_graph(+In, :Goal_1, +Options:list(compound)) is det.

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
