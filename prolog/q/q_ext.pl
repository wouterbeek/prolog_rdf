:- module(
  q_ext,
  [
    q_aggregate_all/3, % +Template, :Goal, -Result
    q_graph/2,         % ?M, ?G
    q_graph_to_file/3, % +G, +Comps, -File
    q_load/2,          % +M, +G
    q_load/3,          % +M, +G, +Opts
    q_load_or_call/3,  % +M, :Goal_1, +G
    q_load_or_call/4,  % +M, :Goal_1, +G, +Opts
    q_save/1           % +G
  ]
).

/** <module> Quine generics

Generic support for the Quine abstraction layer.

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(debug)).
:- use_module(library(hdt/hdt_ext)).
:- use_module(library(rdf/rdfio)).
:- use_module(library(semweb/rdf11)).

:- meta_predicate
    q_aggregate_all(+, 0, -),
    q_load_or_call(+, 1, +).

:- rdf_meta
   q_aggregate_all(+, t, -),
   q_graph(?, r),
   q_load(+, r),
   q_load(+, r, +),
   q_load_or_call(+, :, r),
   q_load_or_call(+, :, r, +),
   q_save(r).

:- debug(q(ext)).





%! q_aggregate_all(+Template, :Goal, -Result) is det.

q_aggregate_all(Template, Goal, Result) :-
  aggregate_all(Template, Goal, Result).



%! q_graph(?M, ?G) is nondet.

q_graph(rdf, G) :-
  rdf_graph(G).
q_graph(hdt, G) :-
  hdt_graph(G).



%! q_graph_to_file(+G, +Comps, -HdtFile) is det.

q_graph_to_file(G, Comps, HdtFile) :-
  q_graph_to_base(G, Base),
  atomic_list_concat([Base|Comps], ., Local),
  absolute_file_name(data(Local), HdtFile, [access(write)]).



%! q_load(+M, +G) is det.
%! q_load(+M, +G, +Opts) is det.

q_load(M, G) :-
  q_load(M, G, []).


q_load(hdt, G, Opts) :- !,
  hdt_load(G, Opts).
q_load(rdf, G, Opts) :- !,
  rdf_load(G, Opts).



%! q_load_or_call(+M, :Goal_1, +G) is det.
%! q_load_or_call(+M, :Goal_1, +G, +Opts) is det.

q_load_or_call(M, Goal_1, G) :-
  q_load_or_call(M, Goal_1, G, []).


q_load_or_call(M, Goal_1, G, Opts) :-
  (   q_load(M, G)
  ->  debug(q(ext), "Loaded graph ~w into backend ~a", [G,M])
  ;   call(Goal_1, G, Opts),
      debug(q(ext), "Called goal to generate graph ~w", [G]),
      q_load_or_call(M, Goal_1, G, Opts)
  ).



%! q_save(+G) is det.

q_save(G) :-
  q_graph_to_file(G, [nt,gz], File),
  rdf_write_to_sink(File, G, [compression(gzip),rdf_format(ntriples)]).





% HELPERS %

%! q_graph_to_base(+G, -Base) is det.

q_graph_to_base(G, Base) :-
  rdf_global_id(Alias:Local, G), !,
  atomic_list_concat([Alias,Local], '_', Base).
q_graph_to_base(G, Base) :-
  Base = G.
