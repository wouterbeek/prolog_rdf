:- module(
  rdf_ext,
  [
    rdf_assert/1,           % +Tuple
    rdf_assert/2,           % +Triple, +G
    rdf_create_iri/2,       % +Prefix, -Iri
    rdf_create_iri/3,       % +Prefix, +SubPaths, -Iri
    rdf_nextto_cl/2,        % ?X, ?Y
    rdf_retractall/1,       % +Tuple
    rdf_snap/1,             % :Goal_0
    rdf_unload_db/0
  ]
).

/** <module> RDF extensions

@author Wouter Beek
@compat RDF 1.1
@version 2015/12-2016/06
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(closure)).
:- use_module(library(error)).
:- use_module(library(gen/gen_ntuples)).
:- use_module(library(nlp/nlp_lang)).
:- use_module(library(ordsets)).
:- use_module(library(print_ext)).
:- use_module(library(rdf/rdf_default)).
:- use_module(library(rdf/rdf_prefix), []). % Load RDF prefixes.
:- use_module(library(rdf/rdf_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(uuid_ext)).
:- use_module(library(yall)).

:- rdf_register_prefix(dbo, 'http://dbpedia.org/ontology/').
:- rdf_register_prefix(dcmit, 'http://purl.org/dc/dcmitype/').

:- meta_predicate
    rdf_snap(0).

:- rdf_meta
   rdf_assert(t),
   rdf_assert(t, r),
   rdf_list(r, r, -),
   rdf_nextto_cl(o, o),
   rdf_reification(r, r, o),
   rdf_reification(r, r, o, r),
   rdf_reification(r, r, o, r, -),
   rdf_retractall(t).





%! rdf_assert(+Tuple) is det.
%! rdf_assert(+Triple, +G) is det.

rdf_assert(rdf(S,P,O)) :- !,
  rdf_assert(S, P, O).
rdf_assert(rdf(S,P,O,G)) :-
  rdf_assert(S, P, O, G).


rdf_assert(rdf(S,P,O), G) :-
  rdf_assert(S, P, O, G).



%! rdf_create_iri(+Prefix,            -Iri) is det.
%! rdf_create_iri(+Prefix, +SubPaths, -Iri) is det.
% Succeeds with a fresh IRI within the RDF namespace denoted by Prefix
% and the given SubPaths.
%
% IRI freshness is guaranteed by the UUID that is used as the path suffix.
%
% @arg Prefix   A registered RDF prefix name.
% @arg SubPaths A list of path names that prefix the UUID.
% @arg Iri      A fresh IRI.

rdf_create_iri(Prefix, Iri) :-
  rdf_create_iri(Prefix, [], Iri).


rdf_create_iri(Prefix, SubPaths0, Iri) :-
  uuid_no_hyphen(Id),
  append(SubPaths0, [Id], SubPaths),
  atomic_list_concat(SubPaths, /, LocalName),
  % Resolve the absolute IRI against the base IRI denoted by the RDF prefix.
  qiri(Prefix:LocalName, Iri).



%! rdf_nextto_cl(?X, ?Y, ?RdfList) is nondet.

rdf_nextto_cl(X, Y) :-
  closure([X,Y]>>rdf_nextto(X, Y), X, Y).



%! rdf_retractall(+Tuple) is det.

rdf_retractall(rdf(S,P,O)) :- !,
  rdf_retractall(S, P, O).
rdf_retractall(rdf(S,P,O,G)) :-
  rdf_retractall(S, P, O, G).



%! rdf_snap(:Goal_0) .

rdf_snap(Goal_0) :-
  rdf_transaction(Goal_0, _, [snapshot(true)]).



%! rdf_unload_db is det.

rdf_unload_db :-
  rdf_graph(G), !,
  rdf_unload_graph(G),
  rdf_unload_db.
rdf_unload_db.
