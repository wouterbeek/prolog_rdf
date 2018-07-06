:- module(
  rdf_export,
  [
    rdf_clean_assert/3, % +Out, +Tuples, +G
    rdf_clean_assert/4, % +Out, +CleanG, +Tuples, +G
    rdf_save/1,         % +Out
    rdf_save/2,         % +Out, +Type
    rdf_save/3,         % +Out, +Type, +G
    rdf_save/6          % +Out, +Type, ?S, ?P, ?O, ?G
  ]
).

/** <module> RDF export

@author Wouter Beek
@version 2017-2018
*/

:- use_module(library(apply)).
:- use_module(library(semweb/turtle), []).
:- use_module(library(uri_ext)).

:- use_module(library(hash_ext)).
:- use_module(library(semweb/rdf_term)).

:- rdf_meta
   rdf_save(+, +, r),
   rdf_save(+, +, r, r, o, r).





rdf_clean_assert(Out, Tuples, _) :-
  maplist(rdf_clean_assert_(Out), Tuples).

rdf_clean_assert_(Out, rdf(S,P,O)):- !,
  rdf_write_triple(Out, S, P, O).
rdf_clean_assert_(Out, rdf(S,P,O,_)):-
  rdf_write_triple(Out, S, P, O).



%! rdf_clean_assert(+Out:stream, +CleanGraph:iri, +Tuples:list(compound),
%!                  +Graph:atom) is det.

rdf_clean_assert(Out, G, Tuples, _) :-
  maplist(rdf_clean_assert_(Out, G), Tuples).

rdf_clean_assert_(Out, G, rdf(S,P,O)):- !,
  rdf_write_quad(Out, S, P, O, G).
rdf_clean_assert_(Out, G, rdf(S,P,O,_)):-
  rdf_write_quad(Out, S, P, O, G).



%! rdf_save(+Out:stream) is det.
%! rdf_save(+Out:stream, +Type:oneof([quads,triples])) is det.
%! rdf_save(+Out:stream, +Type:oneof([quads,triples]), +G) is det.
%! rdf_save(+Out:stream, +Type:oneof([quads,triples]), ?S, ?P, ?O, ?G) is det.

rdf_save(Out) :-
  rdf_save(Out, quads).


rdf_save(Out, Type) :-
  rdf_save(Out, Type, _).


rdf_save(Out, Type, G) :-
  rdf_save(Out, Type, _, _, _, G).


rdf_save(Out, quads, S, P, O, G) :- !,
  forall(
    rdf(S, P, O, G),
    rdf_write_quad(Out, rdf(S,P,O,G))
  ).
rdf_save(Out, triples, S, P, O, G) :-
  forall(
    rdf(S, P, O, G),
    rdf_write_triple(Out, rdf(S,P,O,G))
  ).
