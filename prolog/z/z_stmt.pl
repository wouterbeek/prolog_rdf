:- module(
  z_stmt,
  [
    quad_g/2,    % +Quad, -G
    quad_o/2,    % +Quad, -O
    quad_p/2,    % +Quad, -P
    quad_s/2,    % +Quad, -S
    triple_o/2,  % +Quad, -O
    triple_p/2,  % +Quad, -P
    triple_s/2,  % +Quad, -S
    z/4,         % +Mode, ?S, ?P, ?O
    z/5,         % +Mode, ?S, ?P, ?O, ?G
    z_assert/2,  % +Mode, +Tuple
    z_assert/4,  % +Mode, +S, +P, +O
    z_assert/5,  % +Mode, +S, +P, +O, +G
    z_quad/2,    % +Mode, -Quad
    z_quad/3,    % +Mode, ?G, -Quad
    z_quad/5,    % +Mode, ?S, ?P, ?O, -Quad
    z_quad/6     % +Mode, ?S, ?P, ?O, ?G, -Quad
    z_quads/2,   % +Mode, -Quads
    z_quads/3,   % +Mode, ?G, -Quads
    z_quads/5,   % +Mode, ?S, ?P, ?O, -Quads
    z_quads/6,   % +Mode, ?S, ?P, ?O, ?G, -Quads
    z_triple/2,  % +Mode, -Triple
    z_triple/3,  % +Mode, ?G, -Triple
    z_triple/5,  % +Mode, ?S, ?P, ?O, -Triple
    z_triple/6,  % +Mode, ?S, ?P, ?O, ?G, -Triple
    z_triples/2, % +Mode, -Triples
    z_triples/3, % +Mode, ?G, -Triples
    z_triples/5, % +Mode, ?S, ?P, ?O, -Triples
    z_triples/6  % +Mode, ?S, ?P, ?O, ?G, -Triples
  ]
).

/** <module> Z statements

Enumerating statements from different back-ends.

@author Wouter Beek
@version 2016/06
*/

:- module(library(aggregate)).
:- module(library(hdt/hdt_ext)).
:- module(library(os/open_any2)).
:- module(library(semweb/rdf11)).

:- rdf_meta
   z(+, r, r, o),
   z(+, r, r, o, r),
   z_assert(+, r, r, o),
   z_assert(+, r, r, o, r),
   z_quad(+, r, -),
   z_quad(+, r, r, o, -),
   z_quad(+, r, r, o, r, -),
   z_quads(+, r, -),
   z_quads(+, r, r, o, -),
   z_quads(+, r, r, o, r, -),
   z_triple(+, r, -),
   z_triple(+, r, r, o, -),
   z_triple(+, r, r, o, r, -),
   z_triples(+, r, -),
   z_triples(+, r, r, o, -),
   z_triples(+, r, r, o, r, -).





%! quad_graph(+Quad, -G) is det.
%! quad_object(+Quad, -O) is det.
%! quad_predicate(+Quad, -P) is det.
%! quad_subject(+Quad, -S) is det.

quad_graph(rdf(_,_,_,G), G).


quad_object(rdf(_,_,O,_), O).


quad_predicate(rdf(_,P,_,_), P).


quad_subject(rdf(S,_,_,_), S).



%! triple_object(+Quad, -O) is det.
%! triple_predicate(+Quad, -P) is det.
%! triple_subject(+Quad, -S) is det.

triple_object(rdf(_,_,O,_), O).


triple_predicate(rdf(_,P,_,_), P).


triple_subject(rdf(S,_,_,_), S).



%! z(+Mode, ?S, ?P, ?O) is nondet.
%! z(+Mode, ?S, ?P, ?O, ?G) is nondet.

z(Mode, S, P, O) :-
  z(Mode, S, P, O, _).


z(disk, S, P, O, G) :-
  hdt(S, P, O, G).
z(mem, S, P, O, G) :-
  rdf(S, P, O, G).



%! z_assert(+Mode, +Tuple) is det.
%! z_assert(+Mode, +S, +P, +O) is det.
%! z_assert(+Mode, +S, +P, +O, +G) is det.

z_assert(Mode, rdf(S,P,O)) :- !,
  z_assert(Mode, S, P, O).
z_assert(Mode, rdf(S,P,O,G)) :-
  z_assert(Mode, S, P, O, G).


z_assert(Mode, S, P, O) :-
  rdf_default_graph(G),
  z_assert(Mode, S, P, O, G).


z_assert(hdt, S, P, O, G) :- !,
  graph_file(G, File),
  call_to_stream(File, [Out,M,M]>>with_output_to(Out, gen_ntriple(S, P, O))).
z_assert(mem, S, P, O, G) :-
  rdf_assert(S, P, O, G).



%! z_quad(+Mode, -Quad) is nondet.
%! z_quad(+Mode, ?G, -Quad) is nondet.
%! z_quad(+Mode, ?S, ?P, ?O, -Quad) is nondet.
%! z_quad(+Mode, ?S, ?P, ?O, ?G, -Quad) is nondet.

z_quad(Mode, Quad) :-
  z_quad(Mode, _, Quad).


z_quad(Mode, G, Quad) :-
  z_quad(Mode, _, _, _, G, Quad).


z_quad(Mode, S, P, O, Quad) :-
  z_quad(Mode, S, P, O, _, Quad).


z_quad(Mode, S, P, O, G, Quad) :-
  z(Mode, S, P, O, G),
  Quad = rdf(S, P, O, G).



%! z_quads(+Mode, -Quads) is nondet.
%! z_quads(+Mode, ?G, -Quads) is nondet.
%! z_quads(+Mode, ?S, ?P, ?O, -Quads) is nondet.
%! z_quads(+Mode, ?S, ?P, ?O, ?G, -Quads) is nondet.

z_quads(Mode, Quads) :-
  z_quads(Mode, _, Quads).


z_quads(Mode, G, Quads) :-
  z_quads(Mode, _, _, _, G, Quads).


z_quads(Mode, S, P, O, Quads) :-
  z_quads(Mode, S, P, O, _, Quads).


z_quads(Mode, S, P, O, G, Quads) :-
  aggregate_all(set(Quad), z_quad(Mode, S, P, O, G, Quad), Quads).



%! z_triple(+Mode, ?Triple) is nondet.
%! z_triple(+Mode, ?G, ?Triple) is nondet.
%! z_triple(+Mode, ?S, ?P, ?O, ?Triple) is nondet.
%! z_triple(+Mode, ?S, ?P, ?O, ?G, ?Triple) is nondet.

z_triple(Mode, Triple) :-
  z_triple(Mode, _, Triple).


z_triple(Mode, G, Triple) :-
  z_triple(Mode, _, _, _, G, Triple).


z_triple(Mode, S, P, O, Triple) :-
  z_triple(Mode, S, P, O, _, Triple).


z_triple(Mode, S, P, O, G, Triple) :-
  z(Mode, S, P, O, G),
  Triple = rdf(S, P, O).



%! z_triples(+Mode, -Triple) is nondet.
%! z_triples(+Mode, ?G, -Triple) is nondet.
%! z_triples(+Mode, ?S, ?P, ?O, -Triple) is nondet.
%! z_triples(+Mode, ?S, ?P, ?O, ?G, -Triple) is nondet.

z_triples(Mode, Triples) :-
  z_triples(Mode, _, Triples).


z_triples(Mode, G, Triples) :-
  z_triples(Mode, _, _, _, G, Triples).


z_triples(Mode, S, P, O, Triples) :-
  z_triples(Mode, S, P, O, _, Triples).


z_triples(Mode, S, P, O, G, Triples) :-
  aggregate_all(set(Triple), z_triple(Mode, S, P, O, G, Triple), Triples).





% HELPERS %

%! graph_file(+G, -File) is det.

graph_file(G, File) :-
  rdf_global_id(Alias:Local, G),
  directory_file_name(Alias, Local, Base),
  file_name_extension(Base, nt, File).
