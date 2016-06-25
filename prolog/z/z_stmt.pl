:- module(
  z_stmt,
  [
    z/3,                  % ?S, ?P, ?O
    z/4,                  % ?S, ?P, ?O, ?G
    z_quad/1,             % -Quad
    z_quad/2,             % ?G, -Quad
    z_quad/4,             % ?S, ?P, ?O, -Quad
    z_quad/5,             % ?S, ?P, ?O, ?G, -Quad
    z_quad_datatype/2,    % +Quad, -D
    z_quad_graph/2,       % +Quad, -G
    z_quad_iri/2,         % +Quad, -Iri
    z_quad_object/2,      % +Quad, -O
    z_quad_predicate/2,   % +Quad, -P
    z_quad_subject/2,     % +Quad, -S
    z_quad_term/2,        % +Quad, -Term
    z_quad_terms/5,       % ?Quad, ?S, ?P, ?O, ?G
    z_quads/1,            % -Quads
    z_quads/2,            % ?G, -Quads
    z_quads/4,            % ?S, ?P, ?O, -Quads
    z_quads/5,            % ?S, ?P, ?O, ?G, -Quads
    z_reification/3,      % ?S, ?P, ?O
    z_reification/4,      % ?S, ?P, ?O, ?G
    z_reification/5,      % ?S, ?P, ?O, ?G, -Stmt
    z_triple/1,           % -Triple
    z_triple/2,           % ?G, -Triple
    z_triple/4,           % ?S, ?P, ?O, -Triple
    z_triple/5,           % ?S, ?P, ?O, ?G, -Triple
    z_triple_iri/2,       % +Triple, -Iri
    z_triple_object/2,    % +Triple, -O
    z_triple_predicate/2, % +Triple, -P
    z_triple_subject/2,   % +Triple, -S
    z_triple_term/2,      % +Triple, -Term
    z_triple_terms/4,     % ?Triple, ?S, ?P, ?O
    z_triples/1,          % -Triples
    z_triples/2,          % ?G, -Triples
    z_triples/4,          % ?S, ?P, ?O, -Triples
    z_triples/5           % ?S, ?P, ?O, ?G, -Triples
  ]
).

/** <module> RDF statements

Enumerating statements from different back-ends.

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(aggregate)).
:- use_module(library(hdt/hdt_ext)).
:- use_module(library(gen/gen_ntuples)).
:- use_module(library(os/open_any2)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(yall)).

:- rdf_meta
   z(r, r, o),
   z(r, r, o, r),
   z_quad(r, -),
   z_quad(r, r, o, -),
   z_quad(r, r, o, r, -),
   z_quad_datatype(t, r),
   z_quad_graph(t, r),
   z_quad_iri(t, r),
   z_quad_object(t, o),
   z_quad_predicate(t, r),
   z_quad_subject(t, r),
   z_quad_term(t, o),
   z_quad_terms(t, r, r, o, r),
   z_quads(r, -),
   z_quads(r, r, o, -),
   z_quads(r, r, o, r, -),
   z_reification(r, r, o),
   z_reification(r, r, o, r),
   z_reification(r, r, o, r, r),
   z_triple(r, -),
   z_triple(r, r, o, -),
   z_triple(r, r, o, r, -),
   z_triple_datatype(t, r),
   z_triple_iri(t, r),
   z_triple_object(t, o),
   z_triple_predicate(t, r),
   z_triple_subject(t, r),
   z_triple_term(t, o),
   z_triple_terms(t, r, r, o),
   z_triples(r, -),
   z_triples(r, r, o, -),
   z_triples(r, r, o, r, -).





%! z(?S, ?P, ?O) is nondet.
%! z(?S, ?P, ?O, ?G) is nondet.

z(S, P, O) :-
  z(S, P, O, _).


z(S, P, O, G) :-
  rdf(S, P, O, G).
z(S, P, O, G) :-
  hdt(S, P, O, G).



%! z_quad(-Quad) is nondet.
%! z_quad(?G, -Quad) is nondet.
%! z_quad(?S, ?P, ?O, -Quad) is nondet.
%! z_quad(?S, ?P, ?O, ?G, -Quad) is nondet.

z_quad(Quad) :-
  z_quad(_, Quad).


z_quad(G, Quad) :-
  z_quad(_, _, _, G, Quad).


z_quad(S, P, O, Quad) :-
  z_quad(S, P, O, _, Quad).


z_quad(S, P, O, G, Quad) :-
  rdf(S, P, O, G),
  Quad = rdf(S, P, O, G).



%! z_quad_datatype(+Quad, -D) is det.
%! z_quad_graph(+Quad, -G) is det.
%! z_quad_iri(+Quad, -Iri) is det.
%! z_quad_object(+Quad, -O) is det.
%! z_quad_predicate(+Quad, -P) is det.
%! z_quad_subject(+Quad, -S) is det.
%! z_quad_term(+Quad, -Term) is nondet.

z_quad_datatype(rdf(_,_,O,_), D) :-
  z_literal_datatype(O, D).


z_quad_graph(rdf(_,_,_,G), G).


z_quad_iri(Quad, Iri) :-
  z_quad_term(Quad, Iri),
  rdf_is_iri(Iri).


z_quad_object(rdf(_,_,O,_), O).


z_quad_predicate(rdf(_,P,_,_), P).


z_quad_subject(rdf(S,_,_,_), S).


z_quad_term(rdf(S,_,_,_), S).
z_quad_term(rdf(_,P,_,_), P).
z_quad_term(rdf(_,_,O,_), O).
z_quad_term(rdf(_,_,_,G), G).



%! z_quad_terms(?Quad, ?S, ?P, ?O, ?G) is det.

z_quad_terms(rdf(S,P,O,G), S, P, O, G).



%! z_quads(-Quads) is nondet.
%! z_quads(?G, -Quads) is nondet.
%! z_quads(?S, ?P, ?O, -Quads) is nondet.
%! z_quads(?S, ?P, ?O, ?G, -Quads) is nondet.

z_quads(Quads) :-
  z_quads(_, Quads).


z_quads(G, Quads) :-
  z_quads(_, _, _, G, Quads).


z_quads(S, P, O, Quads) :-
  z_quads(S, P, O, _, Quads).


z_quads(S, P, O, G, Quads) :-
  aggregate_all(set(Quad), z_quad(S, P, O, G, Quad), Quads).



%! z_reification(?S, ?P, ?O) is nondet.
%! z_reification(?S, ?P, ?O, ?G) is nondet.
%! z_reification(?S, ?P, ?O, ?G, -Stmt) is nondet.

z_reification(S, P, O) :-
  z_reification(S, P, O, _).


z_reification(S, P, O, G) :-
  z_reification(S, P, O, G, _).


z_reification(S, P, O, G, Stmt) :-
  z(Stmt, rdf:subject, S, G),
  z(Stmt, rdf:predicate, P, G),
  z(Stmt, rdf:object, O, G).



%! z_triple(?Triple) is nondet.
%! z_triple(?G, ?Triple) is nondet.
%! z_triple(?S, ?P, ?O, ?Triple) is nondet.
%! z_triple(?S, ?P, ?O, ?G, ?Triple) is nondet.

z_triple(Triple) :-
  z_triple(_, Triple).


z_triple(G, Triple) :-
  z_triple(_, _, _, G, Triple).


z_triple(S, P, O, Triple) :-
  z_triple(S, P, O, _, Triple).


z_triple(S, P, O, G, Triple) :-
  rdf(S, P, O, G),
  Triple = rdf(S, P, O).



%! z_triple_datatype(+Triple, -D) is det.
%! z_triple_iri(+Triple, -Iri) is nondet.
%! z_triple_object(+Triple, -O) is det.
%! z_triple_predicate(+Triple, -P) is det.
%! z_triple_subject(+Triple, -S) is det.
%! z_triple_term(+Triple, -Term) is nondet.

z_triple_datatype(rdf(_,_,O), D) :-
  z_literal_datatype(O, D).


z_triple_iri(Triple, Iri) :-
  z_triple_term(Triple, Iri),
  rdf_is_iri(Iri).


z_triple_object(rdf(_,_,O,_), O).


z_triple_predicate(rdf(_,P,_,_), P).


z_triple_subject(rdf(S,_,_,_), S).


z_triple_term(rdf(S,_,_), S).
z_triple_term(rdf(_,P,_), P).
z_triple_term(rdf(_,_,O), O).



%! z_triple_terms(?Triple, ?S, ?P, ?O) is det.

z_triple_terms(rdf(S,P,O), S, P, O).



%! z_triples(-Triple) is nondet.
%! z_triples(?G, -Triple) is nondet.
%! z_triples(?S, ?P, ?O, -Triple) is nondet.
%! z_triples(?S, ?P, ?O, ?G, -Triple) is nondet.

z_triples(Triples) :-
  z_triples(_, Triples).


z_triples(G, Triples) :-
  z_triples(_, _, _, G, Triples).


z_triples(S, P, O, Triples) :-
  z_triples(S, P, O, _, Triples).


z_triples(S, P, O, G, Triples) :-
  aggregate_all(set(Triple), z_triple(S, P, O, G, Triple), Triples).





% HELPERS %

%! graph_file(+G, -File) is det.

graph_file(G, File) :-
  rdf_global_id(Alias:Local, G),
  directory_file_name(Alias, Local, Base),
  file_name_extension(Base, nt, File).
