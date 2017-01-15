:- module(
  rdf_api,
  [
    t/3,                     % ?M, -Triple, ?G
    t/4,                     % ?M, ?S, ?P, ?O
    t/5,                     % ?M, ?S, ?P, ?O, ?G
    rdf_is_def_quad/1,       % @Term
    rdf_is_ground_quad/1,    % @Term
    rdf_is_ground_triple/1,  % @Term
    rdf_is_quad/1,           % @Term
    rdf_is_triple/1,         % @Term
    rdf_last/4,              % ?M, ?L,  ?O,  ?G
    rdf_last/5,              % ?M, ?S,  ?P,  ?O, ?G
    rdf_lexical_form/4,      % ?M, ?S, ?P, -Lex
    rdf_lexical_form/5,      % ?M, ?S, ?P, -Lex, ?G
    rdf_list/3,              % ?M, ?L,  ?G
    rdf_list/4,              % ?M, ?L1, -L2, ?G
    rdf_list/5,              % ?M, ?S,  ?P,  ?L, ?G
    rdf_list/6,              % ?M, ?S,  ?P,  ?L1, -L2, ?G
    rdf_list_member/4,       % ?M, ?L,  ?O,  ?G
    rdf_list_member/5,       % ?M, ?S,  ?P,  ?O, ?G
    rdf_lts/4,               % ?M, ?S, ?P, ?Lit
    rdf_lts/5,               % ?M, ?S, ?P, ?Lit, ?G
    rdf_lts/6,               % ?M, ?S, ?P, +LRange, ?Lit, -G
    rdf_maximal_list/3,      % ?M, ?L,  ?G
    rdf_maximal_list/4,      % ?M, ?L1, ?L2, ?G
    rdf_maximal_list/5,      % ?M, ?S,  ?P,  ?L, ?G
    rdf_maximal_list/6,      % ?M, ?S,  ?P,  ?L1, ?L2, ?G
    rdf_quad/2,              % ?M, -Quad
    rdf_quad/3,              % ?M, ?G, -Quad
    rdf_quad/4,              % ?M, ?S, ?G, -Quad
    rdf_quad/5,              % ?M, ?S, ?P, ?O, -Quad
    rdf_quad/6,              % ?M, ?S, ?P, ?O, ?G, -Quad
    rdf_quad_datatype/2,     % +Quad, -D
    rdf_quad_graph/2,        % +Quad, -G
    rdf_quad_graph_triple/3, % ?Quad, ?G, ?Triple
    rdf_quad_iri/2,          % +Quad, -Iri
    rdf_quad_object/2,       % +Quad, -O
    rdf_quad_predicate/2,    % +Quad, -P
    rdf_quad_subject/2,      % +Quad, -S
    rdf_quad_term/2,         % +Quad, -Term
    rdf_quad_terms/5,        % ?Quad, ?S, ?P, ?O, ?G
    rdf_quad_triple/2,       % ?Quad, ?Triple
    rdf_quads/2,             % ?M, -Quads
    rdf_quads/3,             % ?M, ?G, -Quads
    rdf_quads/4,             % ?M, ?S, ?G, -Quads
    rdf_quads/5,             % ?M, ?S, ?P, ?O, -Quads
    rdf_quads/6,             % ?M, ?S, ?P, ?O, ?G, -Quads
    rdf_reification/4,       % ?M, ?S, ?P, ?O
    rdf_reification/5,       % ?M, ?S, ?P, ?O, ?G
    rdf_reification/6,       % ?M, ?S, ?P, ?O, ?G, -Stmt
    rdf_tuple_triple/2,      % +Tuple, -Triple
    rdf_triple/2,            % ?M, -Triple
    rdf_triple/3,            % ?M, ?G, -Triple
    rdf_triple/4,            % ?M, ?S, ?G, -Triple
    rdf_triple/5,            % ?M, ?S, ?P, ?O, -Triple
    rdf_triple/6,            % ?M, ?S, ?P, ?O, ?G, -Triple
    rdf_triple_datatype/2,   % +Triple, -D
    rdf_triple_iri/2,        % +Triple, -Iri
    rdf_triple_object/2,     % +Triple, -O
    rdf_triple_predicate/2,  % +Triple, -P
    rdf_triple_row/2,        % ?Triple, ?Row
    rdf_triple_subject/2,    % +Triple, -S
    rdf_triple_term/2,       % +Triple, -Term
    rdf_triple_terms/4,      % ?Triple, ?S, ?P, ?O
    rdf_triples/2,           % ?M, -Triples
    rdf_triples/3,           % ?M, ?G, -Triples
    rdf_triples/4,           % ?M, ?S, ?G, -Triples
    rdf_triples/5,           % ?M, ?S, ?P, ?O, -Triples
    rdf_triples/6            % ?M, ?S, ?P, ?O, ?G, -Triples
  ]
).
:- reexport(library(rdf/rdf_term)).

/** <module> RDF API

API at the RDF level of expressivity.

@author Wouter Beek
@version 2016/06-2017/01
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(hdt/hdt_ext)).
:- use_module(library(lists)).
:- use_module(library(ltag/ltag_match)).
:- use_module(library(nlp/nlp_lang)).
:- use_module(library(tapir/tapir), []).

:- multifile
    error:has_type/2.

error:has_type(rdf_quad, Quad) :-
  Quad = rdf(S,P,O,G),
  (error:has_type(rdf_bnode, S) ; error:has_type(rdf_iri, S)),
  error:has_type(q, P),
  error:has_type(rdf_term, O),
  error:has_type(rdf_graph, G).
error:has_type(rdf_triple, Triple) :-
  Triple = rdf(S,P,O),
  (error:has_type(rdf_bnode, S) ; error:has_type(rdf_iri, S)),
  error:has_type(rdf_iri, P),
  error:has_type(rdf_term, O).
error:has_type(rdf_tuple, Triple) :-
  error:has_type(rdf_triple, Triple).
error:has_type(rdf_tuple, Quad) :-
  error:has_type(rdf_quad, Quad).

:- rdf_meta
   t(?, -, r),
   t(?, r, r, o),
   t(?, r, r, o, r),
   rdf_last(?, r, o, r),
   rdf_last(?, r, r, o, r),
   rdf_lexical_form(+, r, r, -),
   rdf_lexical_form(+, r, r, -, r),
   rdf_list(?, r, r),
   rdf_list(?, r, -, r),
   rdf_list(?, r, r, r, r),
   rdf_list(?, r, r, r, -, r),
   rdf_list_member(?, r, o, r),
   rdf_list_member(?, r, r, o, r),
   rdf_lts(+, r, r, -),
   rdf_lts(+, r, r, r, -),
   rdf_lts(+, r, r, +, r, -),
   rdf_maximal_list(?, r, r),
   rdf_maximal_list(?, r, -, r),
   rdf_maximal_list(?, r, r, r, r),
   rdf_maximal_list(?, r, r, r, -, r),
   rdf_quad(+, r, -),
   rdf_quad(+, r, r, -),
   rdf_quad(+, r, r, o, -),
   rdf_quad(+, r, r, o, r, -),
   rdf_quad_datatype(t, r),
   rdf_quad_graph(t, r),
   rdf_quad_graph_triple(t, r, t),
   rdf_quad_iri(t, r),
   rdf_quad_object(t, o),
   rdf_quad_predicate(t, r),
   rdf_quad_subject(t, r),
   rdf_quad_term(t, o),
   rdf_quad_terms(t, r, r, o, r),
   rdf_quad_triple(t, t),
   rdf_quads(+, r, -),
   rdf_quads(+, r, r, -),
   rdf_quads(+, r, r, o, -),
   rdf_quads(+, r, r, o, r, -),
   rdf_reification(+, r, r, o),
   rdf_reification(+, r, r, o, r),
   rdf_reification(+, r, r, o, r, r),
   rdf_tuple_triple(t, t),
   rdf_triple(+, r, -),
   rdf_triple(+, r, r, -),
   rdf_triple(+, r, r, o, -),
   rdf_triple(+, r, r, o, r, -),
   rdf_triple_datatype(t, r),
   rdf_triple_iri(t, r),
   rdf_triple_object(t, o),
   rdf_triple_predicate(t, r),
   rdf_triple_subject(t, r),
   rdf_triple_term(t, o),
   rdf_triple_terms(t, r, r, o),
   rdf_triples(+, r, -),
   rdf_triples(+, r, r, -),
   rdf_triples(+, r, r, o, -),
   rdf_triples(+, r, r, o, r, -).




%! t(?M, -Triple, ?G) is nondet.
%! t(?M, ?S, ?P, ?O) is nondet.
%! t(?M, ?S, ?P, ?O, ?G) is nondet.
%! t(?M, ?S, ?P, ?O, ?G, ?D) is nondet.

t(M, rdf(S,P,O), G) :-
  t(M, S, P, O, G).


t(M, S, P, O) :-
  t(M, S, P, O, _).


t(any, S, P, O, G) :-
  t(_, S, P, O, G).
t(hdt0, S, P, O, Hdt) :-
  hdt0(S, P, O, Hdt).
t(trp, S, P, O, G) :-
  rdf11:rdf(S, P, O, G).
t(tapir(User,Dataset), S, P, O, G) :-
  tapir:tpf(User, Dataset, rdf(S,P,O,G)).


%! rdf_is_def_quad(@Term) is semidet.
%
% Succeeds if Term is a quadruple with default graph.

rdf_is_def_quad(rdf(_,_,_,G)) :-
  rdf_default_graph(G).



%! rdf_is_ground_quad(@Term) is semidet.
%
% Succeeds if the given triple is ground, i.e., contains no blank
% node.

rdf_is_ground_quad(rdf(S,P,O,_)) :-
  rdf_is_ground_triple(rdf(S,P,O)).



%! rdf_is_ground_triple(@Term) is semidet.
%
% Succeeds if the given triple is ground, i.e., contains no blank
% node.

rdf_is_ground_triple(rdf(S,_,O)) :-
  \+ rdf_is_bnode(S),
  \+ rdf_is_bnode(O).



%! rdf_is_quad(@Term) is semidet.

rdf_is_quad(rdf(_,_,_,_)).



%! rdf_is_triple(@Term) is semidet.

rdf_is_triple(rdf(_,_,_)).



%! rdf_last(?M, ?L, ?O, ?G) is nondet.
%! rdf_last(?M, ?S, ?P, ?O, ?G) is nondet.
%
% O is the last item in an RDF list.

rdf_last(M, L, O, G) :-
  q(M, L, rdf:rest, T, G),
  (   rdf_equal(T, rdf:nil)
  ->  q(M, L, rdf:first, O, G)
  ;   rdf_last(M, T, O, G)
  ).


rdf_last(M, S, P, O, G) :-
  rdf_list(M, S, P, L, G),
  rdf_last(M, L, O, G).



%! rdf_lexical_form(?M, ?S, ?P, -Lex) is nondet.
%! rdf_lexical_form(?M, ?S, ?P, -Lex, ?G) is nondet.

rdf_lexical_form(M, S, P, Lex) :-
  rdf_lexical_form(M, S, P, Lex, _).


rdf_lexical_form(M, S, P, Lex, G) :-
  q(M, S, P, Lit, G),
  rdf_is_literal(Lit),
  rdf_literal_lexical_form(Lit, Lex).



%! rdf_list(?M, ?L, ?G) is nondet.
%! rdf_list(?M, ?L1, -L2, ?G) is nondet.
%! rdf_list(?M, ?S, ?P, ?L, ?G) is nondet.
%! rdf_list(?M, ?S, ?P, ?L1, -L2, ?G) is nondet.
%
% Enumerates lists L that are not (strict) sublists.

rdf_list(M, L1, G) :-
  rdf_list_first(M, L1, L2, G),
  rdf_list_rest(M, L2, G).


rdf_list(M, L1, [H|T], G) :-
  rdf_list_first(M, L1, H, L2, G),
  rdf_list_rest(M, L2, T, G).


rdf_list(M, S, P, L, G) :-
  q(M, S, P, L, G),
  rdf_list(M, L, G).


rdf_list(M, S, P, L1, L2, G) :-
  q(M, S, P, L1, G),
  rdf_list(M, L1, L2, G).



%! rdf_list_member(?M, ?L, ?O, ?G) is nondet.
%! rdf_list_member(?M, ?S, ?P, ?O, ?G) is nondet.

rdf_list_member(M, L, O, G) :-
  t(M, L, rdf:first, O, G).
rdf_list_member(M, L, O, G) :-
  t(M, L, rdf:rest, T, G),
  rdf_list_member(M, T, O, G).



rdf_list_member(M, S, P, O, G) :-
  ground(O), !,
  rdf_list_member(M, L, O, G),
  t(M, S, P, L, G).
rdf_list_member(M, S, P, O, G) :-
  t(M, S, P, L, G),
  rdf_list_member(M, L, O, G).



%! rdf_lts(?M, ?S, ?P, -Lit) is nondet.
%! rdf_lts(?M, ?S, ?P, -Lit, ?G) is nondet.
%! rdf_lts(?M, ?S, ?P, +LRange, -Lit, ?G) is nondet.
%
% Matches RDF statements whose object term is a language-tagged string
% that mathes the given language priory list.  Notice that results for
% each of the prioritized languages are given in arbitrary order.

rdf_lts(M, S, P, Lit) :-
  rdf_lts(M, S, P, Lit, _).


rdf_lts(M, S, P, Lit, G) :-
  current_lrange(LRange),
  rdf_lts(M, S, P, LRange, Lit, G).


rdf_lts(M, S, P, LRange, Lit, G) :-
  q(M, S, P, V@LTag, G),
  basic_filtering(LRange, LTag),
  Lit = V@LTag.



%! rdf_maximal_list(?M, ?L,  ?G) is nondet.
%! rdf_maximal_list(?M, ?L1, ?L2, ?G) is nondet.
%! rdf_maximal_list(?M, ?S,  ?P,  ?L, ?G) is nondet.
%! rdf_maximal_list(?M, ?S,  ?P,  ?L1, ?L2, ?G) is nondet.

rdf_maximal_list(M, L1, G) :-
  rdf_list_first0(M, L1, L2, G),
  \+ t(M, _, rdf:rest, L1, G),
  rdf_list_rest0(M, L2, G).


rdf_maximal_list(M, L1, [H|T], G) :-
  rdf_list_first0(M, L1, H, L2, G),
  \+ t(M, _, rdf:rest, L1, G),
  rdf_list_rest0(M, L2, T, G).


rdf_maximal_list(M, S, P, L, G) :-
  t(M, S, P, L, G),
  rdf_maximal_list(M, L, G).


rdf_maximal_list(M, S, P, L1, L2, G) :-
  t(M, S, P, L1, G),
  rdf_maximal_list(M, L1, L2, G).

rdf_list_first0(M, L1, L2, G) :-
  once(t(M, L1, rdf:first, _, G)),
  t(M, L1, rdf:rest, L2, G).

rdf_list_first0(M, L1, H, L2, G) :-
  t(M, L1, rdf:first, H, G),
  t(M, L1, rdf:rest, L2, G).

rdf_list_rest0(M, L1, G) :-
  rdf_list_first0(M, L1, L2, G),
  rdf_list_rest0(M, L2, G).
% @tbd RDF prefix expansion does not work.
rdf_list_rest0(_, L, _) :-
  rdf_equal(rdf:nil, L).

rdf_list_rest0(M, L1, [H|T], G) :-
  rdf_list_first0(M, L1, H, L2, G),
  rdf_list_rest0(M, L2, T, G).
% @tbd RDF prefix expansion does not work.
rdf_list_rest0(_, L, [], _) :-
  rdf_equal(rdf:nil, L).



%! rdf_quad(?M, -Quad) is nondet.
%! rdf_quad(?M, ?G, -Quad) is nondet.
%! rdf_quad(?M, ?S, ?G, -Quad) is nondet.
%! rdf_quad(?M, ?S, ?P, ?O, -Quad) is nondet.
%! rdf_quad(?M, ?S, ?P, ?O, ?G, -Quad) is nondet.

rdf_quad(M, Quad) :-
  rdf_quad(M, _, Quad).


rdf_quad(M, G, Quad) :-
  rdf_quad(M, _, _, _, G, Quad).


rdf_quad(M, S, G, Quad) :-
  rdf_quad(M, S, _, _, G, Quad).


rdf_quad(M, S, P, O, Quad) :-
  rdf_quad(M, S, P, O, _, Quad).


rdf_quad(M, S, P, O, G, Quad) :-
  q(M, S, P, O, G),
  Quad = rdf(S, P, O, G).



%! rdf_quad_datatype(+Quad, -D) is det.

rdf_quad_datatype(rdf(_,_,O,_), D) :-
  rdf_literal_datatype(O, D).



%! rdf_quad_graph(+Quad, -G) is det.

rdf_quad_graph(rdf(_,_,_,G), G).



%! rdf_quad_graph_triple(+Quad, -G, -Triple) is det.
%! rdf_quad_graph_triple(-Quad, +G, +Triple) is det.

rdf_quad_graph_triple(rdf(S,P,O,G), G, rdf(S,P,O)).



%! rdf_quad_iri(+Quad, -Iri) is multi.

rdf_quad_iri(Quad, Iri) :-
  rdf_quad_term(Quad, Iri),
  rdf_is_iri(Iri).



%! rdf_quad_object(+Quad, -O) is det.

rdf_quad_object(rdf(_,_,O,_), O).



%! rdf_quad_predicate(+Quad, -P) is det.

rdf_quad_predicate(rdf(_,P,_,_), P).



%! rdf_quad_subject(+Quad, -S) is det.

rdf_quad_subject(rdf(S,_,_,_), S).



%! rdf_quad_term(+Quad, -Term) is multi.

rdf_quad_term(rdf(S,_,_,_), S).
rdf_quad_term(rdf(_,P,_,_), P).
rdf_quad_term(rdf(_,_,O,_), O).
rdf_quad_term(rdf(_,_,_,G), G).



%! rdf_quad_terms(?Quad, ?S, ?P, ?O, ?G) is det.

rdf_quad_terms(rdf(S,P,O,G), S, P, O, G).



%! rdf_quad_triple(+Quad, -Triple) is det.

rdf_quad_triple(rdf(S,P,O,_), rdf(S,P,O)).



%! rdf_quads(?M, -Quads) is nondet.
%! rdf_quads(?M, ?G, -Quads) is nondet.
%! rdf_quads(?M, ?S, ?G, -Quads) is nondet.
%! rdf_quads(?M, ?S, ?P, ?O, -Quads) is nondet.
%! rdf_quads(?M, ?S, ?P, ?O, ?G, -Quads) is nondet.

rdf_quads(M, Quads) :-
  rdf_quads(M, _, Quads).


rdf_quads(M, G, Quads) :-
  rdf_quads(M, _, _, _, G, Quads).


rdf_quads(M, S, G, Quads) :-
  rdf_quads(M, S, _, _, G, Quads).


rdf_quads(M, S, P, O, Quads) :-
  rdf_quads(M, S, P, O, _, Quads).


rdf_quads(M, S, P, O, G, Quads) :-
  aggregate_all(set(Quad), rdf_quad(M, S, P, O, G, Quad), Quads).



%! rdf_reification(?M, ?S, ?P, ?O) is nondet.
%! rdf_reification(?M, ?S, ?P, ?O, ?G) is nondet.
%! rdf_reification(?M, ?S, ?P, ?O, ?G, -Stmt) is nondet.

rdf_reification(M, S, P, O) :-
  rdf_reification(M, S, P, O, _).


rdf_reification(M, S, P, O, G) :-
  rdf_reification(M, S, P, O, G, _).


rdf_reification(M, S, P, O, G, Stmt) :-
  q(M, Stmt, rdf:subject, S, G),
  q(M, Stmt, rdf:predicate, P, G),
  q(M, Stmt, rdf:object, O, G).



%! rdf_tuple_triple(+Tuple, -Triple) is det.

rdf_tuple_triple(rdf(S,P,O), rdf(S,P,O)) :- !.
rdf_tuple_triple(rdf(S,P,O,_), rdf(S,P,O)).



%! rdf_triple(?M, ?Triple) is nondet.
%! rdf_triple(?M, ?G, ?Triple) is nondet.
%! rdf_triple(?M, ?S, ?G, ?Triple) is nondet.
%! rdf_triple(?M, ?S, ?P, ?O, ?Triple) is nondet.
%! rdf_triple(?M, ?S, ?P, ?O, ?G, ?Triple) is nondet.

rdf_triple(M, Triple) :-
  rdf_triple(M, _, Triple).


rdf_triple(M, G, Triple) :-
  rdf_triple(M, _, _, _, G, Triple).


rdf_triple(M, S, G, Triple) :-
  rdf_triple(M, S, _, _, G, Triple).


rdf_triple(M, S, P, O, Triple) :-
  rdf_triple(M, S, P, O, _, Triple).


rdf_triple(M, S, P, O, G, Triple) :-
  q(M, S, P, O, G),
  Triple = rdf(S, P, O).



%! rdf_triple_datatype(+Triple, -D) is det.

rdf_triple_datatype(rdf(_,_,O), D) :-
  rdf_literal_datatype(O, D).



%! rdf_triple_iri(+Triple, -Iri) is nondet.

rdf_triple_iri(Triple, Iri) :-
  rdf_triple_term(Triple, Iri),
  rdf_is_iri(Iri).



%! rdf_triple_object(+Triple, -O) is det.

rdf_triple_object(rdf(_,_,O,_), O).



%! rdf_triple_predicate(+Triple, -P) is det.

rdf_triple_predicate(rdf(_,P,_,_), P).



%! rdf_triple_row(+Triple, -Row) is det.
%! rdf_triple_row(-Triple, +Row) is det.

rdf_triple_row(rdf(S,P,O), row(S,P,O)).



%! rdf_triple_subject(+Triple, -S) is det.

rdf_triple_subject(rdf(S,_,_,_), S).



%! rdf_triple_term(+Triple, -Term) is nondet.

rdf_triple_term(rdf(S,_,_), S).
rdf_triple_term(rdf(_,P,_), P).
rdf_triple_term(rdf(_,_,O), O).



%! rdf_triple_terms(?Triple, ?S, ?P, ?O) is det.

rdf_triple_terms(rdf(S,P,O), S, P, O).



%! rdf_triples(?M, -Triple) is nondet.
%! rdf_triples(?M, ?G, -Triple) is nondet.
%! rdf_triples(?M, ?S, ?G, -Triple) is nondet.
%! rdf_triples(?M, ?S, ?P, ?O, -Triple) is nondet.
%! rdf_triples(?M, ?S, ?P, ?O, ?G, -Triple) is nondet.

rdf_triples(M, Triples) :-
  rdf_triples(M, _, Triples).


rdf_triples(M, G, Triples) :-
  rdf_triples(M, _, _, _, G, Triples).


rdf_triples(M, S, G, Triples) :-
  rdf_triples(M, S, _, _, G, Triples).


rdf_triples(M, S, P, O, Triples) :-
  rdf_triples(M, S, P, O, _, Triples).


rdf_triples(M, S, P, O, G, Triples) :-
  aggregate_all(set(Triple), rdf_triple(M, S, P, O, G, Triple), Triples).
