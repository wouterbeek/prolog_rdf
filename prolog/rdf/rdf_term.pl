:- module(
  rdf_term,
  [
    rdf_bnode/2,             % ?B, ?G
    rdf_datatype_iri/1,      % ?D
    rdf_datatype_iri/2,      % ?D, ?G
    rdf_iri/2,               % ?Iri, ?G
    rdf_is_lts/1,            % @Term
    rdf_is_legacy_literal/1, % @Term
    rdf_lts/1,               % ?Lit
    rdf_lts/2,               % ?Lit, ?G
    rdf_legacy_literal/4,    % +Lit, -D, -Lex, -LTag
    rdf_literal/2,           % ?Lit, ?G
    rdf_literal/4,           % ?Lit, ?D, ?Lex, ?LTag
    rdf_literal_datatype/2,  % +Lit, ?D
    rdf_literal_lex/2,       % +Lit, ?Lex
    rdf_literal_value/2,     % +Lit, ?V
    rdf_lone_bnode/1,        % ?B
    rdf_name/2,              % ?Name, ?G
    rdf_node/2,              % ?Node, ?G
    rdf_object/2,            % ?O, ?G
    rdf_predicate/2,         % ?P, ?G
    rdf_subject/2,           % ?S, ?G
    rdf_term/2               % ?Term, ?G
  ]
).
:- reexport(library(semweb/rdf11), [
     rdf_is_bnode/1,     % @Term
     rdf_is_iri/1,       % @Term
     rdf_is_literal/1,   % @Term
     rdf_is_name/1,      % @Term
     rdf_is_object/1,    % @Term
     rdf_is_predicate/1, % @Term
     rdf_is_subject/1,   % @Term
     rdf_is_term/1,      % @Term
     rdf_bnode/1,        % ?B
     rdf_iri/1,          % ?Iri
     rdf_literal/1,      % ?Lit
     rdf_name/1,         % ?Name
     rdf_node/1,         % ?Node
     rdf_object/1,       % ?O
     rdf_predicate/1,    % ?P
     rdf_subject/1,      % ?S
     rdf_term/1          % ?Term
   ]).

/** <module> RDF: Terms

Support for RDF 1.1 terms.

### rdf_is_resource/1

Semweb's rdf_is_resource/1 is quite misleading.
A first mistake one may make is to think that this predicate is about
semantics (resources being objects) while it actually is about syntax
(RDF terms that are either IRIs or blank nodes).
A second mistake one may make is to assume that rdf_is_resource/1 will
succeed for precisely those syntactic constructs that have a resource as
their interpretation.
But this is not the case either, since typed literals are mapped onto
resources as well.

---

@author Wouter Beek
@compat RDF 1.1 Concepts and Abstract Syntax
@see http://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/
@version 2015/07-2015/08, 2015/10, 2015/12-2016/03, 2016/05
*/

:- use_module(library(semweb/rdf11)).
:- use_module(library(solution_sequences)).
:- use_module(library(typecheck)).

:- rdf_meta
   rdf_datatype_iri(r),
   rdf_datatype_iri(r, r),
   rdf_iri(r, r),
   rdf_legacy_literal(o, r, -, -),
   rdf_literal(o, r),
   rdf_literal(o, r, ?, ?),
   rdf_literal_datatype(o, r),
   rdf_literal_lex(o, ?),
   rdf_literal_value(o, ?),
   rdf_name(o, r),
   rdf_node(o, r),
   rdf_subject(r, r),
   rdf_object(o, r),
   rdf_predicate(r, r),
   rdf_term(o, r).

:- multifile
    error:has_type/2.

error:has_type(rdf_bnode, B) :-
  rdf_is_bnode(B).
error:has_type(rdf_graph, G) :-
  (   G == default
  ;   error:has_type(iri, G)
  ).
error:has_type(rdf_literal, Lit) :-
  rdf_is_literal(Lit).
error:has_type(rdf_name, Name) :-
  (   error:has_type(iri, Name)
  ;   error:has_type(rdf_literal, Name)
  ).
error:has_type(rdf_tuple, Tuple) :-
  (   error:has_type(rdf_triple, Tuple)
  ;   error:has_type(rdf_quad, Tuple)
  ).
error:has_type(rdf_quad, Quad) :-
  Quad = rdf(S,P,O,G),
  error:has_type(rdf_term, S),
  error:has_type(iri, P),
  error:has_type(rdf_term, O),
  error:has_type(iri, G).
error:has_type(rdf_term, Term) :-
  (   error:has_type(rdf_bnode, Term)
  ;   error:has_type(rdf_literal, Term)
  ;   error:has_type(iri, Term)
  ).
error:has_type(rdf_triple, Triple) :-
  Triple = rdf(S,P,O),
  error:has_type(rdf_term, S),
  error:has_type(iri, P),
  error:has_type(rdf_term, O).





%! rdf_bnode(?B, ?G) is nondet.

rdf_bnode(B, G) :-
  rdf_bnode(B),
  once(rdf_node(B, G)).



%! rdf_datatype_iri(+D) is semidet.
%! rdf_datatype_iri(-D) is nondet.
%! rdf_datatype_iri(?D, ?G) is nondet.

rdf_datatype_iri(D) :-
  distinct(D, (rdf_literal(Lit), rdf_literal_datatype(Lit, D))).


rdf_datatype_iri(D, G) :-
  distinct(D, (rdf_literal(Lit, G), rdf_literal_datatype(Lit, D))).



%! rdf_iri(?Iri, ?G) is nondet.

rdf_iri(Iri, G) :-
  rdf_iri(Iri),
  once(rdf_term(Iri, G)).



%! rdf_is_lts(@Term) is semidet.

rdf_is_lts(Term) :-
  ground(Term),
  Term = _@_.



%! rdf_lts(+Lit) is semidet.
%! rdf_lts(-Lit) is nondet.
% The **language-tagged string**s are the cartesian product of the Unicode
% strings in Normal Form C with the set of BCP 47 language tags.

rdf_lts(Lit) :-
  rdf_literal(Lit),
  Lit = _@_.



%! rdf_lts(?Lit, ?G) is nondet.

rdf_lts(Lit, G) :-
  rdf_literal(Lit, G),
  Lit = _@_.



%! rdf_is_legacy_literal(@Term) is semidet.

rdf_is_legacy_literal(literal(type(_,_))) :- !.
rdf_is_legacy_literal(literal(lang(_,_))) :- !.
rdf_is_legacy_literal(literal(_)).



%! rdf_legacy_literal(+Lit, -D, -Lex, -LTag) is det.

rdf_legacy_literal(literal(type(D,Lex0)), D, Lex, _) :- !,
  atom_string(Lex0, Lex).
rdf_legacy_literal(literal(lang(LTag,Lex0)), rdf:langString, Lex, LTag) :- !,
  atom_string(Lex0, Lex).
rdf_legacy_literal(literal(Lex0), xsd:string, Lex, _) :-
  atom_string(Lex0, Lex).



%! rdf_literal(?Lit, ?G) is nondet.

rdf_literal(G, Lit) :-
  rdf_literal(Lit),
  once(rdf_node(G, Lit)).



%! rdf_literal(+Lit, -D, -Lex, -LTag) is det.

rdf_literal(Lit, D, Lex, LTag) :-
  var(Lit), !,
  rdf_legacy_literal(Lit0, D, Lex, LTag),
  rdf11:post_object(Lit, Lit0).
rdf_literal(V^^D, D, Lex, _) :- !,
  rdf_literal_lex(V^^D, Lex).
rdf_literal(V@LTag, rdf:langString, Lex, LTag) :-
  rdf_literal_lex(V@LTag, Lex).



%! rdf_literal_datatype(+Lit, +D) is semidet.
%! rdf_literal_datatype(+Lit, -D) is det.

rdf_literal_datatype(_^^D, D).
rdf_literal_datatype(_@_, D):- rdf_equal(rdf:langString, D).



%! rdf_literal_lex(+Lit, +Lex) is semidet.
%! rdf_literal_lex(+Lit, -Lex) is det.

rdf_literal_lex(V^^D, Lex) :- !,
  rdf_lexical_form(V^^D, Lex^^D).
rdf_literal_lex(V@_, V).



%! rdf_literal_value(+Lit, +V) is semidet.
%! rdf_literal_value(+Lit, -V) is nondet.

rdf_literal_value(V^^_, V).
rdf_literal_value(V@_, V).



%! rdf_lone_bnode(+B) is semidet.
%! rdf_lone_bnode(-B) is nondet.

rdf_lone_bnode(B) :-
  rdf(_, _, B),
  rdf_is_bnode(B),
  \+ rdf(B, _, _).



%! rdf_name(?Name, ?G) is nondet.

rdf_name(Name, G) :-
  rdf_term(Name, G),
  \+ rdf_is_bnode(Name).



%! rdf_node(?Node, ?G) is nondet.

rdf_node(S, G) :-
  rdf_subject(S, G).
rdf_node(O, G) :-
  rdf_object(O, G),
  % Make sure there are no duplicates.
  \+ rdf_subject(O, G).



%! rdf_object(?O, ?G) is nondet.

rdf_object(O, G) :-
  var(O), !,
  rdf_object(O),
  distinct(G, rdf(_, _, O, G)).
rdf_object(O, G) :-
  distinct(G, rdf(_, _, O, G)).



%! rdf_predicate(?P, ?G) is nondet.

rdf_predicate(P, G) :-
  var(P), !,
  rdf_predicate(P),
  distinct(G, rdf(_, P, _, G)).
rdf_predicate(P, G) :-
  distinct(G, rdf(_, P, _, G)).



%! rdf_subject(?G, ?S) is nondet.

rdf_subject(S, G) :-
  var(S), !,
  rdf_subject(S),
  distinct(G, rdf(S, _, _, G)).
rdf_subject(G, S) :-
  distinct(G, rdf(S, _, _, G)).



%! rdf_term(?Term, ?G) is nondet.

rdf_term(P, G) :-
  rdf_predicate(P, G).
rdf_term(Node, G) :-
  rdf_node(Node, G),
  % Ensure there are no duplicates.
  \+ rdf_predicate(Node).
