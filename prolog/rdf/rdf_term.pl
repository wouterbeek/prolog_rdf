:- module(
  rdf_term,
  [
    rdf_bnode/2,                     % +G, ?B
    rdf_datatype_iri/1,              % ?D
    rdf_datatype_iri/2,              % +G, ?D
    rdf_iri/2,                       % +G, ?Iri
    rdf_is_language_tagged_string/1, % @Term
    rdf_is_legacy_literal/1,         % @Term
    rdf_language_tagged_string/1,    % ?Lit
    rdf_language_tagged_string/2,    % +G, ?Lit
    rdf_legacy_literal_components/4, % +Lit, -D, -Lex, -LTag) is det.
    rdf_literal/2,                   % +G, ?Lit
    rdf_literal_components/4,        % ?Lit, ?D, ?Lex, ?LTag
    rdf_literal_datatype/2,          % +Lit, ?D
    rdf_literal_lexical_form/2,      % +Lit, ?Lex
    rdf_literal_value/2,             % +Lit, ?V
    rdf_name/2,                      % +G, ?Name
    rdf_node/2,                      % +G, ?Node
    rdf_object/2,                    % +G, ?O
    rdf_predicate/2,                 % +G, ?P
    rdf_subject/2,                   % +G, ?S
    rdf_term/2                       % +G, ?Term
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
@version 2015/07-2015/08, 2015/10, 2015/12-2016/03
*/

:- use_module(library(semweb/rdf11)).
:- use_module(library(solution_sequences)).
:- use_module(library(typecheck)).

:- rdf_meta
   rdf_datatype_iri(r),
   rdf_datatype_iri(r, r),
   rdf_iri(r, r),
   rdf_legacy_literal_components(o, r, -, -),
   rdf_literal(r, o),
   rdf_literal_components(o, r, ?, ?),
   rdf_literal_datatype(o, r),
   rdf_literal_lexical_form(o, ?),
   rdf_literal_value(o, ?),
   rdf_name(r, o),
   rdf_node(r, o),
   rdf_subject(r, r),
   rdf_object(r, o),
   rdf_predicate(r, r),
   rdf_term(r, o).

:- multifile(error:has_type/2).
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





%! rdf_bnode(+G, +B) is semidet.
%! rdf_bnode(+G, -B) is nondet.

rdf_bnode(G, B) :-
  rdf_bnode(B),
  once(rdf_node(G, B)).



%! rdf_datatype_iri(+D) is semidet.
%! rdf_datatype_iri(-D) is nondet.

rdf_datatype_iri(D) :-
  distinct(D, (rdf_literal(Lit), rdf_literal_datatype(Lit, D))).


%! rdf_datatype_iri(+G, +D) is semidet.
%! rdf_datatype_iri(+G, -D) is nondet.

rdf_datatype_iri(G, D) :-
  distinct(D, (rdf_literal(G, Lit), rdf_literal_datatype(Lit, D))).



%! rdf_iri(+G, +Iri) is semidet.
%! rdf_iri(+G, -Iri) is nondet.

rdf_iri(G, Iri) :-
  rdf_iri(Iri),
  once(rdf_term(G, Iri)).



%! rdf_is_language_tagged_string(@Term) is semidet.

rdf_is_language_tagged_string(Term) :-
  ground(Term),
  Term = _@_.



%! rdf_language_tagged_string(+Lit) is semidet.
%! rdf_language_tagged_string(-Lit) is nondet.
% The **language-tagged string**s are the cartesian product of the Unicode
% strings in Normal Form C with the set of BCP 47 language tags.

rdf_language_tagged_string(Lit) :-
  rdf_literal(Lit),
  Lit = _@_.


%! rdf_language_tagged_string(+G, +Lit) is semidet.
%! rdf_language_tagged_string(+G, -Lit) is nondet.

rdf_language_tagged_string(G, Lit) :-
  rdf_literal(G, Lit),
  Lit = _@_.



%! rdf_is_legacy_literal(@Term) is semidet.

rdf_is_legacy_literal(literal(type(_,_))) :- !.
rdf_is_legacy_literal(literal(lang(_,_))) :- !.
rdf_is_legacy_literal(literal(_)).



%! rdf_legacy_literal_components(+Literal, -D, -Lex, -LTag) is det.

rdf_legacy_literal_components(literal(type(D,Lex)), D, Lex, _) :- !.
rdf_legacy_literal_components(literal(lang(LTag,Lex)), rdf:langString, Lex, LTag) :- !.
rdf_legacy_literal_components(literal(Lex), xsd:string, Lex, _).



%! rdf_literal(+G, +Lit) is semidet.
%! rdf_literal(+G, -Lit) is nondet.

rdf_literal(G, Lit) :-
  rdf_literal(Lit),
  once(rdf_node(G, Lit)).



%! rdf_literal_components(?Lit, ?D, ?Lex, ?LTag) .

rdf_literal_components(V^^D, D, Lex, _) :- !,
  rdf_literal_lexical_form(V^^D, Lex).
rdf_literal_components(V@LTag, rdf:langString, Lex, LTag) :-
  rdf_literal_lexical_form(V@LTag, Lex).


%! rdf_literal_datatype(+Lit, +D) is semidet.
%! rdf_literal_datatype(+Lit, -D) is det.

rdf_literal_datatype(_^^D, D).
rdf_literal_datatype(_@_, D):- rdf_equal(rdf:langString, D).



%! rdf_literal_lexical_form(+Lit, +Lex) is semidet.
%! rdf_literal_lexical_form(+Lit, -Lex) is det.

rdf_literal_lexical_form(V^^D, Lex) :- !,
  rdf11:in_type(D, V, Lex).
rdf_literal_lexical_form(V@_, Lex) :-
  atom_string(Lex, V).



%! rdf_literal_value(+Lit, +V) is semidet.
%! rdf_literal_value(+Lit, -V) is nondet.

rdf_literal_value(V^^_, V).
rdf_literal_value(V@_, V).



%! rdf_name(+G, +Name) is semidet.
%! rdf_name(+G, -Name) is nondet.

rdf_name(G, Name) :-
  rdf_term(G, Name),
  \+ rdf_is_bnode(Name).



%! rdf_node(+G, +Node) is semidet.
%! rdf_node(+G, -Node) is nondet.

rdf_node(G, S) :-
  rdf_subject(G, S).
rdf_node(G, O) :-
  rdf_object(G, O),
  % Make sure there are no duplicates.
  \+ rdf_subject(G, O).



%! rdf_object(+G, +O) is semidet.
%! rdf_object(+G, -O) is nondet.

rdf_object(G, O) :-
  var(O), !,
  rdf_object(O),
  once(rdf(_, _, O, G)).
rdf_object(G, O) :-
  rdf(_, _, O, G).



%! rdf_predicate(+G, +P) is semidet.
%! rdf_predicate(+G, -P) is nondet.

rdf_predicate(G, P) :-
  var(P), !,
  rdf_predicate(P),
  once(rdf(_, P, _, G)).
rdf_predicate(G, P) :-
  rdf(_, P, _, G).



%! rdf_subject(+G, +S) is semidet.
%! rdf_subject(+G, -S) is nondet.

rdf_subject(G, S) :-
  var(S), !,
  rdf_subject(S),
  rdf(S, _, _, G).
rdf_subject(G, S) :-
  rdf(S, _, _, G).



%! rdf_term(+G, +Term) is semidet.
%! rdf_term(+G, -Term) is nondet.

rdf_term(G, P) :-
  rdf_predicate(G, P).
rdf_term(G, Node) :-
  rdf_node(G, Node),
  % Ensure there are no duplicates.
  \+ rdf_predicate(Node).
