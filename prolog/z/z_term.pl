:- module(
  z_term,
  [
% Term transformations
    rdf_is_lts/1,            % @Term
    rdf_is_legacy_literal/1, % @Term
    rdf_legacy_literal/4,    % +Lit, -D, -Lex, -LTag
    rdf_literal/4,           % ?Lit, ?D, ?Lex, ?LTag
    rdf_literal_datatype/2,  % +Lit, ?D
    rdf_literal_lex/2,       % +Lit, ?Lex
    rdf_literal_val/2,       % +Lit, ?Val

% Term enumerations
    z_bnode/3,       % +Mode, ?B, ?G
    z_datatype/2,    % +Mode, ?D
    z_datatype/3,    % +Mod,e ?D, ?G
    z_iri/3,         % +Mode, ?Iri, ?G
    z_literal/3,     % +Mode, ?Lit, ?G
    z_lts/2,         % +Mode, ?Lit
    z_lts/3,         % +Mode, ?Lit, ?G
    z_name/3,        % +Mode, ?Name, ?G
    z_node/3,        % +Mode, ?Node, ?G
    z_object/3,      % +Mode, ?O, ?G
    z_predicate/3,   % +Mode, ?P, ?G
    z_subject/3,     % +Mode, ?S, ?G
    z_term/3         % +Mode, ?Term, ?G
  ]
).

/** <module> Z terms

@author Wouter Beek
@compat RDF 1.1 Concepts and Abstract Syntax
@see http://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/
@version 2015/07-2015/08, 2015/10, 2015/12-2016/03, 2016/05-2016/06
*/

:- use_module(library(semweb/rdf11)).
:- use_module(library(solution_sequences)).
:- use_module(library(typecheck)).





%! rdf_is_lts(@Term) is semidet.

rdf_is_lts(Term) :-
  ground(Term),
  Term = _@_.



%! rdf_is_legacy_literal(@Term) is semidet.

rdf_is_legacy_literal(literal(type(_,_))) :- !.
rdf_is_legacy_literal(literal(lang(_,_))) :- !.
rdf_is_legacy_literal(literal(_)).



%! rdf_legacy_literal(+Lit, -D, -Lex, -LTag) is det.
%! rdf_legacy_literal(-Lit, +D, +Lex, +LTag) is det.

rdf_legacy_literal(literal(type(D,Lex0)), D, Lex, _) :-
  \+ rdf_equal(rdf:langString, D), !,
  atom_string(Lex0, Lex).
rdf_legacy_literal(literal(lang(LTag,Lex0)), rdf:langString, Lex, LTag) :- !,
  atom_string(Lex0, Lex).
rdf_legacy_literal(literal(Lex0), xsd:string, Lex, _) :-
  atom_string(Lex0, Lex).



%! rdf_literal(+Lit, -D, -Lex, -LTag) is det.
%! rdf_literal(-Lit, +D, +Lex, +LTag) is det.

rdf_literal(Lit, D, Lex, LTag) :-
  var(Lit), !,
  rdf_legacy_literal(Lit0, D, Lex, LTag),
  rdf11:post_object(Lit, Lit0).
rdf_literal(Val^^D, D, Lex, _) :- !,
  rdf_literal_lex(Val^^D, Lex).
rdf_literal(Val@LTag, rdf:langString, Lex, LTag) :-
  rdf_literal_lex(Val@LTag, Lex).



%! rdf_literal_datatype(+Lit, +D) is semidet.
%! rdf_literal_datatype(+Lit, -D) is det.

rdf_literal_datatype(_^^D, D).
rdf_literal_datatype(_@_, rdf:langString).



%! rdf_literal_lex(+Lit, +Lex) is semidet.
%! rdf_literal_lex(+Lit, -Lex) is det.

rdf_literal_lex(Val^^D, Lex) :- !,
  rdf_lexical_form(Val^^D, Lex^^D).
rdf_literal_lex(Val@_, Val).



%! rdf_literal_val(+Lit, +Val) is semidet.
%! rdf_literal_val(+Lit, -Val) is nondet.

rdf_literal_val(Val^^_, Val).
rdf_literal_val(Val@_, Val).



%! z_bnode(+Mode, ?B, ?G) is nondet.

z_bnode(Mode, B, G) :-
  z_bnode(Mode, B),
  once(z_node(Mode, B, G)).



%! z_datatype(+Mode, ?D) is nondet.

z_datatype(Mode, D) :-
  distinct(D, (
    z_literal(Mode, Lit),
    z_literal_datatype(Mode, Lit, D)
  )).



%! z_datatype(+Mode, ?D, ?G) is nondet.

z_datatype(Mode, D, G) :-
  distinct(D, (
    z_literal(Mode, Lit, G),
    z_literal_datatype(Mode, Lit, D)
  )).



%! z_iri(?Iri, ?G) is nondet.

z_iri(Mode, Iri, G) :-
  rdf_iri(Mode, Iri),
  once(rdf_term(Mode, Iri, G)).



%! z_lts(+Mode, ?Lit) is nondet.
%
% The **language-tagged string**s are the cartesian product of the Unicode
% strings in Normal Form C with the set of BCP 47 language tags.

z_lts(Mode, Lit) :-
  z_literal(Mode, Lit),
  Lit = _@_.



%! z_lts(+Mode, ?Lts, ?G) is nondet.

z_lts(Mode, Lit, G) :-
  z_literal(Mode, Lit, G),
  Lit = _@_.



%! z_literal(?Lit, ?G) is nondet.

z_literal(Mode, Lit, G) :-
  z_literal(Mode, Lit),
  once(z_node(Mode, Lit, G)).



%! z_name(+Mode, ?Name, ?G) is nondet.

z_name(Mode, Name, G) :-
  z_term(Mode, Name, G),
  \+ rdf_is_bnode(Name).



%! z_node(+Mode, ?Node, ?G) is nondet.

z_node(Mode, S, G) :-
  z_subject(Mode, S, G).
z_node(Mode, O, G) :-
  z_object(Mode, O, G),
  % Make sure there are no duplicates.
  \+ z_subject(Mode, O, G).



%! z_object(+Mode, ?O, ?G) is nondet.

z_object(Mode, O, G) :-
  % [O] In memory we can pre-enumerate.
  (var(O), Mode == mem -> rdf_object(O) ; true),
  distinct(G, z(Mode, _, _, O, G)).



%! z_predicate(+Mode, ?P, ?G) is nondet.

z_predicate(Mode, P, G) :-
  % [O] In memory we can pre-enumerate and pre-check syntax.
  (   var(P)
  ->  (Mode == mem -> rdf_predicate(P) ; true)
  ;   (Mode == mem -> rdf_is_iri(P) ; true)
  ),
  distinct(G, z(Mode, _, P, _, G)).



%! z_subject(+Mode, ?S, ?G) is nondet.

z_subject(Mode, S, G) :-
  % [O] In memory we can pre-enumerate and pre-check syntax.
  (   var(S)
  ->  (Mode == mem -> rdf_subject(S) ; true)
  ;   (Mode == mem -> rdf_is_subject(S) ; true)
  ),
  distinct(G, z(Mode, S, _, _, G)).



%! z_term(+Mode, ?Term, ?G) is nondet.

z_term(Mode, P, G) :-
  z_predicate(Mode, P, G).
z_term(Mode, Node, G) :-
  z_node(Mode, Node, G),
  % Ensure there are no duplicates.
  \+ z_predicate(Mode, Node, G).
