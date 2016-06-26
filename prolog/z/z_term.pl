:- module(
  z_term,
  [
    z_alias/1,             % ?Alias
    z_alias_prefix/2,      % ?Alias, ?Prefix
    z_bnode/1,             % ?B
    z_bnode/2,             % ?B, ?G
    z_datatype/1,          % ?D
    z_datatype/2,          % ?D, ?G
    z_iri/1,               % ?Iri
    z_iri/2,               % ?Iri, ?G
    z_is_lts/1,            % @Term
    z_is_legacy_literal/1, % @Term
    z_iri_alias/2,         % +Iri, -Alias
    z_iri_alias_prefix/3,  % +Iri, -Alias, -Prefix
    z_legacy_literal/4,    % +Lit, -D, -Lex, -LTag
    z_literal/1,           % ?Lit
    z_literal/2,           % ?Lit, ?G
    z_literal/4,           % ?Lit, ?D, ?Lex, ?LTag
    z_literal_datatype/2,  % +Lit, ?D
    z_literal_lex/2,       % +Lit, ?Lex
    z_literal_val/2,       % +Lit, ?Val
    z_lone_bnode/1,        % ?B
    z_lone_bnode/2,        % ?B, ?G
    z_lts/1,               % ?Lit
    z_lts/2,               % ?Lit, ?G
    z_name/1,              % ?Name
    z_name/2,              % ?Name, ?G
    z_node/1,              % ?Node
    z_node/2,              % ?Node, ?G
    z_object/1,            % ?O
    z_object/2,            % ?O, ?G
    z_predicate/1,         % ?P
    z_predicate/2,         % ?P, ?G
    z_prefix/1,            % ?Prefix
    z_subject/1,           % ?S
    z_subject/2,           % ?S, ?G
    z_term/1,              % ?Term
    z_term/2               % ?Term, ?G
  ]
).

/** <module> Z terms

@author Wouter Beek
@compat RDF 1.1 Concepts and Abstract Syntax
@see http://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/
@version 2015/07-2015/08, 2015/10, 2015/12-2016/03, 2016/05-2016/06
*/

:- use_module(library(hdt/hdt_term)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(solution_sequences)).
:- use_module(library(typecheck)).
:- use_module(library(z/z_stmt)).

:- rdf_meta
   z_bnode(?, r),
   z_datatype(r),
   z_datatype(r, r),
   z_iri(r),
   z_iri(r, r),
   z_is_lts(o),
   z_is_legacy_literal(o),
   z_iri_alias(r, ?),
   z_iri_alias_prefix(r, ?, ?),
   z_legacy_literal(o, r, ?, ?),
   z_literal(o),
   z_literal(o, r),
   z_literal(o, r, ?, ?),
   z_literal_datatype(o, r),
   z_literal_lex(o, ?),
   z_literal_val(o, ?),
   z_lone_bnode(?, r),
   z_lts(o),
   z_lts(o, r),
   z_name(o),
   z_name(o, r),
   z_node(o),
   z_node(o, r),
   z_object(o),
   z_object(o, r),
   z_predicate(r),
   z_predicate(r, r),
   z_subject(r),
   z_subject(r, r),
   z_term(o),
   z_term(o, r).





%! z_alias(?Alias) is nondet.

z_alias(Alias) :-
  rdf_current_prefix(Alias, _).



%! z_alias_prefix(?Alias, ?Prefix) is nondet.

z_alias_prefix(Alias, Prefix) :-
  rdf_current_prefix(Alias, Prefix).



%! z_bnode(?B) is nondet.
%! z_bnode(?B, ?G) is nondet.

z_bnode(B) :-
  rdf_bnode(B).
z_bnode(B) :-
  hdt_bnode(B).


z_bnode(B, G) :-
  rdf_bnode(B, G).
z_bnode(B, G) :-
  hdt_bnode(B, G).



%! z_datatype(?D) is nondet.
%! z_datatype(?D, ?G) is nondet.

z_datatype(D) :-
  rdf_datatype(D).
z_datatype(D) :-
  hdt_datatype(D).


z_datatype(D, G) :-
  rdf_datatype(D, G).
z_datatype(D, G) :-
  hdt_datatype(D, G).



%! z_is_lts(@Term) is semidet.

z_is_lts(Term) :-
  ground(Term),
  Term = _@_.



%! z_is_legacy_literal(@Term) is semidet.

z_is_legacy_literal(literal(type(_,_))) :- !.
z_is_legacy_literal(literal(lang(_,_))) :- !.
z_is_legacy_literal(literal(_)).



%! z_iri(?Iri) is nondet.
%! z_iri(?Iri, ?G) is nondet.

z_iri(Iri) :-
  rdf_iri(Iri).
z_iri(Iri) :-
  hdt_iri(Iri).


z_iri(Iri, G) :-
  rdf_iri(Iri, G).
z_iri(Iri, G) :-
  hdt_iri(Iri, G).



%! z_iri_alias(+Iri, -Alias) is nondet.

z_iri_alias(Iri, Alias) :-
  rdf_global_id(Alias:_, Iri).



%! z_iri_alias_prefix(+Iri, -Alias, -Prefix) is nondet.

z_iri_alias_prefix(Iri, Alias, Prefix) :-
  z_iri_alias(Iri, Alias),
  rdf_current_prefix(Alias, Prefix).



%! z_legacy_literal(+Lit, -D, -Lex, -LTag) is det.
%! z_legacy_literal(-Lit, +D, +Lex, +LTag) is det.

z_legacy_literal(literal(type(D,Lex0)), D, Lex, _) :-
  \+ rdf_equal(rdf:langString, D), !,
  atom_string(Lex0, Lex).
z_legacy_literal(literal(lang(LTag,Lex0)), rdf:langString, Lex, LTag) :- !,
  atom_string(Lex0, Lex).
z_legacy_literal(literal(Lex0), xsd:string, Lex, _) :-
  atom_string(Lex0, Lex).



%! z_literal(?Lit) is nondet.
%! z_literal(?Lit, ?G) is nondet.

z_literal(Lit) :-
  rdf_literal(Lit).
z_literal(Lit) :-
  hdt_literal(Lit).


z_literal(Lit, G) :-
  rdf_literal(Lit, G).
z_literal(Lit, G) :-
  hdt_literal(Lit, G).



%! z_literal(+Lit, -D, -Lex, -LTag) is det.
%! z_literal(-Lit, +D, +Lex, +LTag) is det.

z_literal(Lit, D, Lex, LTag) :-
  var(Lit), !,
  z_legacy_literal(Lit0, D, Lex, LTag),
  rdf11:post_object(Lit, Lit0).
z_literal(Val^^D, D, Lex, _) :- !,
  z_literal_lex(Val^^D, Lex).
z_literal(Val@LTag, rdf:langString, Lex, LTag) :-
  z_literal_lex(Val@LTag, Lex).



%! z_literal_datatype(+Lit, +D) is semidet.
%! z_literal_datatype(+Lit, -D) is det.

z_literal_datatype(_^^D, D).
z_literal_datatype(_@_, rdf:langString).



%! z_literal_lex(+Lit, +Lex) is semidet.
%! z_literal_lex(+Lit, -Lex) is det.

z_literal_lex(Val^^D, Lex) :- !,
  rdf_lexical_form(Val^^D, Lex^^D).
z_literal_lex(Val@_, Val).



%! z_literal_val(+Lit, +Val) is semidet.
%! z_literal_val(+Lit, -Val) is nondet.

z_literal_val(Val^^_, Val).
z_literal_val(Val@_, Val).



%! z_lone_bnode(?B) is nondet.
%! z_lone_bnode(?B, ?G) is nondet.

z_lone_bnode(B) :-
  z_lone_bnode(B, _).


z_lone_bnode(B, G) :-
  z_bnode(B, G),
  \+ z(B, _, _, G).



%! z_lts(?Lit) is nondet.
%! z_lts(?Lts, ?G) is nondet.

z_lts(Lit) :-
  rdf_lts(Lit).
z_lts(Lit) :-
  hdt_lts(Lit).


z_lts(Lit, G) :-
  rdf_lts(Lit, G).
z_lts(Lit, G) :-
  hdt_lts(Lit, G).



%! z_name(?Name) is nondet.
%! z_name(?Name, ?G) is nondet.

z_name(Name) :-
  rdf_name(Name).
z_name(Name) :-
  hdt_name(Name).


z_name(Name, G) :-
  rdf_name(Name, G).
z_name(Name, G) :-
  hdt_name(Name, G).



%! z_node(?Node) is nondet.
%! z_node(?Node, ?G) is nondet.

z_node(Node) :-
  rdf_node(Node).
z_node(Node) :-
  hdt_node(Node).


z_node(Node, G) :-
  rdf_node(Node, G).
z_node(Node, G) :-
  hdt_node(Node, G).



%! z_object(?O) is nondet.
%! z_object(?O, ?G) is nondet.

z_object(O) :-
  rdf_object(O).
z_object(O) :-
  hdt_object(O).


z_object(O, G) :-
  rdf_object(O, G).
z_object(O, G) :-
  hdt_object(O, G).



%! z_predicate(?P) is nondet.
%! z_predicate(?P, ?G) is nondet.

z_predicate(P) :-
  rdf_predicate(P).
z_predicate(P) :-
  hdt_predicate(P).


z_predicate(P, G) :-
  rdf_predicate(P, G).
z_predicate(P, G) :-
  hdt_predicate(P, G).



%! z_prefix(?Prefix) is nondet.

z_prefix(Prefix) :-
  rdf_current_prefix(_, Prefix).



%! z_subject(?S) is nondet.
%! z_subject(?S, ?G) is nondet.

z_subject(S) :-
  rdf_subject(S).
z_subject(S) :-
  hdt_subject(S).


z_subject(S, G) :-
  rdf_subject(S, G).
z_subject(S, G) :-
  hdt_subject(S, G).



%! z_term(?Term) is nondet.
%! z_term(?Term, ?G) is nondet.

z_term(Term) :-
  rdf_term(Term).
z_term(Term) :-
  hdt_term(Term).


z_term(Term, G) :-
  rdf_term(Term, G).
z_term(Term, G) :-
  hdt_term(Term, G).
