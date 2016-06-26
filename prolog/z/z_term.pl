:- module(
  z_term,
  [
    z_alias/1,             % ?Alias
    z_alias_prefix/2,      % ?Alias, ?Prefix
    z_bnode/2,             % ?M, ?B
    z_bnode/3,             % ?M, ?B, ?G
    z_datatype/2,          % ?M, ?D
    z_datatype/3,          % ?M, ?D, ?G
    z_iri/2,               % ?M, ?Iri
    z_iri/3,               % ?M, ?Iri, ?G
    z_is_lts/1,            % @Term
    z_is_legacy_literal/1, % @Term
    z_iri_alias/2,         % +Iri, -Alias
    z_iri_alias_prefix/3,  % +Iri, -Alias, -Prefix
    z_legacy_literal/4,    % +Lit, -D, -Lex, -LTag
    z_literal/2,           % ?M, ?Lit
    z_literal/3,           % ?M, ?Lit, ?G
    z_literal/4,           % ?Lit, ?D, ?Lex, ?LTag
    z_literal_datatype/2,  % +Lit, ?D
    z_literal_lex/2,       % +Lit, ?Lex
    z_literal_val/2,       % +Lit, ?Val
    z_lone_bnode/2,        % ?M, ?B
    z_lone_bnode/3,        % ?M, ?B, ?G
    z_lts/2,               % ?M, ?Lit
    z_lts/3,               % ?M, ?Lit, ?G
    z_name/2,              % ?M, ?Name
    z_name/3,              % ?M, ?Name, ?G
    z_node/2,              % ?M, ?Node
    z_node/3,              % ?M, ?Node, ?G
    z_object/2,            % ?M, ?O
    z_object/3,            % ?M, ?O, ?G
    z_predicate/2,         % ?M, ?P
    z_predicate/3,         % ?M, ?P, ?G
    z_prefix/1,            % ?Prefix
    z_subject/2,           % ?M, ?S
    z_subject/3,           % ?M, ?S, ?G
    z_term/2,              % ?M, ?Term
    z_term/3               % ?M, ?Term, ?G
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
   z_bnode(?, ?, r),
   z_datatype(?, r),
   z_datatype(?, r, r),
   z_iri(?, r),
   z_iri(?, r, r),
   z_is_lts(o),
   z_is_legacy_literal(o),
   z_iri_alias(r, ?),
   z_iri_alias_prefix(r, ?, ?),
   z_legacy_literal(o, r, ?, ?),
   z_literal(?, o),
   z_literal(?, o, r),
   z_literal(o, r, ?, ?),
   z_literal_datatype(o, r),
   z_literal_lex(o, ?),
   z_literal_val(o, ?),
   z_lone_bnode(?, ?, r),
   z_lts(?, o),
   z_lts(?, o, r),
   z_name(?, o),
   z_name(?, o, r),
   z_node(?, o),
   z_node(?, o, r),
   z_object(?, o),
   z_object(?, o, r),
   z_predicate(?, r),
   z_predicate(?, r, r),
   z_subject(?, r),
   z_subject(?, r, r),
   z_term(?, o),
   z_term(?, o, r).





%! z_alias(?Alias) is nondet.

z_alias(Alias) :-
  rdf_current_prefix(Alias, _).



%! z_alias_prefix(?Alias, ?Prefix) is nondet.

z_alias_prefix(Alias, Prefix) :-
  rdf_current_prefix(Alias, Prefix).



%! z_bnode(?M, ?B) is nondet.
%! z_bnode(?M, ?B, ?G) is nondet.

z_bnode(rdf, B) :-
  rdf_bnode(B).
z_bnode(hdt, B) :-
  hdt_bnode(B).


z_bnode(rdf, B, G) :-
  rdf_bnode(B, G).
z_bnode(hdt, B, G) :-
  hdt_bnode(B, G).



%! z_datatype(?M, ?D) is nondet.
%! z_datatype(?M, ?D, ?G) is nondet.

z_datatype(rdf, D) :-
  rdf_datatype(D).
z_datatype(hdt, D) :-
  hdt_datatype(D).


z_datatype(rdf, D, G) :-
  rdf_datatype(D, G).
z_datatype(hdt, D, G) :-
  hdt_datatype(D, G).



%! z_is_lts(@Term) is semidet.

z_is_lts(Term) :-
  ground(Term),
  Term = _@_.



%! z_is_legacy_literal(@Term) is semidet.

z_is_legacy_literal(literal(type(_,_))) :- !.
z_is_legacy_literal(literal(lang(_,_))) :- !.
z_is_legacy_literal(literal(_)).



%! z_iri(?M, ?Iri) is nondet.
%! z_iri(?M, ?Iri, ?G) is nondet.

z_iri(rdf, Iri) :-
  rdf_iri(Iri).
z_iri(hdt, Iri) :-
  hdt_iri(Iri).


z_iri(rdf, Iri, G) :-
  rdf_iri(Iri, G).
z_iri(hdt, Iri, G) :-
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



%! z_literal(?M, ?Lit) is nondet.
%! z_literal(?M, ?Lit, ?G) is nondet.

z_literal(rdf, Lit) :-
  rdf_literal(Lit).
z_literal(hdt, Lit) :-
  hdt_literal(Lit).


z_literal(rdf, Lit, G) :-
  rdf_literal(Lit, G).
z_literal(hdt, Lit, G) :-
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



%! z_lone_bnode(?M, ?B) is nondet.
%! z_lone_bnode(?M, ?B, ?G) is nondet.

z_lone_bnode(M, B) :-
  z_lone_bnode(M, B, _).


z_lone_bnode(M, B, G) :-
  z_bnode(M, B, G),
  \+ z(M, B, _, _, G).



%! z_lts(?M, ?Lit) is nondet.
%! z_lts(?M, ?Lts, ?G) is nondet.

z_lts(rdf, Lit) :-
  rdf_lts(Lit).
z_lts(hdt, Lit) :-
  hdt_lts(Lit).


z_lts(rdf, Lit, G) :-
  rdf_lts(Lit, G).
z_lts(hdt, Lit, G) :-
  hdt_lts(Lit, G).



%! z_name(?M, ?Name) is nondet.
%! z_name(?M, ?Name, ?G) is nondet.

z_name(rdf, Name) :-
  rdf_name(Name).
z_name(hdt, Name) :-
  hdt_name(Name).


z_name(rdf, Name, G) :-
  rdf_name(Name, G).
z_name(hdt, Name, G) :-
  hdt_name(Name, G).



%! z_node(?M, ?Node) is nondet.
%! z_node(?M, ?Node, ?G) is nondet.

z_node(rdf, Node) :-
  rdf_node(Node).
z_node(hdt, Node) :-
  hdt_node(Node).


z_node(rdf, Node, G) :-
  rdf_node(Node, G).
z_node(hdt, Node, G) :-
  hdt_node(Node, G).



%! z_object(?M, ?O) is nondet.
%! z_object(?M, ?O, ?G) is nondet.

z_object(rdf, O) :-
  rdf_object(O).
z_object(hdt, O) :-
  hdt_object(O).


z_object(rdf, O, G) :-
  rdf_object(O, G).
z_object(hdt, O, G) :-
  hdt_object(O, G).



%! z_predicate(?M, ?P) is nondet.
%! z_predicate(?M, ?P, ?G) is nondet.

z_predicate(rdf, P) :-
  rdf_predicate(P).
z_predicate(hdt, P) :-
  hdt_predicate(P).


z_predicate(rdf, P, G) :-
  rdf_predicate(P, G).
z_predicate(hdt, P, G) :-
  hdt_predicate(P, G).



%! z_prefix(?Prefix) is nondet.

z_prefix(Prefix) :-
  rdf_current_prefix(_, Prefix).



%! z_subject(?M, ?S) is nondet.
%! z_subject(?M, ?S, ?G) is nondet.

z_subject(rdf, S) :-
  rdf_subject(S).
z_subject(hdt, S) :-
  hdt_subject(S).


z_subject(rdf, S, G) :-
  rdf_subject(S, G).
z_subject(hdt, S, G) :-
  hdt_subject(S, G).



%! z_term(?M, ?Term) is nondet.
%! z_term(?M, ?Term, ?G) is nondet.

z_term(rdf, Term) :-
  rdf_term(Term).
z_term(hdt, Term) :-
  hdt_term(Term).


z_term(rdf, Term, G) :-
  rdf_term(Term, G).
z_term(hdt, Term, G) :-
  hdt_term(Term, G).
