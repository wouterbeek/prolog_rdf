:- module(
  q_term,
  [
    q_alias/1,             % ?Alias
   %q_alias_prefix/2,      % ?Alias, ?Prefix
    q_bnode/2,             % ?M, ?B
    q_bnode/3,             % ?M, ?B, ?G
   %q_create_bnode/1,      % -B
    q_datatype/2,          % ?M, ?D
    q_datatype/3,          % ?M, ?D, ?G
    q_default_graph/1,     % ?G
    q_iri/2,               % ?M, ?Iri
    q_iri/3,               % ?M, ?Iri, ?G
   %q_is_bnode/1,          % @Term
   %q_is_iri/1,            % @Term
   %q_is_literal/1,        % @Term
    q_is_lts/1,            % @Term
    q_is_legacy_literal/1, % @Term
   %q_is_name/1,           % @Term
    q_is_node/1,           % @Term
   %q_is_object/1,         % @Term
   %q_is_predicate/1,      % @Term
   %q_is_subject/1,        % @Term
   %q_is_term/1,           % @Term
    q_iri_alias/2,         % +Iri, -Alias
    q_iri_alias_local/3,   % +Iri, ?Alias, ?Local
    q_iri_alias_prefix/3,  % +Iri, ?Alias, ?Prefix
    q_iri_local/2,         % +Iri, -Local
    q_iri_prefix/2,        % +Iri, -Prefix
    q_legacy_literal/4,    % +Lit, -D, -Lex, -LTag
    q_literal/2,           % ?M, ?Lit
    q_literal/3,           % ?M, ?Lit, ?G
    q_literal/4,           % ?Lit, ?D, ?Lex, ?LTag
    q_literal_datatype/2,  % +Lit, ?D
    q_literal_lex/2,       % +Lit, ?Lex
    q_literal_string/2,    % +Lit, -Str
    q_literal_val/2,       % +Lit, ?Val
    q_lone_bnode/2,        % ?M, ?B
    q_lone_bnode/3,        % ?M, ?B, ?G
    q_lts/2,               % ?M, ?Lit
    q_lts/3,               % ?M, ?Lit, ?G
    q_name/2,              % ?M, ?Name
    q_name/3,              % ?M, ?Name, ?G
    q_node/2,              % ?M, ?Node
    q_node/3,              % ?M, ?Node, ?G
    q_object/2,            % ?M, ?O
    q_object/3,            % ?M, ?O, ?G
    q_predicate/2,         % ?M, ?P
    q_predicate/3,         % ?M, ?P, ?G
    q_prefix/1,            % ?Prefix
    q_subject/2,           % ?M, ?S
    q_subject/3,           % ?M, ?S, ?G
    q_term/2,              % ?M, ?Term
    q_term/3               % ?M, ?Term, ?G,
   %qcompound/2,           % ?Comp0, ?Comp
   %qiri/2,                % ?Iri0, ?Iri
   %qis/2,                 % +Iri0, ?Iri
   %qobject/2              % ?O0, ?O
  ]
).
:- reexport(library(semweb/rdf11), [
     rdf_create_bnode/1 as q_create_bnode,
     rdf_current_prefix/2 as q_alias_prefix,
     rdf_default_graph/1 as q_default_graph,
     rdf_equal/2 as qis,
     rdf_global_id/2 as qiri,
     rdf_global_object/2 as qobject,
     rdf_global_term/2 as qcompound,
     rdf_is_bnode/1 as q_is_bnode,
     rdf_is_iri/1 as q_is_iri,
     rdf_is_literal/1 as q_is_literal,
     rdf_is_name/1 as q_is_name,
     rdf_is_object/1 as q_is_object,
     rdf_is_predicate/1 as q_is_predicate,
     rdf_is_subject/1 as q_is_subject,
     rdf_is_term/1 as q_is_term,
     (rdf_meta)/1,
     rdf_register_prefix/2 as q_create_alias,
     op(110, xfx, @),
     op(650, xfx, ^^)
   ]).

/** <module> Quine term API

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(semweb/rdf11)). % Priority for rdf_meta/1.
:- use_module(library(hdt/hdt_term)).
:- use_module(library(q/q_stmt)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(solution_sequences)).
:- use_module(library(typecheck)).

:- rdf_meta
   q_bnode(?, ?, r),
   q_datatype(?, r),
   q_datatype(?, r, r),
   q_iri(?, r),
   q_iri(?, r, r),
   q_is_lts(o),
   q_is_legacy_literal(o),
   q_iri_alias(r, ?),
   q_iri_alias_prefix(r, ?, ?),
   q_is_node(o),
   q_legacy_literal(o, r, ?, ?),
   q_literal(?, o),
   q_literal(?, o, r),
   q_literal(o, r, ?, ?),
   q_literal_datatype(o, r),
   q_literal_lex(o, ?),
   q_literal_string(o, -),
   q_literal_val(o, ?),
   q_lone_bnode(?, ?, r),
   q_lts(?, o),
   q_lts(?, o, r),
   q_name(?, o),
   q_name(?, o, r),
   q_node(?, o),
   q_node(?, o, r),
   q_object(?, o),
   q_object(?, o, r),
   q_predicate(?, r),
   q_predicate(?, r, r),
   q_subject(?, r),
   q_subject(?, r, r),
   q_term(?, o),
   q_term(?, o, r).





%! q_alias(?Alias) is nondet.

q_alias(Alias) :-
  q_alias_prefix(Alias, _).



%! q_bnode(?M, ?B) is nondet.
%! q_bnode(?M, ?B, ?G) is nondet.

q_bnode(rdf, B) :-
  rdf_bnode(B).
q_bnode(hdt, B) :-
  hdt_bnode(B).


q_bnode(rdf, B, G) :-
  rdf_bnode(B, G).
q_bnode(hdt, B, G) :-
  hdt_bnode(B, G).



%! q_datatype(?M, ?D) is nondet.
%! q_datatype(?M, ?D, ?G) is nondet.

q_datatype(rdf, D) :-
  rdf_datatype(D).
q_datatype(hdt, D) :-
  hdt_datatype(D).


q_datatype(rdf, D, G) :-
  rdf_datatype(D, G).
q_datatype(hdt, D, G) :-
  hdt_datatype(D, G).



%! q_iri(?M, ?Iri) is nondet.
%! q_iri(?M, ?Iri, ?G) is nondet.

q_iri(rdf, Iri) :-
  rdf_iri(Iri).
q_iri(hdt, Iri) :-
  hdt_iri(Iri).


q_iri(rdf, Iri, G) :-
  rdf_iri(Iri, G).
q_iri(hdt, Iri, G) :-
  hdt_iri(Iri, G).



%! q_iri_alias(+Iri, -Alias) is nondet.

q_iri_alias(Iri, Alias) :-
  qiri(Alias:_, Iri).



%! q_iri_alias_local(+Iri, -Alias, -Local) is det.
%! q_iri_alias_local(-Iri, +Alias, +Local) is det.

q_iri_alias_local(Iri, Alias, Local) :-
  qiri(Alias:Local, Iri).


%! q_iri_alias_prefix(+Iri, -Alias, -Prefix) is nondet.
%! q_iri_alias_prefix(-Iri, +Alias, +Prefix) is det.

q_iri_alias_prefix(Iri, Alias, Prefix) :-
  q_iri_alias(Iri, Alias),
  q_alias_prefix(Alias, Prefix).



%! q_iri_local(+Iri, -Local) is det.

q_iri_local(Iri, Local) :-
  qiri(_:Local, Iri).



%! q_iri_prefix(+Iri, -Prefix) is nondet.

q_iri_prefix(Iri, Prefix) :-
  qiri(_:Prefix, Iri).



%! q_is_lts(@Term) is semidet.

q_is_lts(Term) :-
  ground(Term),
  Term = _@_.



%! q_is_legacy_literal(@Term) is semidet.

q_is_legacy_literal(literal(type(_,_))) :- !.
q_is_legacy_literal(literal(lang(_,_))) :- !.
q_is_legacy_literal(literal(_)).



%! q_is_node(@Term) is semidet.

q_is_node(Term) :-
  q_is_object(Term).



%! q_legacy_literal(+Lit, -D, -Lex, -LTag) is det.
%! q_legacy_literal(-Lit, +D, +Lex, +LTag) is det.

q_legacy_literal(literal(type(D,Lex0)), D, Lex, _) :-
  \+ qis(rdf:langString, D), !,
  atom_string(Lex0, Lex).
q_legacy_literal(literal(lang(LTag,Lex0)), rdf:langString, Lex, LTag) :- !,
  atom_string(Lex0, Lex).
q_legacy_literal(literal(Lex0), xsd:string, Lex, _) :-
  atom_string(Lex0, Lex).



%! q_literal(?M, ?Lit) is nondet.
%! q_literal(?M, ?Lit, ?G) is nondet.

q_literal(rdf, Lit) :-
  rdf_literal(Lit).
q_literal(hdt, Lit) :-
  hdt_literal(Lit).


q_literal(rdf, Lit, G) :-
  rdf_literal(Lit, G).
q_literal(hdt, Lit, G) :-
  hdt_literal(Lit, G).



%! q_literal(+Lit, -D, -Lex, -LTag) is det.
%! q_literal(-Lit, +D, +Lex, +LTag) is det.

q_literal(Lit, D, Lex, LTag) :-
  var(Lit), !,
  q_legacy_literal(Lit0, D, Lex, LTag),
  rdf11:post_object(Lit, Lit0).
q_literal(Val^^D, D, Lex, _) :- !,
  q_literal_lex(Val^^D, Lex).
q_literal(Val@LTag, rdf:langString, Lex, LTag) :-
  q_literal_lex(Val@LTag, Lex).



%! q_literal_datatype(+Lit, +D) is semidet.
%! q_literal_datatype(+Lit, -D) is det.

q_literal_datatype(_^^D, D).
q_literal_datatype(_@_, rdf:langString).



%! q_literal_lex(+Lit, +Lex) is semidet.
%! q_literal_lex(+Lit, -Lex) is det.

q_literal_lex(Val^^D, Lex) :- !,
  rdf11:rdf_lexical_form(Val^^D, Lex^^D).
q_literal_lex(Val@_, Val).



%! q_literal_string(@Term) is semidet.

q_literal_string(V^^xsd:string, V) :- !.
q_literal_string(V@_, V).



%! q_literal_val(+Lit, +Val) is semidet.
%! q_literal_val(+Lit, -Val) is nondet.

q_literal_val(Val^^_, Val).
q_literal_val(Val@_, Val).



%! q_lone_bnode(?M, ?B) is nondet.
%! q_lone_bnode(?M, ?B, ?G) is nondet.

q_lone_bnode(M, B) :-
  q_lone_bnode(M, B, _).


q_lone_bnode(M, B, G) :-
  q_bnode(M, B, G),
  \+ q(M, B, _, _, G).



%! q_lts(?M, ?Lit) is nondet.
%! q_lts(?M, ?Lts, ?G) is nondet.

q_lts(rdf, Lit) :-
  rdf_lts(Lit).
q_lts(hdt, Lit) :-
  hdt_lts(Lit).


q_lts(rdf, Lit, G) :-
  rdf_lts(Lit, G).
q_lts(hdt, Lit, G) :-
  hdt_lts(Lit, G).



%! q_name(?M, ?Name) is nondet.
%! q_name(?M, ?Name, ?G) is nondet.

q_name(rdf, Name) :-
  rdf_name(Name).
q_name(hdt, Name) :-
  hdt_name(Name).


q_name(rdf, Name, G) :-
  rdf_name(Name, G).
q_name(hdt, Name, G) :-
  hdt_name(Name, G).



%! q_node(?M, ?Node) is nondet.
%! q_node(?M, ?Node, ?G) is nondet.

q_node(rdf, Node) :-
  rdf_node(Node).
q_node(hdt, Node) :-
  hdt_node(Node).


q_node(rdf, Node, G) :-
  rdf_node(Node, G).
q_node(hdt, Node, G) :-
  hdt_node(Node, G).



%! q_object(?M, ?O) is nondet.
%! q_object(?M, ?O, ?G) is nondet.

q_object(rdf, O) :-
  rdf_object(O).
q_object(hdt, O) :-
  hdt_object(O).


q_object(rdf, O, G) :-
  rdf_object(O, G).
q_object(hdt, O, G) :-
  hdt_object(O, G).



%! q_predicate(?M, ?P) is nondet.
%! q_predicate(?M, ?P, ?G) is nondet.

q_predicate(rdf, P) :-
  rdf_predicate(P).
q_predicate(hdt, P) :-
  hdt_predicate(P).


q_predicate(rdf, P, G) :-
  rdf_predicate(P, G).
q_predicate(hdt, P, G) :-
  hdt_predicate(P, G).



%! q_prefix(?Prefix) is nondet.

q_prefix(Prefix) :-
  q_alias_prefix(_, Prefix).



%! q_subject(?M, ?S) is nondet.
%! q_subject(?M, ?S, ?G) is nondet.

q_subject(rdf, S) :-
  rdf_subject(S).
q_subject(hdt, S) :-
  hdt_subject(S).


q_subject(rdf, S, G) :-
  rdf_subject(S, G).
q_subject(hdt, S, G) :-
  hdt_subject(S, G).



%! q_term(?M, ?Term) is nondet.
%! q_term(?M, ?Term, ?G) is nondet.

q_term(rdf, Term) :-
  rdf_term(Term).
q_term(hdt, Term) :-
  hdt_term(Term).


q_term(rdf, Term, G) :-
  rdf_term(Term, G).
q_term(hdt, Term, G) :-
  hdt_term(Term, G).
