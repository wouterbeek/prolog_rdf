:- module(
  rdf_term,
  [
    rdf_alias/1,                % ?Alias
    rdf_alias_aggregate_all/3,  % +Templ, :Goal_0, -Result
    rdf_alias_defval/2,         % +Def, -Val
    rdf_alias_domain/2,         % +Alias, -Domain
    rdf_alias_member/2,         % ?Elem, +L
    rdf_alias_memberchk/2,      % ?Elem, +L
   %rdf_alias_prefix/2,         % ?Alias, ?Prefix
    rdf_bnode/2,                % +M, ?BNode
    rdf_bnode/3,                % +M, ?BNode, ?G
    rdf_bnode_label/2,          % +BNode, -Lbl
    rdf_class/2,                % +M, -C
    rdf_class/3,                % +M, -C, ?G
    rdf_datatype/2,             % +M, ?D
    rdf_datatype/3,             % +M, ?D, ?G
   %rdf_equal/2,                % +?Term1, ?Term2
    rdf_external_iri/2,         % +M, ?Iri
   %rdf_global_id/2,            % ?Id, ?GlobalId
   %rdf_global_object/2,        % ?O, ?GlobalO
   %rdf_global_term/2,          % ?Term, ?GlobalTerm
    rdf_instance/3,             % +M, ?I, ?C
    rdf_instance/4,             % +M, ?I, ?C, ?G
    rdf_internal_iri/2,         % +M, ?Iri
    rdf_iri/2,                  % +M, ?Iri
    rdf_iri/3,                  % +M, ?Iri, ?G
    rdf_iri_alias/2,            % +Iri, -Alias
    rdf_iri_alias_local/3,      % +Iri, ?Alias, ?Local
    rdf_iri_alias_prefix/3,     % +Iri, ?Alias, ?Prefix
    rdf_iri_local/2,            % +Iri, -Local
    rdf_iri_prefix/2,           % +Iri, -Prefix
   %rdf_is_bnode/1,             % @Term
   %rdf_is_iri/1,               % @Term
   %rdf_is_literal/1,           % @Term
    rdf_is_lts/1,               % @Term
    rdf_is_legacy_literal/1,    % @Term
   %rdf_is_name/1,              % @Term
    rdf_is_node/1,              % @Term
   %rdf_is_object/1,            % @Term
   %rdf_is_predicate/1,         % @Term
   %rdf_is_subject/1,           % @Term
   %rdf_is_term/1,              % @Term
    rdf_legacy_literal/4,       % +Lit, -D, -Lex, -LTag
    rdf_literal/2,              % +M, ?Lit
    rdf_literal/3,              % +M, ?Lit, ?G
    rdf_literal/4,              % ?Lit, ?D, ?Lex, ?LTag
    rdf_literal_datatype/2,     % +Lit, ?D
    rdf_literal_lexical_form/2, % +Lit, ?Lex
    rdf_literal_ltag/2,         % +Lit, -LTag
    rdf_literal_string/2,       % +Lit, -Str
    rdf_literal_value/2,        % +Lit, ?Val
    rdf_lone_bnode/2,           % +M, ?BNode
    rdf_lone_bnode/3,           % +M, ?BNode, ?G
    rdf_lts/2,                  % +M, ?Lit
    rdf_lts/3,                  % +M, ?Lit, ?G
   %(rdf_meta)/1,
    rdf_name/2,                 % +M, ?Name
    rdf_name/3,                 % +M, ?Name, ?G
    rdf_node/2,                 % +M, ?Node
    rdf_node/3,                 % +M, ?Node, ?G
    rdf_object/2,               % +M, ?O
    rdf_object/3,               % +M, ?O, ?G
    rdf_predicate/2,            % +M, ?P
    rdf_predicate/3,            % +M, ?P, ?G
    rdf_property/2,             % +M, ?Prop
    rdf_property/3,             % +M, ?Prop, ?G
    rdf_prefix/1,               % ?Prefix
    rdf_query_term/2,           % +Term, -QueryTerm
    rdf_query_term/3,           % +Key, +Val, -QueryTerm
    rdf_snap/1,                 % :Goal_0
    rdf_subject/2,              % +M, ?S
    rdf_subject/3,              % +M, ?S, ?G
    rdf_term/2,                 % +M, ?Term
    rdf_term/3,                 % +M, ?Term, ?G
    rdf_term_expansion/2,       % +Atom, -Term
    rdf_term_to_atom/2          % +Term, -Atom
   %rdf_transaction/1           % :Goal_0
   %rdf_transaction/2           % :Goal_0, +Id
   %rdf_transaction/3           % :Goal_0, +Is, +Opts
  ]
).
:- reexport(library(semweb/rdf_db), [
     rdf_current_prefix/2 as rdf_alias_prefix,
     rdf_equal/2,
     rdf_global_id/2,
     rdf_global_object/2,
     rdf_global_term/2,
     (rdf_meta)/1
   ]).
:- reexport(library(semweb/rdf11), [
     op(110, xfx, @),
     op(650, xfx, ^^)
   ]).

/** <module> Quine term API

@author Wouter Beek
@version 2016/06-2017/01
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(hdt/hdt_ext)).
:- use_module(library(rdf/rdf_api)).
:- use_module(library(rdf/rdf_iri)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(rdf/rdfs_api)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(solution_sequences)).
:- use_module(library(typecheck)).

:- multifile
    error:has_type/2.

error:has_type(rdf_bnode, BNode) :-
  rdf_is_bnode(BNode).
error:has_type(rdf_graph, G) :-
  error:has_type(rdf_iri, G).
error:has_type(rdf_iri, Iri) :-
  error:has_type(iri, Iri).
error:has_type(rdf_literal, Lit) :-
  rdf_is_literal(Lit).
error:has_type(rdf_name, Iri) :-
  error:has_type(rdf_iri, Iri).
error:has_type(rdf_name, Lit) :-
  error:has_type(rdf_literal, Lit).
error:has_type(rdf_term, BNode) :-
  error:has_type(rdf_bnode, BNode).
error:has_type(rdf_term, Lit) :-
  error:has_type(rdf_literal, Lit).
error:has_type(rdf_term, Iri) :-
  error:has_type(rdf_iri, Iri).

:- meta_predicate
    rdf_alias_aggregate_all(+, 0, -),
    rdf_snap(0).

:- multifile
    http:convert_parameter/3.

http:convert_parameter(rdf_iri, A, A).
http:convert_parameter(rdf_literal, A, Lit) :-
  atom_phrase(rdf_literal0(Lit), A).
http:convert_parameter(rdf_term, A, Lit) :-
  http:convert_parameter(rdf_literal, A, Lit).
http:convert_parameter(rdf_term, A, Iri) :-
  http:convert_parameter(rdf_iri, A, Iri).

rdf_iri0(Iri) -->
  "<",
  '...'(Cs),
  ">",
  {atom_codes(Iri, Cs)}.

rdf_lexical_form0(End, L)     --> "\\\'", !, rdf_lexical_form0(End, L).
rdf_lexical_form0(End, L)     --> "\\\"", !, rdf_lexical_form0(End, L).
rdf_lexical_form0(End, [])    --> End,    !.
rdf_lexical_form0(End, [H|T]) --> [H],    !, rdf_lexical_form0(End, T).
rdf_lexical_form0(_,   [])    --> "".

rdf_literal0(Lit) -->
  "\"", !,
  rdf_lexical_form0([0'"], Cs), %"
  (   "@"
  ->  rdf_ltag0(LTag),
      {
        string_codes(Str, Cs),
        Lit = Str@LTag
      }
  ;   "^^",
      rdf_iri0(D),
      {
        atom_codes(Lex, Cs),
        rdf11:out_type(D, Val, Lex),
        Lit = Val^^D
      }
  ).

rdf_ltag0(LTag) -->
  rest(Cs),
  {atom_codes(LTag, Cs)}.

:- rdf_meta
   rdf_alias_aggregate_all(+, t, -),
   rdf_alias_defval(r, -),
   rdf_alias_member(t, t),
   rdf_alias_memberchk(t, t),
   rdf_bnode(?, ?, r),
   rdf_class(+, r),
   rdf_class(+, r, r),
   rdf_datatype(?, r),
   rdf_datatype(?, r, r),
   rdf_external_iri(?, r),
   rdf_instance(+, r, r),
   rdf_instance(+, r, r, r),
   rdf_internal_iri(?, r),
   rdf_iri(?, r),
   rdf_iri(?, r, r),
   rdf_is_lts(o),
   rdf_is_legacy_literal(o),
   rdf_iri_alias(r, ?),
   rdf_iri_alias_prefix(r, ?, ?),
   rdf_is_node(o),
   rdf_legacy_literal(o, r, ?, ?),
   rdf_literal(?, o),
   rdf_literal(?, o, r),
   rdf_literal(o, r, ?, ?),
   rdf_literal_datatype(o, r),
   rdf_literal_lexical_form(o, ?),
   rdf_literal_string(o, -),
   rdf_literal_value(o, ?),
   rdf_lone_bnode(?, ?, r),
   rdf_lts(?, o),
   rdf_lts(?, o, r),
   rdf_name(?, o),
   rdf_name(?, o, r),
   rdf_node(?, o),
   rdf_node(?, o, r),
   rdf_object(?, o),
   rdf_object(?, o, r),
   rdf_predicate(?, r),
   rdf_predicate(?, r, r),
   rdf_property(?, r),
   rdf_property(?, r, r),
   rdf_subject(?, r),
   rdf_subject(?, r, r),
   rdf_term(?, o),
   rdf_term(?, o, r).





%! rdf_alias(?Alias) is nondet.

rdf_alias(Alias) :-
  rdf_alias_prefix(Alias, _).



%! rdf_alias_aggregate_all(+Templ, :Goal_0, -Result) is det.
%
% Calls aggregate_all/3 under RDF alias expansion.

rdf_alias_aggregate_all(Templ, Goal_0, Result) :-
  aggregate_all(Templ, Goal_0, Result).



%! rdf_alias_defval(+Def, -Val) is det.

rdf_alias_defval(_, X) :-
  nonvar(X), !.
rdf_alias_defval(X, X).



%! rdf_alias_domain(+Alias, -Domain) is det.

rdf_alias_domain(Alias, Domain) :-
  rdf_alias_prefix(Alias, Prefix),
  uri_components(Prefix, uri_components(_,Domain,_,_,_)).



%! rdf_alias_member(?Elem, +L) is nondet.
%
% Calls member/2 under RDF alias expansion.

rdf_alias_member(Elem, L) :-
  member(Elem, L).



%! rdf_alias_memberchk(?Elem, +L) is nondet.
%
% Calls memberchk/2 under RDF alias expansion.

rdf_alias_memberchk(Elem, L) :-
  memberchk(Elem, L).



%! rdf_bnode(+M, ?BNode) is nondet.
%! rdf_bnode(+M, ?BNode, ?G) is nondet.

rdf_bnode(hdt, BNode) :-
  hdt_bnode(BNode).
rdf_bnode(trp, BNode) :-
  rdf11:rdf_bnode(BNode).


rdf_bnode(hdt, BNode, G) :-
  hdt_bnode(BNode, G).
rdf_bnode(hdt0, BNode, Hdt) :-
  hdt_bnode0(BNode, Hdt).
rdf_bnode(trp, BNode, G) :-
  trp_bnode(BNode, G).



%! rdf_bnode_label(+BNode, -Lbl) is det.

rdf_bnode_label(BNode, BNode) :-
  rdf_is_bnode(BNode), !.
rdf_bnode_label(BNode, Lbl) :-
  rdf_global_id(bnode:Local, BNode),
  atomic_list_concat([_,Lbl], :, Local).



%! rdf_class(+M, ?C) is nondet.
%! rdf_class(+M, ?C, ?G) is det.

rdf_class(M, C) :-
  distinct(C, rdf_class(M, C, _)).


rdf_class(M, C, G) :-
  distinct(C, rdf_instance(M, _, C)),
  rdf_node(M, C, G).



%! rdf_datatype(+M, ?D) is nondet.
%! rdf_datatype(+M, ?D, ?G) is nondet.

rdf_datatype(hdt, D) :-
  hdt_datatype(D).
rdf_datatype(trp, D) :-
  trp_datatype(D).


rdf_datatype(hdt, D, G) :-
  hdt_datatype(D, G).
rdf_datatype(hdt0, D, Hdt) :-
  hdt_datatype0(D, Hdt).
rdf_datatype(trp, D, G) :-
  trp_datatype(D, G).



%! rdf_external_iri(+M, +Iri) is semidet.
%! rdf_external_iri(+M, -Iri) is nondet.

rdf_external_iri(M, Iri) :-
  rdf_iri(M, Iri),
  rdf_is_external_iri(Iri).



%! rdf_instance(+M, ?I, ?C) is nondet.
%! rdf_instance(+M, ?I, ?C, ?G) is nondet.

rdf_instance(M, I, C) :-
  rdf_instance(M, I, C, _).


rdf_instance(_, I, D, _) :-
  rdf_is_literal(I), !,
  rdf_literal_datatype(I, D).
rdf_instance(M, I, C, G) :-
  rdf_equal(rdf:type, P), % @hack
  t(M, I, P, C, G).



%! rdf_internal_iri(+M, +Iri) is semidet.
%! rdf_internal_iri(+M, -Iri) is nondet.

rdf_internal_iri(M, Iri) :-
  rdf_iri(M, Iri),
  rdf_is_internal_iri(Iri).



%! rdf_iri(+M, ?Iri) is nondet.
%! rdf_iri(+M, ?Iri, ?G) is nondet.

rdf_iri(hdt, Iri) :-
  hdt_iri(Iri).
rdf_iri(trp, Iri) :-
  rdf11:rdf_iri(Iri).


rdf_iri(hdt, Iri, G) :-
  hdt_iri(Iri, G).
rdf_iri(hdt0, Iri, Hdt) :-
  hdt_iri0(Iri, Hdt).
rdf_iri(trp, Iri, G) :-
  trp_iri(Iri, G).



%! rdf_iri_alias(+Iri, -Alias) is nondet.

rdf_iri_alias(Iri, Alias) :-
  rdf_global_id(Alias:_, Iri).



%! rdf_iri_alias_local(+Iri, -Alias, -Local) is det.
%! rdf_iri_alias_local(-Iri, +Alias, +Local) is det.

rdf_iri_alias_local(Iri, Alias, Local) :-
  rdf_global_id(Alias:Local, Iri).



%! rdf_iri_alias_prefix(+Iri, -Alias, -Prefix) is nondet.
%! rdf_iri_alias_prefix(-Iri, +Alias, +Prefix) is det.

rdf_iri_alias_prefix(Iri, Alias, Prefix) :-
  rdf_iri_alias(Iri, Alias),
  rdf_alias_prefix(Alias, Prefix).



%! rdf_iri_local(+Iri, -Local) is det.

rdf_iri_local(Iri, Local) :-
  rdf_global_id(_:Local, Iri).



%! rdf_iri_prefix(+Iri, -Prefix) is nondet.

rdf_iri_prefix(Iri, Prefix) :-
  rdf_global_id(_:Prefix, Iri).



%! rdf_is_lts(@Term) is semidet.

rdf_is_lts(Term) :-
  ground(Term),
  Term = _@_.



%! rdf_is_legacy_literal(@Term) is semidet.

rdf_is_legacy_literal(literal(type(_,_))) :- !.
rdf_is_legacy_literal(literal(lang(_,_))) :- !.
rdf_is_legacy_literal(literal(_)).



%! rdf_is_node(@Term) is semidet.

rdf_is_node(Term) :-
  rdf_is_object(Term).



%! rdf_legacy_literal(+Lit, -D, -Lex, -LTag) is det.
%! rdf_legacy_literal(-Lit, +D, +Lex, +LTag) is det.

rdf_legacy_literal(literal(type(D,Dom)), D, Dom, _) :-
  rdf_memberchk(D, [rdf:'HTML',rdf:'XMLLiteral']), !.
rdf_legacy_literal(literal(type(D,Lex0)), D, Lex, _) :-
  \+ rdf_equal(rdf:langString, D), !,
  atom_string(Lex0, Lex).
rdf_legacy_literal(literal(lang(LTag,Lex0)), rdf:langString, Lex, LTag) :- !,
  atom_string(Lex0, Lex).
rdf_legacy_literal(literal(Lex0), xsd:string, Lex, _) :-
  atom_string(Lex0, Lex).



%! rdf_literal(+M, ?Lit) is nondet.
%! rdf_literal(+M, ?Lit, ?G) is nondet.

rdf_literal(hdt, Lit) :-
  hdt_literal(Lit).
rdf_literal(trp, Lit) :-
  rdf11:rdf_literal(Lit).


rdf_literal(hdt, Lit, G) :-
  hdt_literal(Lit, G).
rdf_literal(hdt0, Lit, Hdt) :-
  hdt_literal0(Lit, Hdt).
rdf_literal(trp, Lit, G) :-
  trp_literal(Lit, G).



%! rdf_literal(+Lit, -D, -Lex, -LTag) is det.
%! rdf_literal(-Lit, +D, +Lex, +LTag) is det.

rdf_literal(Lit, D, Lex, LTag) :-
  var(Lit), !,
  rdf_legacy_literal(Lit0, D, Lex, LTag),
  rdf11:post_object(Lit, Lit0).
rdf_literal(Val^^D, D, Lex, _) :- !,
  rdf_literal_lexical_form(Val^^D, Lex).
rdf_literal(Val@LTag, rdf:langString, Lex, LTag) :-
  rdf_literal_lexical_form(Val@LTag, Lex).



%! rdf_literal_datatype(+Lit, +D) is semidet.
%! rdf_literal_datatype(+Lit, -D) is det.

rdf_literal_datatype(_^^D, D).
rdf_literal_datatype(_@_, rdf:langString).



%! rdf_literal_lexical_form(+Lit, +Lex) is semidet.
%! rdf_literal_lexical_form(+Lit, -Lex) is det.
%
% Lexical Lex is always a string.

rdf_literal_lexical_form(Val^^D, Lex) :- !,
  rdf11:rdf_lexical_form(Val^^D, Lex^^D).
rdf_literal_lexical_form(Val@_, Val).



%! rdf_literal_ltag(+Lit, -LTag) is semidet.

rdf_literal_ltag(_@LTag, LTag).



%! rdf_literal_string(@Term) is semidet.

rdf_literal_string(V^^xsd:string, V) :- !.
rdf_literal_string(V@_, V).



%! rdf_literal_value(+Lit, +Val) is semidet.
%! rdf_literal_value(+Lit, -Val) is nondet.

rdf_literal_value(Val^^_, Val).
rdf_literal_value(Val@_, Val).



%! rdf_lone_bnode(+M, ?BNode) is nondet.
%! rdf_lone_bnode(+M, ?BNode, ?G) is nondet.

rdf_lone_bnode(M, BNode) :-
  rdf_lone_bnode(M, BNode, _).


rdf_lone_bnode(M, BNode, G) :-
  rdf_bnode(M, BNode, G),
  \+ q(M, BNode, _, _, G).



%! rdf_lts(+M, ?Lit) is nondet.
%! rdf_lts(+M, ?Lts, ?G) is nondet.

rdf_lts(hdt, Lit) :-
  hdt_lts(Lit).
rdf_lts(trp, Lit) :-
  trp_lts(Lit).


rdf_lts(hdt, Lit, G) :-
  hdt_lts(Lit, G).
rdf_lts(hdt0, Lit, Hdt) :-
  hdt_lts0(Lit, Hdt).
rdf_lts(trp, Lit, G) :-
  trp_lts(Lit, G).



%! rdf_name(+M, ?Name) is nondet.
%! rdf_name(+M, ?Name, ?G) is nondet.

rdf_name(hdt, Name) :-
  hdt_name(Name).
rdf_name(trp, Name) :-
  rdf11:rdf_name(Name).


rdf_name(hdt, Name, G) :-
  hdt_name(Name, G).
rdf_name(hdt0, Name, Hdt) :-
  hdt_name0(Name, Hdt).
rdf_name(trp, Name, G) :-
  trp_name(Name, G).



%! rdf_node(+M, ?Node) is nondet.
%! rdf_node(+M, ?Node, ?G) is nondet.

rdf_node(hdt, Node) :-
  hdt_node(Node).
rdf_node(trp, Node) :-
  rdf11:rdf_node(Node).


rdf_node(hdt, Node, G) :-
  hdt_node(Node, G).
rdf_node(hdt0, Node, Hdt) :-
  hdt_node0(Node, Hdt).
rdf_node(trp, Node, G) :-
  trp_node(Node, G).



%! rdf_object(+M, ?O) is nondet.
%! rdf_object(+M, ?O, ?G) is nondet.

rdf_object(hdt, O) :-
  hdt_object(O).
rdf_object(trp, O) :-
  rdf11:rdf_object(O).


rdf_object(hdt, O, G) :-
  hdt_object(O, G).
rdf_object(hdt0, O, Hdt) :-
  hdt_object0(O, Hdt).
rdf_object(trp, O, G) :-
  trp_object(O, G).



%! rdf_predicate(+M, ?P) is nondet.
%! rdf_predicate(+M, ?P, ?G) is nondet.

rdf_predicate(hdt, P) :-
  hdt_predicate(P).
rdf_predicate(trp, P) :-
  rdf11:rdf_predicate(P).


rdf_predicate(hdt, P, G) :-
  hdt_predicate(P, G).
rdf_predicate(hdt0, P, Hdt) :-
  hdt_predicate0(P, Hdt).
rdf_predicate(trp, P, G) :-
  trp_predicate(P, G).



%! rdf_prefix(?Prefix) is nondet.

rdf_prefix(Prefix) :-
  rdf_alias_prefix(_, Prefix).



%! rdf_property(+M, ?Prop) is nondet.
%! rdf_property(+M, ?Prop, ?G) is nondet.

rdf_property(M, Prop) :-
  distinct(Prop, rdf_property(M, Prop, _)).


rdf_property(M, Prop, G) :-
  distinct(Prop, rdf_property0(M, Prop, G)).

rdf_property0(M, Prop, G) :-
  rdf_predicate(M, Prop, G).
rdf_property0(M, Prop, G) :-
  rdf_domain(M, Prop, _, G).
rdf_property0(M, Prop, G) :-
  rdf_range(M, Prop, _, G).



%! rdf_query_term(+Term, -QueryTerm) is det.
%! rdf_query_term(+Key, +Val, -QueryTerm) is det.

rdf_query_term(Term, QueryTerm) :-
  Term =.. [Key,Val],
  rdf_query_term(Key, Val, QueryTerm).


rdf_query_term(Key, Val, QueryTerm) :-
  rdf_term_to_atom(Val, A),
  QueryTerm =.. [Key,A].



%! rdf_snap(:Goal_0) .
%
% Call Goal_0 inside an RDF snapshot.

rdf_snap(Goal_0) :-
  rdf_transaction(Goal_0, _, [snapshot(true)]).



%! rdf_subject(+M, ?S) is nondet.
%! rdf_subject(+M, ?S, ?G) is nondet.

rdf_subject(hdt, S) :-
  hdt_subject(S).
rdf_subject(trp, S) :-
  rdf11:rdf_subject(S).


rdf_subject(hdt, S, G) :-
  hdt_subject(S, G).
rdf_subject(hdt0, S, Hdt) :-
  hdt_subject0(S, Hdt).
rdf_subject(trp, S, G) :-
  trp_subject(S, G).



%! rdf_term(+M, ?Term) is nondet.
%! rdf_term(+M, ?Term, ?G) is nondet.

rdf_term(hdt, Term) :-
  hdt_term(Term).
rdf_term(trp, Term) :-
  rdf11:rdf_term(Term).


rdf_term(hdt, Term, G) :-
  hdt_term(Term, G).
rdf_term(hdt0, Term, Hdt) :-
  hdt_term0(Term, Hdt).
rdf_term(trp, Term, G) :-
  trp_term(Term, G).



%! rdf_term_expansion(+Atom, -Term) is det.

rdf_term_expansion(X, X) :-
  is_http_iri(X), !.
rdf_term_expansion(X, Y) :-
  atom(X),
  atomic_list_concat([Alias,Local], :, X),
  rdf_alias(Alias), !,
  rdf_global_id(Alias:Local, Y).
rdf_term_expansion(X, X).



%! rdf_term_to_atom(+Term, -Atom) is det.

rdf_term_to_atom(X, X) :-
  var(X), !.
rdf_term_to_atom(Val^^D, A) :- !,
  rdf11:in_ground_type(D, Val, Lex),
  format(atom(A), '"~a"^^<~a>', [Lex,D]).
rdf_term_to_atom(Lex@LTag, A) :- !,
  format(atom(A), '"~a"@~a', [Lex,LTag]).
rdf_term_to_atom(A, A) :-
  atom(A).
