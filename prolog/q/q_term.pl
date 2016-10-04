:- module(
  q_term,
  [
    q_aggregate_all/3,     % +Template, :Goal_0, -Result
    q_alias/1,             % ?Alias
    q_alias_domain/2,      % +Alias, -Domain
   %q_alias_prefix/2,      % ?Alias, ?Prefix
    q_bnode/2,             % ?M, ?B
    q_bnode/3,             % ?M, ?B, ?G
    q_datatype/2,          % ?M, ?D
    q_datatype/3,          % ?M, ?D, ?G
    q_defval/2,            % +Def, -Val
    q_default_graph/1,     % ?G
    q_iri/2,               % ?M, ?Iri
    q_iri/3,               % ?M, ?Iri, ?G
    q_iri_alias/2,         % +Iri, -Alias
    q_iri_alias_local/3,   % +Iri, ?Alias, ?Local
    q_iri_alias_prefix/3,  % +Iri, ?Alias, ?Prefix
    q_iri_local/2,         % +Iri, -Local
    q_iri_prefix/2,        % +Iri, -Prefix
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
    q_legacy_literal/4,    % +Lit, -D, -Lex, -LTag
    q_literal/2,           % ?M, ?Lit
    q_literal/3,           % ?M, ?Lit, ?G
    q_literal/4,           % ?Lit, ?D, ?Lex, ?LTag
    q_literal_datatype/2,  % +Lit, ?D
    q_literal_lex/2,       % +Lit, ?Lex
    q_literal_ltag/2,      % +Lit, -LTag
    q_literal_string/2,    % +Lit, -Str
    q_literal_val/2,       % +Lit, ?Val
    q_lone_bnode/2,        % ?M, ?B
    q_lone_bnode/3,        % ?M, ?B, ?G
    q_lts/2,               % ?M, ?Lit
    q_lts/3,               % ?M, ?Lit, ?G
    q_member/2,            % ?Elem, +L
    q_memberchk/2,         % ?Elem, +L
    q_name/2,              % ?M, ?Name
    q_name/3,              % ?M, ?Name, ?G
    q_node/2,              % ?M, ?Node
    q_node/3,              % ?M, ?Node, ?G
    q_object/2,            % ?M, ?O
    q_object/3,            % ?M, ?O, ?G
    q_predicate/2,         % ?M, ?P
    q_predicate/3,         % ?M, ?P, ?G
    q_prefix/1,            % ?Prefix
    q_query_term/2,        % +Term, -QueryTerm
    q_query_term/3,        % +Key, +Val, -QueryTerm
    q_snap/1,              % :Goal_0
    q_subject/2,           % ?M, ?S
    q_subject/3,           % ?M, ?S, ?G
    q_term/2,              % ?M, ?Term
    q_term/3,              % ?M, ?Term, ?G
    q_term_expansion/2,    % +Atom, -Term
   %q_transaction/1,       % :Goal_0
   %q_transaction/2,       % :Goal_0, +Id
   %q_transaction/3,       % :Goal_0, +Is, +Opts
    q_type/3,              % +M, +G, -C
    ll_is_bnode/1          % @Term
  ]
).
:- reexport(library(semweb/rdf_db), [
     rdf_current_prefix/2 as q_alias_prefix,
     rdf_equal/2,
     rdf_global_id/2,
     rdf_global_object/2,
     rdf_global_term/2,
     (rdf_meta)/1
   ]).
:- reexport(library(semweb/rdf11), [
     rdf_default_graph/1 as q_default_graph,
     rdf_is_bnode/1 as q_is_bnode,
     rdf_is_iri/1 as q_is_iri,
     rdf_is_literal/1 as q_is_literal,
     rdf_is_name/1 as q_is_name,
     rdf_is_object/1 as q_is_object,
     rdf_is_predicate/1 as q_is_predicate,
     rdf_is_subject/1 as q_is_subject,
     rdf_is_term/1 as q_is_term,
     rdf_transaction/1 as q_transaction,
     rdf_transaction/2 as q_transaction,
     rdf_transaction/3 as q_transaction,
     op(110, xfx, @),
     op(650, xfx, ^^)
   ]).

/** <module> Quine term API

@author Wouter Beek
@version 2016/06, 2016/08, 2016/10
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(hdt/hdt_ext)).
:- use_module(library(q/q_rdf)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(solution_sequences)).
:- use_module(library(typecheck)).


:- meta_predicate
    q_aggregate_all(+, 0, -),
    q_snap(0).


:- multifile
    http:convert_parameter/3.


http:convert_parameter(q_iri, A, A) :- !.
http:convert_parameter(q_literal, A, Term) :- !,
  atom_phrase(q_term0(Term), A).
http:convert_parameter(q_term, A, Term) :-
  (   http:convert_parameter(q_literal, A, Term)
  ->  true
  ;   http:convert_parameter(q_iri, A, Term)
  ).

q_term0(Term) -->
  "\"", !,
  q_lex0([0'"], Cs), %"
  (   "@"
  ->  q_ltag0(LTag),
      {
        string_codes(Str, Cs),
        Term = Str@LTag
      }
  ;   "^^",
      q_iri0(D),
      {
        atom_codes(Lex, Cs),
        rdf11:out_type(D, Val, Lex),
        Term = Val^^D
      }
  ).
q_term0(Iri) -->
  q_iri0(Iri).

q_ltag0(LTag) -->
  rest(Cs),
  {atom_codes(LTag, Cs)}.

q_lex0(End, L)     --> "\\\'", !, q_lex0(End, L).
q_lex0(End, L)     --> "\\\"", !, q_lex0(End, L).
q_lex0(End, [])    --> End,    !.
q_lex0(End, [H|T]) --> [H],    !, q_lex0(End, T).
q_lex0(_,   [])    --> "".

q_iri0(Iri) -->
  "<",
  '...'(Cs),
  ">",
  {atom_codes(Iri, Cs)}.


:- rdf_meta
   q_aggregate_all(+, t, -),
   q_bnode(?, ?, r),
   q_datatype(?, r),
   q_datatype(?, r, r),
   q_defval(r, -),
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
   q_member(r, t),
   q_memberchk(r, t),
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





%! q_aggregate_all(+Template, :Goal_0, -Result) is det.
%
% Calls aggregate_all/3 under RDF alias expansion.

q_aggregate_all(Template, Goal_0, Result) :-
  aggregate_all(Template, Goal_0, Result).



%! q_alias(?Alias) is nondet.

q_alias(Alias) :-
  q_alias_prefix(Alias, _).



%! q_alias_domain(+Alias, -Domain) is det.

q_alias_domain(Alias, Domain) :-
  q_alias_prefix(Alias, Prefix),
  uri_components(Prefix, uri_components(_,Domain,_,_,_)).



%! q_bnode(?M, ?B) is nondet.
%! q_bnode(?M, ?B, ?G) is nondet.

q_bnode(hdt, B) :-
  hdt_bnode(B).
q_bnode(trp, B) :-
  rdf_bnode(B).


q_bnode(hdt, B, G) :-
  hdt_bnode(B, G).
q_bnode(hdt0, B, Hdt) :-
  hdt_bnode0(B, Hdt).
q_bnode(trp, B, G) :-
  rdf_bnode(B, G).



%! q_datatype(?M, ?D) is nondet.
%! q_datatype(?M, ?D, ?G) is nondet.

q_datatype(hdt, D) :-
  hdt_datatype(D).
q_datatype(trp, D) :-
  rdf_datatype(D).


q_datatype(hdt, D, G) :-
  hdt_datatype(D, G).
q_datatype(hdt0, D, Hdt) :-
  hdt_datatype0(D, Hdt).
q_datatype(trp, D, G) :-
  rdf_datatype(D, G).



%! q_defval(+Def, -Val) is det.

q_defval(_, X) :-
  nonvar(X), !.
q_defval(X, X).



%! q_iri(?M, ?Iri) is nondet.
%! q_iri(?M, ?Iri, ?G) is nondet.

q_iri(hdt, Iri) :-
  hdt_iri(Iri).
q_iri(trp, Iri) :-
  rdf_iri(Iri).


q_iri(hdt, Iri, G) :-
  hdt_iri(Iri, G).
q_iri(hdt0, Iri, Hdt) :-
  hdt_iri0(Iri, Hdt).
q_iri(trp, Iri, G) :-
  rdf_iri(Iri, G).



%! q_iri_alias(+Iri, -Alias) is nondet.

q_iri_alias(Iri, Alias) :-
  rdf_global_id(Alias:_, Iri).



%! q_iri_alias_local(+Iri, -Alias, -Local) is det.
%! q_iri_alias_local(-Iri, +Alias, +Local) is det.

q_iri_alias_local(Iri, Alias, Local) :-
  rdf_global_id(Alias:Local, Iri).



%! q_iri_alias_prefix(+Iri, -Alias, -Prefix) is nondet.
%! q_iri_alias_prefix(-Iri, +Alias, +Prefix) is det.

q_iri_alias_prefix(Iri, Alias, Prefix) :-
  q_iri_alias(Iri, Alias),
  q_alias_prefix(Alias, Prefix).



%! q_iri_local(+Iri, -Local) is det.

q_iri_local(Iri, Local) :-
  rdf_global_id(_:Local, Iri).



%! q_iri_prefix(+Iri, -Prefix) is nondet.

q_iri_prefix(Iri, Prefix) :-
  rdf_global_id(_:Prefix, Iri).



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

q_legacy_literal(literal(type(D,Dom)), D, Dom, _) :-
  q_memberchk(D, [rdf:'HTML',rdf:'XMLLiteral']), !.
q_legacy_literal(literal(type(D,Lex0)), D, Lex, _) :-
  \+ rdf_equal(rdf:langString, D), !,
  atom_string(Lex0, Lex).
q_legacy_literal(literal(lang(LTag,Lex0)), rdf:langString, Lex, LTag) :- !,
  atom_string(Lex0, Lex).
q_legacy_literal(literal(Lex0), xsd:string, Lex, _) :-
  atom_string(Lex0, Lex).



%! q_literal(?M, ?Lit) is nondet.
%! q_literal(?M, ?Lit, ?G) is nondet.

q_literal(hdt, Lit) :-
  hdt_literal(Lit).
q_literal(trp, Lit) :-
  rdf_literal(Lit).


q_literal(hdt, Lit, G) :-
  hdt_literal(Lit, G).
q_literal(hdt0, Lit, Hdt) :-
  hdt_literal0(Lit, Hdt).
q_literal(trp, Lit, G) :-
  rdf_literal(Lit, G).



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



%! q_literal_ltag(+Lit, -LTag) is semidet.

q_literal_ltag(_@LTag, LTag).



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

q_lts(hdt, Lit) :-
  hdt_lts(Lit).
q_lts(trp, Lit) :-
  rdf_lts(Lit).


q_lts(hdt, Lit, G) :-
  hdt_lts(Lit, G).
q_lts(hdt0, Lit, Hdt) :-
  hdt_lts0(Lit, Hdt).
q_lts(trp, Lit, G) :-
  rdf_lts(Lit, G).



%! q_member(?Elem, +L) is nondet.
%
% Calls member/2 under RDF alias expansion.

q_member(Elem, L) :-
  member(Elem, L).



%! q_memberchk(?Elem, +L) is nondet.
%
% Calls memberchk/2 under RDF alias expansion.

q_memberchk(Elem, L) :-
  memberchk(Elem, L).



%! q_name(?M, ?Name) is nondet.
%! q_name(?M, ?Name, ?G) is nondet.

q_name(hdt, Name) :-
  hdt_name(Name).
q_name(trp, Name) :-
  rdf_name(Name).


q_name(hdt, Name, G) :-
  hdt_name(Name, G).
q_name(hdt0, Name, Hdt) :-
  hdt_name0(Name, Hdt).
q_name(trp, Name, G) :-
  rdf_name(Name, G).



%! q_node(?M, ?Node) is nondet.
%! q_node(?M, ?Node, ?G) is nondet.

q_node(hdt, Node) :-
  hdt_node(Node).
q_node(trp, Node) :-
  rdf_node(Node).


q_node(hdt, Node, G) :-
  hdt_node(Node, G).
q_node(hdt0, Node, Hdt) :-
  hdt_node0(Node, Hdt).
q_node(trp, Node, G) :-
  rdf_node(Node, G).



%! q_object(?M, ?O) is nondet.
%! q_object(?M, ?O, ?G) is nondet.

q_object(hdt, O) :-
  hdt_object(O).
q_object(trp, O) :-
  rdf_object(O).


q_object(hdt, O, G) :-
  hdt_object(O, G).
q_object(hdt0, O, Hdt) :-
  hdt_object0(O, Hdt).
q_object(trp, O, G) :-
  rdf_object(O, G).



%! q_predicate(?M, ?P) is nondet.
%! q_predicate(?M, ?P, ?G) is nondet.

q_predicate(hdt, P) :-
  hdt_predicate(P).
q_predicate(trp, P) :-
  rdf_predicate(P).


q_predicate(hdt, P, G) :-
  hdt_predicate(P, G).
q_predicate(hdt0, P, Hdt) :-
  hdt_predicate0(P, Hdt).
q_predicate(trp, P, G) :-
  rdf_predicate(P, G).



%! q_prefix(?Prefix) is nondet.

q_prefix(Prefix) :-
  q_alias_prefix(_, Prefix).



%! q_query_term(+Term, -QueryTerm) is det.
%! q_query_term(+Key, +Val, -QueryTerm) is det.

q_query_term(Term, QueryTerm) :-
  Term =.. [Key,Val],
  q_query_term(Key, Val, QueryTerm).


q_query_term(Key, Val, QueryTerm) :-
  q_write_query_term(Val, A),
  QueryTerm =.. [Key,A].


q_write_query_term(Val^^D, A) :- !,
  rdf11:in_ground_type(D, Val, Lex),
  format(atom(A), '"~a"^^<~a>', [Lex,D]).
q_write_query_term(Lex@LTag, A) :- !,
  format(atom(A), '"~a"@~a', [Lex,LTag]).
q_write_query_term(A, A).



%! q_snap(:Goal_0) .
%
% Call Goal_0 inside an RDF snapshot.

q_snap(Goal_0) :-
  q_transaction(Goal_0, _, [snapshot(true)]).



%! q_subject(?M, ?S) is nondet.
%! q_subject(?M, ?S, ?G) is nondet.

q_subject(hdt, S) :-
  hdt_subject(S).
q_subject(trp, S) :-
  rdf_subject(S).


q_subject(hdt, S, G) :-
  hdt_subject(S, G).
q_subject(hdt0, S, Hdt) :-
  hdt_subject0(S, Hdt).
q_subject(trp, S, G) :-
  rdf_subject(S, G).



%! q_term(?M, ?Term) is nondet.
%! q_term(?M, ?Term, ?G) is nondet.

q_term(hdt, Term) :-
  hdt_term(Term).
q_term(trp, Term) :-
  rdf_term(Term).


q_term(hdt, Term, G) :-
  hdt_term(Term, G).
q_term(hdt0, Term, Hdt) :-
  hdt_term0(Term, Hdt).
q_term(trp, Term, G) :-
  rdf_term(Term, G).



%! q_term_expansion(+Atom, -Term) is det.

q_term_expansion(X, Y) :-
  atom(X),
  atomic_list_concat([Alias,Local], :, X),
  q_alias(Alias), !,
  rdf_global_id(Alias:Local, Y).
q_term_expansion(X, X).



%! q_type(+M, +G, -C) is nondet.
%
% Generates classes for which there is at least one instance in graph
% G.
%
% @tbd Add entailment.

q_type(M, G, C) :-
  distinct(C, (
    q_subject(M, I, G),
    q_instance(M, I, C)
  )).



%! ll_is_bnode(@Term) is semidet.

ll_is_bnode(B) :-
  atom_prefix(B, 'http://lodlaundromat.org/.well-known/genid/').
