:- module(
  q_stmt,
  [
    q/4,                  % ?M, ?S, ?P, ?O
    q/5,                  % ?M, ?S, ?P, ?O, ?G
    q_is_ground_quad/1,   % @Term
    q_is_ground_triple/1, % @Term
    q_is_quad/1,          % @Term
    q_is_triple/1,        % @Term
    q_lts/4,              % ?M, ?S, ?P, ?Lit
    q_lts/5,              % ?M, ?S, ?P, ?Lit, ?G
    q_lts/6,              % ?M, ?S, ?P, +LRange, ?Lit, -G
    q_pref_label/3,       % ?M, ?S, ?Lit
    q_pref_label/4,       % ?M, ?S, ?Lit, ?G
    q_pref_lex/4,         % ?M, ?S, ?P, ?Lex
    q_pref_lex/5,         % ?M, ?S, ?P, ?Lex, ?G
    q_pref_string/4,      % ?M, ?S, ?P, ?Lit
    q_pref_string/5,      % ?M, ?S, ?P, ?Lit, ?G
    q_quad/2,             % ?M, -Quad
    q_quad/3,             % ?M, ?G, -Quad
    q_quad/5,             % ?M, ?S, ?P, ?O, -Quad
    q_quad/6,             % ?M, ?S, ?P, ?O, ?G, -Quad
    q_quad_datatype/2,    % +Quad, -D
    q_quad_graph/2,       % +Quad, -G
    q_quad_iri/2,         % +Quad, -Iri
    q_quad_object/2,      % +Quad, -O
    q_quad_predicate/2,   % +Quad, -P
    q_quad_subject/2,     % +Quad, -S
    q_quad_term/2,        % +Quad, -Term
    q_quad_terms/5,       % ?Quad, ?S, ?P, ?O, ?G
    q_quads/2,            % ?M, -Quads
    q_quads/3,            % ?M, ?G, -Quads
    q_quads/5,            % ?M, ?S, ?P, ?O, -Quads
    q_quads/6,            % ?M, ?S, ?P, ?O, ?G, -Quads
    q_reification/4,      % ?M, ?S, ?P, ?O
    q_reification/5,      % ?M, ?S, ?P, ?O, ?G
    q_reification/6,      % ?M, ?S, ?P, ?O, ?G, -Stmt
    q_triple/2,           % ?M, -Triple
    q_triple/3,           % ?M, ?G, -Triple
    q_triple/5,           % ?M, ?S, ?P, ?O, -Triple
    q_triple/6,           % ?M, ?S, ?P, ?O, ?G, -Triple
    q_triple_datatype/2,  % +Triple, -D
    q_triple_iri/2,       % +Triple, -Iri
    q_triple_object/2,    % +Triple, -O
    q_triple_predicate/2, % +Triple, -P
    q_triple_subject/2,   % +Triple, -S
    q_triple_term/2,      % +Triple, -Term
    q_triple_terms/4,     % ?Triple, ?S, ?P, ?O
    q_triples/2,          % ?M, -Triples
    q_triples/3,          % ?M, ?G, -Triples
    q_triples/5,          % ?M, ?S, ?P, ?O, -Triples
    q_triples/6           % ?M, ?S, ?P, ?O, ?G, -Triples
  ]
).

/** <module> Quine statements API

Enumerate statements from different back-ends.

Perform basic RDF statement manipulations: statement ↔ terms

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(aggregate)).
:- use_module(library(hdt/hdt_stmt)).
:- use_module(library(lists)).
:- use_module(library(nlp/nlp_lang)).
:- use_module(library(q/q_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(yall)).

:- rdf_meta
   q(?, r, r, o),
   q(?, r, r, o, r),
   q_lts(?, r, r, -),
   q_lts(?, r, r, r, -),
   q_lts(?, r, r, +, r, -),
   q_pref_label(?, r, -),
   q_pref_label(?, r, r, -),
   q_pref_lex(?, r, r, -),
   q_pref_lex(?, r, r, r, -),
   q_pref_string(?, r, r, -),
   q_pref_string(?, r, r, r, -),
   q_quad(?, r, -),
   q_quad(?, r, r, o, -),
   q_quad(?, r, r, o, r, -),
   q_quad_datatype(t, r),
   q_quad_graph(t, r),
   q_quad_iri(t, r),
   q_quad_object(t, o),
   q_quad_predicate(t, r),
   q_quad_subject(t, r),
   q_quad_term(t, o),
   q_quad_terms(t, r, r, o, r),
   q_quads(?, r, -),
   q_quads(?, r, r, o, -),
   q_quads(?, r, r, o, r, -),
   q_reification(?, r, r, o),
   q_reification(?, r, r, o, r),
   q_reification(?, r, r, o, r, r),
   q_triple(?, r, -),
   q_triple(?, r, r, o, -),
   q_triple(?, r, r, o, r, -),
   q_triple_datatype(t, r),
   q_triple_iri(t, r),
   q_triple_object(t, o),
   q_triple_predicate(t, r),
   q_triple_subject(t, r),
   q_triple_term(t, o),
   q_triple_terms(t, r, r, o),
   q_triples(?, r, -),
   q_triples(?, r, r, o, -),
   q_triples(?, r, r, o, r, -).





%! q(?M, ?S, ?P, ?O) is nondet.
%! q(?M, ?S, ?P, ?O, ?G) is nondet.

q(M, S, P, O) :-
  q(M, S, P, O, _).


q(rdf, S, P, O, G) :-
  rdf(S, P, O, G).
q(hdt, S, P, O, G) :-
  hdt(S, P, O, G).



%! q_is_ground_quad(@Term) is semidet.
%
% Succeeds if the given triple is ground, i.e., contains no blank
% node.

q_is_ground_quad(rdf(S,P,O,_)) :-
  q_is_ground_triple(rdf(S,P,O)).



%! q_is_ground_triple(@Term) is semidet.
%
% Succeeds if the given triple is ground, i.e., contains no blank
% node.

q_is_ground_triple(rdf(S,_,O)) :-
  \+ q_is_bnode(S),
  \+ q_is_bnode(O).



%! q_is_quad(@Term) is semidet.

q_is_quad(rdf(_,_,_,_)).



%! q_is_triple(@Term) is semidet.

q_is_triple(rdf(_,_,_)).



%! q_lts(?M, ?S, ?P, -Lit) is nondet.
%! q_lts(?M, ?S, ?P, -Lit, ?G) is nondet.
%! q_lts(?M, ?S, ?P, +LRange, -Lit, ?G) is nondet.
%
% Matches RDF statements whose object term is a language-tagged string
% that mathes the given language priory list.  Notice that results for
% each of the prioritized languages are given in arbitrary order.

q_lts(M, S, P, Lit) :-
  q_lts(M, S, P, Lit, _).


q_lts(M, S, P, Lit, G) :-
  current_lrange(LRange),
  q_lts(M, S, P, LRange, Lit, G).


q_lts(M, S, P, LRange, Lit, G) :-
  q(M, S, P, V@LTag, G),
  basic_filtering(LRange, LTag),
  Lit = V@LTag.



%! q_pref_label(?M, ?S, ?Lit) is nondet.
%! q_pref_label(?M, ?S, ?Lit, ?G) is nondet.

q_pref_label(M, S, Lit) :-
  q_pref_label(M, S, Lit, _).


q_pref_label(M, S, Lit, G) :-
  q_pref_string(M, S, rdfs:label, Lit, G).



%! q_pref_lex(?M, ?S, ?P, -Lex) is nondet.
%! q_pref_lex(?M, ?S, ?P, -Lex, ?G) is nondet.
%
% Like q_pref_string/[4,5], but returns only the lexical form.

q_pref_lex(M, S, P, Lex) :-
  q_pref_lex(M, S, P, Lex, _).


q_pref_lex(M, S, P, Lex, G) :-
  q_pref_string(M, S, P, Lit, G),
  q_literal_lex(Lit, Lex).



%! q_pref_string(?M, ?S, ?P, ?Lit) is nondet.
%! q_pref_string(?M, ?S, ?P, ?Lit, ?G) is nondet.
%
% Returns, in this exact order:
%
%   1. The language-tagged strings that match the given language
%   priority list; returning results for higher priority language
%   earlier.
%
%   2. The language-tagged strings that do not match the given
%   language priority list.
%
%   3. XSD strings.

q_pref_string(M, S, P, Lit) :-
  q_pref_string(M, S, P, Lit, _).


q_pref_string(M, S, P, Lit, G) :-
  current_lrange(LRange),
  q_pref_string(M, S, P, LRange, Lit, G).


% Matching language-tagged strings.
q_pref_string(M, S, P, LRange, Lit, G) :-
  q_lts(M, S, P, LRange, Lit, G).
% Non-matching language-tagged strings.
q_pref_string(M, S, P, LRange, Lit, G) :-
  q(M, S, P, V@LTag, G),
  % Avoid duplicates.
  \+ basic_filtering(LRange, LTag),
  Lit = V@LTag.
% Plain XSD strings.
q_pref_string(M, S, P, _, V^^xsd:string, G) :-
  q(M, S, P, V^^xsd:string, G).



%! q_quad(?M, -Quad) is nondet.
%! q_quad(?M, ?G, -Quad) is nondet.
%! q_quad(?M, ?S, ?P, ?O, -Quad) is nondet.
%! q_quad(?M, ?S, ?P, ?O, ?G, -Quad) is nondet.

q_quad(M, Quad) :-
  q_quad(M, _, Quad).


q_quad(M, G, Quad) :-
  q_quad(M, _, _, _, G, Quad).


q_quad(M, S, P, O, Quad) :-
  q_quad(M, S, P, O, _, Quad).


q_quad(M, S, P, O, G, Quad) :-
  q(M, S, P, O, G),
  Quad = rdf(S, P, O, G).



%! q_quad_datatype(+Quad, -D) is det.
%! q_quad_graph(+Quad, -G) is det.
%! q_quad_iri(+Quad, -Iri) is det.
%! q_quad_object(+Quad, -O) is det.
%! q_quad_predicate(+Quad, -P) is det.
%! q_quad_subject(+Quad, -S) is det.
%! q_quad_term(+Quad, -Term) is nondet.

q_quad_datatype(rdf(_,_,O,_), D) :-
  q_literal_datatype(O, D).


q_quad_graph(rdf(_,_,_,G), G).


q_quad_iri(Quad, Iri) :-
  q_quad_term(Quad, Iri),
  q_is_iri(Iri).


q_quad_object(rdf(_,_,O,_), O).


q_quad_predicate(rdf(_,P,_,_), P).


q_quad_subject(rdf(S,_,_,_), S).


q_quad_term(rdf(S,_,_,_), S).
q_quad_term(rdf(_,P,_,_), P).
q_quad_term(rdf(_,_,O,_), O).
q_quad_term(rdf(_,_,_,G), G).



%! q_quad_terms(?Quad, ?S, ?P, ?O, ?G) is det.

q_quad_terms(rdf(S,P,O,G), S, P, O, G).



%! q_quads(?M, -Quads) is nondet.
%! q_quads(?M, ?G, -Quads) is nondet.
%! q_quads(?M, ?S, ?P, ?O, -Quads) is nondet.
%! q_quads(?M, ?S, ?P, ?O, ?G, -Quads) is nondet.

q_quads(M, Quads) :-
  q_quads(M, _, Quads).


q_quads(M, G, Quads) :-
  q_quads(M, _, _, _, G, Quads).


q_quads(M, S, P, O, Quads) :-
  q_quads(M, S, P, O, _, Quads).


q_quads(M, S, P, O, G, Quads) :-
  aggregate_all(set(Quad), q_quad(M, S, P, O, G, Quad), Quads).



%! q_reification(?M, ?S, ?P, ?O) is nondet.
%! q_reification(?M, ?S, ?P, ?O, ?G) is nondet.
%! q_reification(?M, ?S, ?P, ?O, ?G, -Stmt) is nondet.

q_reification(M, S, P, O) :-
  q_reification(M, S, P, O, _).


q_reification(M, S, P, O, G) :-
  q_reification(M, S, P, O, G, _).


q_reification(M, S, P, O, G, Stmt) :-
  q(M, Stmt, rdf:subject, S, G),
  q(M, Stmt, rdf:predicate, P, G),
  q(M, Stmt, rdf:object, O, G).



%! q_triple(?M, ?Triple) is nondet.
%! q_triple(?M, ?G, ?Triple) is nondet.
%! q_triple(?M, ?S, ?P, ?O, ?Triple) is nondet.
%! q_triple(?M, ?S, ?P, ?O, ?G, ?Triple) is nondet.

q_triple(M, Triple) :-
  q_triple(M, _, Triple).


q_triple(M, G, Triple) :-
  q_triple(M, _, _, _, G, Triple).


q_triple(M, S, P, O, Triple) :-
  q_triple(M, S, P, O, _, Triple).


q_triple(M, S, P, O, G, Triple) :-
  q(M, S, P, O, G),
  Triple = rdf(S, P, O).



%! q_triple_datatype(+Triple, -D) is det.
%! q_triple_iri(+Triple, -Iri) is nondet.
%! q_triple_object(+Triple, -O) is det.
%! q_triple_predicate(+Triple, -P) is det.
%! q_triple_subject(+Triple, -S) is det.
%! q_triple_term(+Triple, -Term) is nondet.

q_triple_datatype(rdf(_,_,O), D) :-
  q_literal_datatype(O, D).


q_triple_iri(Triple, Iri) :-
  q_triple_term(Triple, Iri),
  q_is_iri(Iri).


q_triple_object(rdf(_,_,O,_), O).


q_triple_predicate(rdf(_,P,_,_), P).


q_triple_subject(rdf(S,_,_,_), S).


q_triple_term(rdf(S,_,_), S).
q_triple_term(rdf(_,P,_), P).
q_triple_term(rdf(_,_,O), O).



%! q_triple_terms(?Triple, ?S, ?P, ?O) is det.

q_triple_terms(rdf(S,P,O), S, P, O).



%! q_triples(?M, -Triple) is nondet.
%! q_triples(?M, ?G, -Triple) is nondet.
%! q_triples(?M, ?S, ?P, ?O, -Triple) is nondet.
%! q_triples(?M, ?S, ?P, ?O, ?G, -Triple) is nondet.

q_triples(M, Triples) :-
  q_triples(M, _, Triples).


q_triples(M, G, Triples) :-
  q_triples(M, _, _, _, G, Triples).


q_triples(M, S, P, O, Triples) :-
  q_triples(M, S, P, O, _, Triples).


q_triples(M, S, P, O, G, Triples) :-
  aggregate_all(set(Triple), q_triple(M, S, P, O, G, Triple), Triples).





% HELPERS %

%! basic_filtering(
%!   +LanguagePriorityList:list(atom),
%!   +LanguageTag:atom
%! ) is semidet.
% Succeeds if the LanguagePriorityList matches the LanguageTag according to
% the basic filtering algorithm described in RFC 4647,
% i.e., if the former is a case-insensitive prefix of the latter,
% while also treating the `*` sign as a wildcard.
%
% @compat RFC 4647

basic_filtering(Ranges, Tag):-
  % NONDET
  member(Range, Ranges),
  atomic_list_concat(Subtags1, -, Range),
  atomic_list_concat(Subtags2, -, Tag),
  basic_filtering0(Subtags1, Subtags2), !.


basic_filtering0(_, []).
basic_filtering0([H1|T1], [H2|T2]):-
  subtag_match(H1, H2),
  basic_filtering0(T1, T2).



%! graph_file(+G, -File) is det.

graph_file(G, File) :-
  rdf_global_id(Alias:Local, G),
  directory_file_path(Alias, Local, Base),
  file_name_extension(Base, nt, File).



%! subtag_match(+RangeSubtag:atom, +Subtag:atom) is semidet.
% Two subtags match if either they are the same when compared
% case-insensitively or the language range's subtag is the wildcard `*`

subtag_match(*, _):- !.
subtag_match(X1, X2):-
  downcase_atom(X1, X),
  downcase_atom(X2, X).
