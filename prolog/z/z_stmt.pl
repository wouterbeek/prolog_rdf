:- module(
  z_stmt,
  [
    z/4,                  % ?M, ?S, ?P, ?O
    z/5,                  % ?M, ?S, ?P, ?O, ?G
    z_lts/4,              % ?M, ?S, ?P, -Lit
    z_lts/5,              % ?M, ?S, ?P, ?G, -Lit
    z_lts/6,              % ?M, ?S, ?P, +LRange, ?G, -Lit
    z_pref_label/3,       % ?M, ?S, -Lit
    z_pref_label/4,       % ?M, ?S, ?G, -Lit
    z_pref_lex/4,         % ?M, ?S, ?P, -Lex
    z_pref_lex/5,         % ?M, ?S, ?P, ?G, -Lex
    z_pref_string/4,      % ?M, ?S, ?P, -Lit
    z_pref_string/5,      % ?M, ?S, ?P, ?G, -Lit
    z_quad/2,             % ?M, -Quad
    z_quad/3,             % ?M, ?G, -Quad
    z_quad/5,             % ?M, ?S, ?P, ?O, -Quad
    z_quad/6,             % ?M, ?S, ?P, ?O, ?G, -Quad
    z_quad_datatype/2,    % +Quad, -D
    z_quad_graph/2,       % +Quad, -G
    z_quad_iri/2,         % +Quad, -Iri
    z_quad_object/2,      % +Quad, -O
    z_quad_predicate/2,   % +Quad, -P
    z_quad_subject/2,     % +Quad, -S
    z_quad_term/2,        % +Quad, -Term
    z_quad_terms/5,       % ?Quad, ?S, ?P, ?O, ?G
    z_quads/2,            % ?M, -Quads
    z_quads/3,            % ?M, ?G, -Quads
    z_quads/5,            % ?M, ?S, ?P, ?O, -Quads
    z_quads/6,            % ?M, ?S, ?P, ?O, ?G, -Quads
    z_reification/4,      % ?M, ?S, ?P, ?O
    z_reification/5,      % ?M, ?S, ?P, ?O, ?G
    z_reification/6,      % ?M, ?S, ?P, ?O, ?G, -Stmt
    z_triple/2,           % ?M, -Triple
    z_triple/3,           % ?M, ?G, -Triple
    z_triple/5,           % ?M, ?S, ?P, ?O, -Triple
    z_triple/6,           % ?M, ?S, ?P, ?O, ?G, -Triple
    z_triple_iri/2,       % +Triple, -Iri
    z_triple_object/2,    % +Triple, -O
    z_triple_predicate/2, % +Triple, -P
    z_triple_subject/2,   % +Triple, -S
    z_triple_term/2,      % +Triple, -Term
    z_triple_terms/4,     % ?Triple, ?S, ?P, ?O
    z_triples/2,          % ?M, -Triples
    z_triples/3,          % ?M, ?G, -Triples
    z_triples/5,          % ?M, ?S, ?P, ?O, -Triples
    z_triples/6           % ?M, ?S, ?P, ?O, ?G, -Triples
  ]
).

/** <module> RDF statements

Enumerating statements from different back-ends.

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(aggregate)).
:- use_module(library(hdt/hdt_ext)).
:- use_module(library(gen/gen_ntuples)).
:- use_module(library(lists)).
:- use_module(library(nlp/nlp_lang)).
:- use_module(library(os/open_any2)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(yall)).

:- rdf_meta
   z(?, r, r, o),
   z(?, r, r, o, r),
   z_lts(?, r, r, -),
   z_lts(?, r, r, r, -),
   z_lts(?, r, r, +, r, -),
   z_pref_label(?, r, -),
   z_pref_label(?, r, r, -),
   z_pref_lex(?, r, r, -),
   z_pref_lex(?, r, r, r, -),
   z_pref_string(?, r, r, -),
   z_pref_string(?, r, r, r, -),
   z_quad(?, r, -),
   z_quad(?, r, r, o, -),
   z_quad(?, r, r, o, r, -),
   z_quad_datatype(t, r),
   z_quad_graph(t, r),
   z_quad_iri(t, r),
   z_quad_object(t, o),
   z_quad_predicate(t, r),
   z_quad_subject(t, r),
   z_quad_term(t, o),
   z_quad_terms(t, r, r, o, r),
   z_quads(?, r, -),
   z_quads(?, r, r, o, -),
   z_quads(?, r, r, o, r, -),
   z_reification(?, r, r, o),
   z_reification(?, r, r, o, r),
   z_reification(?, r, r, o, r, r),
   z_triple(?, r, -),
   z_triple(?, r, r, o, -),
   z_triple(?, r, r, o, r, -),
   z_triple_datatype(t, r),
   z_triple_iri(t, r),
   z_triple_object(t, o),
   z_triple_predicate(t, r),
   z_triple_subject(t, r),
   z_triple_term(t, o),
   z_triple_terms(t, r, r, o),
   z_triples(?, r, -),
   z_triples(?, r, r, o, -),
   z_triples(?, r, r, o, r, -).





%! z(?M, ?S, ?P, ?O) is nondet.
%! z(?M, ?S, ?P, ?O, ?G) is nondet.

z(M, S, P, O) :-
  z(M, S, P, O, _).


z(rdf, S, P, O, G) :-
  rdf(S, P, O, G).
z(hdt, S, P, O, G) :-
  hdt(S, P, O, G).



%! z_lts(?M, ?S, ?P, -Lit) is nondet.
%! z_lts(?M, ?S, ?P, ?G, -Lit) is nondet.
%! z_lts(?M, ?S, ?P, +LRange, ?G, -Lit) is nondet.
%
% Matches RDF statements whose object term is a language-tagged string
% that mathes the given language priory list.  Notice that results for
% each of the prioritized languages are given in arbitrary order.

z_lts(M, S, P, Lit) :-
  z_lts(M, S, P, _, Lit).


z_lts(M, S, P, G, Lit) :-
  current_lrange(LRange),
  z_lts(M, S, P, LRange, G, Lit).


z_lts(M, S, P, LRange, G, Lit) :-
  z(M, S, P, V@LTag, G),
  basic_filtering(LRange, LTag),
  Lit = V@LTag.



%! z_pref_label(?M, ?S, -Lit) is nondet.
%! z_pref_label(?M, ?S, ?G, -Lit) is nondet.

z_pref_label(M, S, Lit) :-
  z_pref_label(M, S, _, Lit).


z_pref_label(M, S, G, Lit) :-
  z_pref_string(M, S, rdfs:label, G, Lit).



%! z_pref_lex(?M, ?S, ?P, -Lex) is nondet.
%! z_pref_lex(?M, ?S, ?P, ?G, -Lex) is nondet.
%
% Like z_pref_string/[4,5], but returns only the lexical form.

z_pref_lex(M, S, P, Lex) :-
  z_pref_lex(M, S, P, _, Lex).


z_pref_lex(M, S, P, G, Lex) :-
  z_pref_string(M, S, P, G, Lit),
  z_literal_lex(Lit, Lex).



%! z_pref_string(?M, ?S, ?P, ?Lit) is nondet.
%! z_pref_string(?M, ?S, ?P, ?G, ?Lit) is nondet.
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

z_pref_string(M, S, P, Lit) :-
  z_pref_string(M, S, P, _, Lit).


z_pref_string(M, S, P, G, Lit) :-
  current_lrange(LRange),
  z_pref_string(M, S, P, LRange, G, Lit).


% Matching language-tagged strings.
z_pref_string(M, S, P, LRange, G, Lit) :-
  z_lts(M, S, P, LRange, G, Lit).
% Non-matching language-tagged strings.
z_pref_string(M, S, P, LRange, G, Lit) :-
  z(M, S, P, V@LTag, G),
  % Avoid duplicates.
  \+ basic_filtering(LRange, LTag),
  Lit = V@LTag.
% Plain XSD strings.
z_pref_string(M, S, P, _, G, V^^xsd:string) :-
  z(M, S, P, V^^xsd:string, G).



%! z_quad(?M, -Quad) is nondet.
%! z_quad(?M, ?G, -Quad) is nondet.
%! z_quad(?M, ?S, ?P, ?O, -Quad) is nondet.
%! z_quad(?M, ?S, ?P, ?O, ?G, -Quad) is nondet.

z_quad(M, Quad) :-
  z_quad(M, _, Quad).


z_quad(M, G, Quad) :-
  z_quad(M, _, _, _, G, Quad).


z_quad(M, S, P, O, Quad) :-
  z_quad(M, S, P, O, _, Quad).


z_quad(M, S, P, O, G, Quad) :-
  z(M, S, P, O, G),
  Quad = rdf(S, P, O, G).



%! z_quad_datatype(+Quad, -D) is det.
%! z_quad_graph(+Quad, -G) is det.
%! z_quad_iri(+Quad, -Iri) is det.
%! z_quad_object(+Quad, -O) is det.
%! z_quad_predicate(+Quad, -P) is det.
%! z_quad_subject(+Quad, -S) is det.
%! z_quad_term(+Quad, -Term) is nondet.

z_quad_datatype(rdf(_,_,O,_), D) :-
  z_literal_datatype(O, D).


z_quad_graph(rdf(_,_,_,G), G).


z_quad_iri(Quad, Iri) :-
  z_quad_term(Quad, Iri),
  rdf_is_iri(Iri).


z_quad_object(rdf(_,_,O,_), O).


z_quad_predicate(rdf(_,P,_,_), P).


z_quad_subject(rdf(S,_,_,_), S).


z_quad_term(rdf(S,_,_,_), S).
z_quad_term(rdf(_,P,_,_), P).
z_quad_term(rdf(_,_,O,_), O).
z_quad_term(rdf(_,_,_,G), G).



%! z_quad_terms(?Quad, ?S, ?P, ?O, ?G) is det.

z_quad_terms(rdf(S,P,O,G), S, P, O, G).



%! z_quads(?M, -Quads) is nondet.
%! z_quads(?M, ?G, -Quads) is nondet.
%! z_quads(?M, ?S, ?P, ?O, -Quads) is nondet.
%! z_quads(?M, ?S, ?P, ?O, ?G, -Quads) is nondet.

z_quads(M, Quads) :-
  z_quads(M, _, Quads).


z_quads(M, G, Quads) :-
  z_quads(M, _, _, _, G, Quads).


z_quads(M, S, P, O, Quads) :-
  z_quads(M, S, P, O, _, Quads).


z_quads(M, S, P, O, G, Quads) :-
  aggregate_all(set(Quad), z_quad(M, S, P, O, G, Quad), Quads).



%! z_reification(?M, ?S, ?P, ?O) is nondet.
%! z_reification(?M, ?S, ?P, ?O, ?G) is nondet.
%! z_reification(?M, ?S, ?P, ?O, ?G, -Stmt) is nondet.

z_reification(M, S, P, O) :-
  z_reification(M, S, P, O, _).


z_reification(M, S, P, O, G) :-
  z_reification(M, S, P, O, G, _).


z_reification(M, S, P, O, G, Stmt) :-
  z(M, Stmt, rdf:subject, S, G),
  z(M, Stmt, rdf:predicate, P, G),
  z(M, Stmt, rdf:object, O, G).



%! z_triple(?M, ?Triple) is nondet.
%! z_triple(?M, ?G, ?Triple) is nondet.
%! z_triple(?M, ?S, ?P, ?O, ?Triple) is nondet.
%! z_triple(?M, ?S, ?P, ?O, ?G, ?Triple) is nondet.

z_triple(M, Triple) :-
  z_triple(M, _, Triple).


z_triple(M, G, Triple) :-
  z_triple(M, _, _, _, G, Triple).


z_triple(M, S, P, O, Triple) :-
  z_triple(M, S, P, O, _, Triple).


z_triple(M, S, P, O, G, Triple) :-
  z(M, S, P, O, G),
  Triple = rdf(S, P, O).



%! z_triple_datatype(+Triple, -D) is det.
%! z_triple_iri(+Triple, -Iri) is nondet.
%! z_triple_object(+Triple, -O) is det.
%! z_triple_predicate(+Triple, -P) is det.
%! z_triple_subject(+Triple, -S) is det.
%! z_triple_term(+Triple, -Term) is nondet.

z_triple_datatype(rdf(_,_,O), D) :-
  z_literal_datatype(O, D).


z_triple_iri(Triple, Iri) :-
  z_triple_term(Triple, Iri),
  rdf_is_iri(Iri).


z_triple_object(rdf(_,_,O,_), O).


z_triple_predicate(rdf(_,P,_,_), P).


z_triple_subject(rdf(S,_,_,_), S).


z_triple_term(rdf(S,_,_), S).
z_triple_term(rdf(_,P,_), P).
z_triple_term(rdf(_,_,O), O).



%! z_triple_terms(?Triple, ?S, ?P, ?O) is det.

z_triple_terms(rdf(S,P,O), S, P, O).



%! z_triples(?M, -Triple) is nondet.
%! z_triples(?M, ?G, -Triple) is nondet.
%! z_triples(?M, ?S, ?P, ?O, -Triple) is nondet.
%! z_triples(?M, ?S, ?P, ?O, ?G, -Triple) is nondet.

z_triples(M, Triples) :-
  z_triples(M, _, Triples).


z_triples(M, G, Triples) :-
  z_triples(M, _, _, _, G, Triples).


z_triples(M, S, P, O, Triples) :-
  z_triples(M, S, P, O, _, Triples).


z_triples(M, S, P, O, G, Triples) :-
  aggregate_all(set(Triple), z_triple(M, S, P, O, G, Triple), Triples).





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
