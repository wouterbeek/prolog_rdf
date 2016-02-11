:- module(
  rdf_api,
  [
    rdf_image/2,         % +S, -Image
    rdf_langstring/3,    % ?S, ?P, ?V
    rdf_langstring/4,    % ?S, ?P, +LanguagePriorityList, ?V
    rdf_one_string/3,    % +S, +P, -V:atom
    rdf_pref_string/3,   % ?S, ?P, -Lex
    rdf_pref_string/4,   % ?S, ?P, +LanguagePriorityList, -Lex
    rdf_pref_string/5,   % ?S, ?P, +LanguagePriorityList, -LTag, -Lex
    rdf_snap/1,          % :Goal_0
    rdf_triples/4,       % ?S, ?P. ?O, -Triples:ordset(rdf_triple)
    rdfs_instance0/2,    % ?I, ?C
    rdfs_label/2,        % +S, -LexicalForm
    rdfs_label/3,        % +S, +LanguagePriorityList, -LexicalForm
    rdfs_label/4,        % +S, +LanguagePriorityList, -LanguageTag, -LexicalForm
    rdfs_one_label/2     % +S, -LexicalForm
  ]
).
:- reexport(library(semweb/rdf11)).

/** <module> RDF: API

@author Wouter Beek
@compat RDF 1.1
@version 2015/12-2016/01
*/

:- use_module(library(aggregate)).
:- use_module(library(rdf/rdf_prefix), []). % Load RDF prefixes.
:- use_module(library(uuid)).

:- meta_predicate
    rdf_snap(0).

:- rdf_meta
   rdf_image(o, -),
   rdf_langstring(o, r, ?),
   rdf_langstring(o, r, +, ?),
   rdf_langstring(o, r, +, ?, r),
   rdf_one_string(o, r, -),
   rdf_pref_string(o, r, -),
   rdf_pref_string(o, r, +, -),
   rdf_pref_string(o, r, +, -, -),
   rdf_pref_string(o, r, +, -, -, r),
   rdf_triples(o, r, o, -),
   rdfs_instance0(o, r),
   rdfs_label(o, -),
   rdfs_label(o, +, -),
   rdfs_label(o, +, -, -),
   rdfs_one_label(o, -).





%! rdf_image(+S, -Image:iri) is nondet.

rdf_image(S, V) :-
  rdf_has(S, dbo:thumbnail, V^^xsd:anyURI).
rdf_image(S, V) :-
  rdf_has(S, foaf:depiction, V^^xsd:anyURI).
rdf_image(S, V) :-
  rdf(S, _, V),
  rdfs_instance0(V, dcmit:'Image').



%! rdf_langstring(?S, ?P, -Lit) is nondet.
%! rdf_langstring(?S, ?P, +LanguagePriorityList:list(atom), -Lit) is nondet.
% Matches RDF statements whose object term is a language-tagged string
% that mathes the given language priory list.
% Notice that results for each of the prioritized languages are given
% in arbitrary order.

rdf_langstring(S, P, Lit) :-
  user:setting(language_priority_list, LRanges),
  rdf_langstring(S, P, LRanges, Lit).

rdf_langstring(S, P, LRanges, Lex@LTag) :-
  rdf_has(S, P, Lex@LTag),
  atom(LTag),
  basic_filtering(LRanges, LTag).



%! rdf_one_string(+S, +P, -LexicalForm:atom) is det.

rdf_one_string(S, P, Lex) :-
  rdf_pref_string(S, P, Lex), !.
rdf_one_string(_, _, '∅').



%! rdf_pref_string(?S, ?P, -LexicalForm:atom) is nondet.
%! rdf_pref_string(?S, ?P, +LanguagePriorityList:list(atom), -LexicalFrom:atom) is nondet.
%! rdf_pref_string(?S, ?P, +LanguagePriorityList:list(atom), -LanguageTag:atom, -LexicalFrom:atom) is nondet.
% Returns, in this exact order:
%   1. The language-tagged strings that match the given
%      language priority list; returning results for higher
%      priority language earlier.
%   2. The language-tagged strings that do not match the given
%      language priority list.
%   3. XSD strings.

rdf_pref_string(S, P, Lex) :-
  user:setting(language_priority_list, LRanges),
  rdf_pref_string(S, P, LRanges, Lex).

rdf_pref_string(S, P, LRanges, Lex) :-
  rdf_pref_string(S, P, LRanges, _, Lex).

% Matching language-tagged strings.
rdf_pref_string(S, P, LRanges, LTag, Lex) :-
  member(LRange, LRanges),
  rdf_langstring(S, P, [LRange], Lex@LTag).
% Non-matching language-tagged strings.
rdf_pref_string(S, P, LRanges, LTag, Lex) :-
  rdf_has(S, P, Lex@LTag),
  % Avoid duplicates.
  \+ basic_filtering(LRanges, LTag).
% Plain XSD strings.
rdf_pref_string(S, P, _, _, Lex) :-
  rdf_has(S, P, Lex^^xsd:string).



%! rdf_snap(:Goal_0) .

rdf_snap(Goal_0) :-
  rdf_transaction(Goal_0, _, [snapshot(true)]).



%! rdf_triples(?S, ?P, ?O, -Trips) is det.

rdf_triples(S, P, O, Trips):-
  aggregate_all(set(rdf(S,P,O)), rdf(S, P, O), Trips).



%! rdfs_instance0(?I, ?C) is nondet.

rdfs_instance0(I, D) :-
  rdf_has(I, rdf:type, C),
  rdf_reachable(C, rdfs:subClassOf, D).



%! rdfs_label(+S, -Label) is nondet.
%! rdfs_label(+S, +LanguagePriorityList, -Label) is nondet.
%! rdfs_label(+S, +LanguagePriorityList, -LTag, -Label) is nondet.

rdfs_label(S, Lex) :-
  rdf_pref_string(S, rdfs:label, Lex).

rdfs_label(S, LRanges, Lex) :-
  rdf_pref_string(S, rdfs:lalbel, LRanges, Lex).

rdfs_label(S, LRanges, LTag, Lex) :-
  rdf_pref_string(S, rdfs:label, LRanges, LTag, Lex).



%! rdfs_one_label(+S, -Label) is det.

rdfs_one_label(S, Lex) :-
  rdfs_label(S, Lex), !.
rdfs_one_label(_, '∅').





% HELPER %

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


%! subtag_match(+RangeSubtag:atom, +Subtag:atom) is semidet.
% Two subtags match if either they are the same when compared
% case-insensitively or the language range's subtag is the wildcard `*`

subtag_match(*, _):- !.
subtag_match(X1, X2):-
  downcase_atom(X1, X),
  downcase_atom(X2, X).
