:- module(
  rdf_api,
  [
    rdf_aggregate_all/3,   % +Template, :Goal, -Result
    rdf_image/2,           % +S, -Img
    rdf_langstring/3,      % ?S, ?P, -Lit
    rdf_langstring_lex/3,  % ?S, ?P, -Lex
    rdf_one_string/3,      % +S, +P, -Lit
    rdf_one_string_lex/3,  % +S, +P, -Lex
    rdf_pref_string/3,     % ?S, ?P, -Lit
    rdf_pref_string_lex/3, % ?S, ?P, -Lex
    rdf_snap/1,            % :Goal_0
    rdf_triples/4,         % ?S, ?P. ?O, -Trips:ordset
    rdfs_instance0/2,      % ?I, ?C
    rdfs_label/2,          % +S, -Lit
    rdfs_label_lex/2,      % +S, -Lex
    rdfs_one_label/2,      % +S, -Lit
    rdfs_one_label_lex/2   % +S, -Lex
  ]
).
:- reexport(library(rdf11/rdf11)).

/** <module> RDF: API

@author Wouter Beek
@compat RDF 1.1
@version 2015/12-2016/02
*/

:- use_module(library(aggregate)).
:- use_module(library(rdf/rdf_prefix), []). % Load RDF prefixes.
:- use_module(library(uuid)).

:- meta_predicate
    rdf_aggregate_all(+, 0, -),
    rdf_snap(0).

:- rdf_meta
   rdf_aggregate_all(+, t, -),
   rdf_image(r, -),
   rdf_langstring(r, r, o),
   rdf_langstring_lex(r, r, -),
   rdf_one_string(r, r, o),
   rdf_one_string_lex(r, r, -),
   rdf_pref_string(r, r, o),
   rdf_pref_string(r, r, -, o),
   rdf_pref_string(r, r, -, -, o),
   rdf_pref_string_lex(r, r, -),
   rdf_triples(r, r, o, -),
   rdfs_instance0(o, r),
   rdfs_label(r, o),
   rdfs_label_lex(r, -),
   rdfs_one_label(r, o),
   rdfs_one_label_lex(r, -).





%! rdf_aggregate_all(+Template, :Goal, -Result) is det.

rdf_aggregate_all(Template, Goal, Result) :-
  aggregate_all(Template, Goal, Result).



%! rdf_image(+S, -Image:iri) is nondet.

rdf_image(S, V) :-
  rdf_has(S, dbo:thumbnail, V^^xsd:anyURI).
rdf_image(S, V) :-
  rdf_has(S, foaf:depiction, V^^xsd:anyURI).
rdf_image(S, V) :-
  rdf(S, _, V),
  rdfs_instance0(V, dcmit:'Image').



%! rdf_langstring(?S, ?P, -Lit) is nondet.
% Matches RDF statements whose object term is a language-tagged string
% that mathes the given language priory list.
% Notice that results for each of the prioritized languages are given
% in arbitrary order.

rdf_langstring(S, P, Lit) :-
  current_lrange(LRange),
  rdf_langstring(S, P, LRange, Lit).

rdf_langstring(S, P, LRange, Lit) :-
  rdf_has(S, P, String@LTag),
  basic_filtering(LRange, LTag),
  Lit = String@LTag.


%! rdf_langstring_lex(?S, ?P, -Lex) is nondet.

rdf_langstring_lex(S, P, Lex) :-
  rdf_langstring(S, P, Lit),
  rdf_lexical_form(Lit, Lex).



%! rdf_one_string(+S, +P, -Lit) is det.

rdf_one_string(S, P, Lit) :-
  rdf_pref_string(S, P, Lit), !.
rdf_one_string(_, _, '∅'^^xsd:string).


%! rdf_one_string_lex(+S, +P, -Lex) is det.

rdf_one_string_lex(S, P, Lex) :-
  rdf_one_string(S, P, Lit),
  rdf_lexical_form(Lit, Lex).



%! rdf_pref_string(?S, ?P, -Lit) is nondet.
% Returns, in this exact order:
%   1. The language-tagged strings that match the given
%      language priority list; returning results for higher
%      priority language earlier.
%   2. The language-tagged strings that do not match the given
%      language priority list.
%   3. XSD strings.

rdf_pref_string(S, P, Lit) :-
  current_lrange(LRange),
  rdf_pref_string(S, P, LRange, Lit).

% Matching language-tagged strings.
rdf_pref_string(S, P, LRange, Lit) :-
  rdf_langstring(S, P, LRange, Lit).
% Non-matching language-tagged strings.
rdf_pref_string(S, P, LRange, Lit) :-
  rdf_has(S, P, S@LTag),
  % Avoid duplicates.
  \+ basic_filtering(LRange, LTag),
  Lit = S@LTag.
% Plain XSD strings.
rdf_pref_string(S, P, _, S^^xsd:string) :-
  rdf_has(S, P, S^^xsd:string).


%! rdf_pref_string_lex(?S, ?P, -Lex) is nondet.

rdf_pref_string_lex(S, P, Lex) :-
  rdf_pref_string(S, P, Lit),
  rdf_lexical_form(Lit, Lex).



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



%! rdfs_label(+S, -Lit) is nondet.

rdfs_label(S, Lit) :-
  rdf_pref_string(S, rdfs:label, Lit).


%! rdfs_label_lex(+S, -Lex) is nondet.

rdfs_label_lex(S, Lex) :-
  rdfs_label(S, Lit),
  rdf_lexical_form(Lit, Lex).



%! rdfs_one_label(+S, -Lit) is det.

rdfs_one_label(S, Lit) :-
  rdfs_label(S, Lit), !.
rdfs_one_label(_, '∅'^^xsd:string).


%! rdfs_one_label_lex(+S, -Lex) is det.

rdfs_one_label_lex(S, Lex) :-
  rdfs_one_label(S, Lit),
  rdf_lexical_form(Lit, Lex).





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
