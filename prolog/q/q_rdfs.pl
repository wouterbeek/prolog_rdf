:- module(
  q_rdfs,
  [
    q_domain/4,          % +M, ?P, ?C, ?G
    q_pref_label/3,      % +M, ?S, -Lit
    q_pref_label/4,      % +M, ?S, -Lit, ?G
    q_pref_label/5,      % +M, ?S, +LRange, -Lit, ?G
    q_pref_label_lex/3,  % +M, ?S, -Lex
    q_pref_label_lex/4,  % +M, ?S, -Lex, ?G
    q_pref_label_lex/5,  % +M, ?S, +LRange, -Lex, ?G
    q_pref_string/4,     % +M, ?S, ?P, -Lit
    q_pref_string/5,     % +M, ?S, ?P, -Lit, ?G
    q_pref_string/6,     % +M, ?S, ?P, +LRange, -Lit, ?G
    q_pref_string_lex/4, % +M, ?S, ?P, -Lex
    q_pref_string_lex/5, % +M, ?S, ?P, -Lex, ?G
    q_pref_string_lex/6, % +M, ?S, ?P, +LRange, -Lex, ?G
    q_range/4,           % +M, ?P, ?C, ?G
    q_subclass/4         % +M, ?C, ?D, ?G
  ]
).

/** <module> Quine: RDFS

@author Wouter Beek
@version 2016/09-2016/10
*/

:- use_module(library(ltag/ltag_match)).
:- use_module(library(nlp/nlp_lang)).
:- use_module(library(q/q_rdf)).
:- use_module(library(q/q_term)).
:- use_module(library(semweb/rdf11)).

%! q_label_hook(+S, +LRange, -Lbl).

%! q_label_property(+P) is semidet.
%! q_label_property(-P) is det.
%
% True if Property is used to believed to commonly be used to
% represent human-readable labels.

:- multifile
    q_label_hook/3,
    q_label_property/1.

q_label_property(M, P, G) :-
  q_range(M, P, Ran, G),
  q_subclass(M, Ran, rdfs:'Literal', G).
q_label_property(skos:prefLabel).
q_label_property(foaf:name).
q_label_property(dc:title).
q_label_property(rdfs:label).
q_label_property(skos:altLabel).

:- rdf_meta
   q_domain(?, r, r, r),
   q_pref_label(?, r, -),
   q_pref_label(?, r, -, r),
   q_pref_label(?, r, +, -, r),
   q_pref_label_lex(?, r, -),
   q_pref_label_lex(?, r, -, r),
   q_pref_label_lex(?, r, +, -, r),
   q_pref_string(?, r, r, -),
   q_pref_string(?, r, r, -, r),
   q_pref_string(?, r, r, +, -, r),
   q_pref_string_lex(?, r, r, -),
   q_pref_string_lex(?, r, r, -, r),
   q_pref_string_lex(?, r, r, +, -, r),
   q_range(?, r, r, r),
   q_subclass(?, r, r, r).





%! q_domain(+M, ?P, ?C, ?G) is nondet.

q_domain(M, P, C, G) :-
  q(M, P, rdfs:domain, C, G).



%! q_pref_label(+M, ?S, -Lit) is nondet.
%! q_pref_label(+M, ?S, -Lit, ?G) is nondet.
%! q_pref_label(+M, +S, +LRange, -Lit, ?G) is nondet.
%
% Returns a preferred label


q_pref_label(M, S, Lit) :-
  q_pref_label(M, S, Lit, _).


q_pref_label(M, S, Lit, G) :-
  current_lrange(LRange),
  q_pref_label(M, S, LRange, Lit, G).


q_pref_label(_, S, LRange, Lit, _) :-
  q_rdfs:q_label_hook(S, LRange, Lit), !.
q_pref_label(M, S, LRange, Lit, G) :-
  %q_rdfs:q_label_property(P),
  q_pref_string(M, S, rdfs:label, LRange, Lit, G).



%! q_pref_label_lex(+M, ?S, -Lex) is nondet.
%! q_pref_label_lex(+M, ?S, -Lex, ?G) is nondet.
%! q_pref_label_lex(+M, ?S, +LRange, -Lex, ?G) is nondet.

q_pref_label_lex(M, S, Lex) :-
  q_pref_label_lex(M, S, Lex, _).


q_pref_label_lex(M, S, Lex, G) :-
  current_lrange(LRange),
  q_pref_label_lex(M, S, LRange, Lex, G).


q_pref_label_lex(M, S, LRange, Lex, G) :-
  q_pref_label(M, S, LRange, Lit, G),
  q_literal_lex(Lit, Lex).



%! q_pref_string(+M, ?S, ?P, -Lit) is nondet.
%! q_pref_string(+M, ?S, ?P, -Lit, ?G) is nondet.
%! q_pref_string(+M, ?S, ?P, +LRange, -Lit, ?G) is nondet.
%
% Returns, in this exact order:
%
%   1. The language-tagged strings that match the given language
%      priority list; returning results for higher priority language
%      earlier.
%
%   2. The language-tagged strings that do not match the given
%      language priority list.
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
q_pref_string(M, S, P, _, V^^D, G) :-
  % @bug RDF prefix expansion does not work here.
  rdf_equal(D, xsd:string),
  q(M, S, P, V^^D, G).



%! q_pref_string_lex(+M, ?S, ?P, -Lex) is nondet.
%! q_pref_string_lex(+M, ?S, ?P, -Lex, ?G) is nondet.
%! q_pref_string_lex(+M, ?S, ?P, +LRange, -Lex, ?G) is nondet.
%
% Like q_pref_string/[4-6], but returns only the lexical form.

q_pref_string_lex(M, S, P, Lex) :-
  q_pref_string_lex(M, S, P, Lex, _).


q_pref_string_lex(M, S, P, Lex, G) :-
  current_lrange(LRange),
  q_pref_string_lex(M, S, P, LRange, Lex, G).


q_pref_string_lex(M, S, P, LRange, Lex, G) :-
  q_pref_string(M, S, P, LRange, Lit, G),
  q_literal_lex(Lit, Lex).



%! q_range(+M, ?P, ?C, ?G) is nondet.

q_range(M, P, C, G) :-
  q(M, P, rdfs:range, C, G).



%! q_subclass(+M, ?C, ?D, ?G) is nondet.

q_subclass(M, C, D, G) :-
  q(M, C, rdfs:subClassOf, D, G).
