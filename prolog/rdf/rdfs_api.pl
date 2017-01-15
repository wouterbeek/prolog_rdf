:- module(
  rdfs_api,
  [
    rdfs_domain/4,                   % +M, ?P, ?C, ?G
    rdfs_pref_label/3,               % +M, ?S, -Lit
    rdfs_pref_label/4,               % +M, ?S, -Lit, ?G
    rdfs_pref_label/5,               % +M, ?S, +LRange, -Lit, ?G
    rdfs_pref_label_lexical_form/3,  % +M, ?S, -Lex
    rdfs_pref_label_lexical_form/4,  % +M, ?S, -Lex, ?G
    rdfs_pref_label_lexical_form/5,  % +M, ?S, +LRange, -Lex, ?G
    rdfs_pref_string/4,              % +M, ?S, ?P, -Lit
    rdfs_pref_string/5,              % +M, ?S, ?P, -Lit, ?G
    rdfs_pref_string/6,              % +M, ?S, ?P, +LRange, -Lit, ?G
    rdfs_pref_string_lexical_form/4, % +M, ?S, ?P, -Lex
    rdfs_pref_string_lexical_form/5, % +M, ?S, ?P, -Lex, ?G
    rdfs_pref_string_lexical_form/6, % +M, ?S, ?P, +LRange, -Lex, ?G
    rdfs_range/4,                    % +M, ?P, ?C, ?G
    rdfs_strict_subclass/4,          % +M, ?C, ?D, ?G
    rdfs_subclass/4                  % +M, ?C, ?D, ?G
  ]
).

/** <module> RDFS API

@author Wouter Beek
@version 2017/01
*/

:- use_module(library(ltag/ltag_match)).
:- use_module(library(nlp/nlp_lang)).
:- use_module(library(rdf/rdf_api)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(semweb/rdf11)).

%! rdfs_label_hook(+S, +LRange, -Lbl).

%! rdfs_label_property(+P) is semidet.
%! rdfs_label_property(-P) is det.
%
% True if Property is used to believed to commonly be used to
% represent human-readable labels.

:- multifile
    rdf_label_hook/3,
    rdfs_label_property/1.

rdfs_label_property(M, P, G) :-
  rdfs_range(M, P, Ran, G),
  rdfs_subclass(M, Ran, rdfs:'Literal', G).
rdfs_label_property(skos:prefLabel).
rdfs_label_property(foaf:name).
rdfs_label_property(dc:title).
rdfs_label_property(rdfs:label).
rdfs_label_property(skos:altLabel).

:- rdf_meta
   rdfs_domain(?, r, r, r),
   rdfs_pref_label(?, r, -),
   rdfs_pref_label(?, r, -, r),
   rdfs_pref_label(?, r, +, -, r),
   rdfs_pref_label_lexical_form(?, r, -),
   rdfs_pref_label_lexical_form(?, r, -, r),
   rdfs_pref_label_lexical_form(?, r, +, -, r),
   rdfs_pref_string(?, r, r, -),
   rdfs_pref_string(?, r, r, -, r),
   rdfs_pref_string(?, r, r, +, -, r),
   rdfs_pref_string_lexical_form(?, r, r, -),
   rdfs_pref_string_lexical_form(?, r, r, -, r),
   rdfs_pref_string_lexical_form(?, r, r, +, -, r),
   rdfs_range(?, r, r, r),
   rdfs_strict_subclass(?, r, r, r),
   rdfs_subclass(?, r, r, r).





%! rdfs_domain(+M, ?P, ?C, ?G) is nondet.

rdfs_domain(M, P, C, G) :-
  t(M, P, rdfs:domain, C, G).



%! rdfs_pref_label(+M, ?S, -Lit) is nondet.
%! rdfs_pref_label(+M, ?S, -Lit, ?G) is nondet.
%! rdfs_pref_label(+M, +S, +LRange, -Lit, ?G) is nondet.
%
% Returns a preferred label.

rdfs_pref_label(M, S, Lit) :-
  rdfs_pref_label(M, S, Lit, _).


rdfs_pref_label(M, S, Lit, G) :-
  current_lrange(LRange),
  rdfs_pref_label(M, S, LRange, Lit, G).


rdfs_pref_label(_, S, LRange, Lit, _) :-
  rdfs_api:rdf_label_hook(S, LRange, Lit), !.
rdfs_pref_label(M, S, LRange, Lit, G) :-
  %rdfs_api:rdfs_label_property(P),
  rdfs_pref_string(M, S, rdfs:label, LRange, Lit, G).



%! rdfs_pref_label_lexical_form(+M, ?S, -Lex) is nondet.
%! rdfs_pref_label_lexical_form(+M, ?S, -Lex, ?G) is nondet.
%! rdfs_pref_label_lexical_form(+M, ?S, +LRange, -Lex, ?G) is nondet.

rdfs_pref_label_lexical_form(M, S, Lex) :-
  rdfs_pref_label_lexical_form(M, S, Lex, _).


rdfs_pref_label_lexical_form(M, S, Lex, G) :-
  current_lrange(LRange),
  rdfs_pref_label_lexical_form(M, S, LRange, Lex, G).


rdfs_pref_label_lexical_form(M, S, LRange, Lex, G) :-
  rdfs_pref_label(M, S, LRange, Lit, G),
  rdf_literal_lexical_form(Lit, Lex).



%! rdfs_pref_string(+M, ?S, ?P, -Lit) is nondet.
%! rdfs_pref_string(+M, ?S, ?P, -Lit, ?G) is nondet.
%! rdfs_pref_string(+M, ?S, ?P, +LRange, -Lit, ?G) is nondet.
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

rdfs_pref_string(M, S, P, Lit) :-
  rdfs_pref_string(M, S, P, Lit, _).


rdfs_pref_string(M, S, P, Lit, G) :-
  current_lrange(LRange),
  rdfs_pref_string(M, S, P, LRange, Lit, G).


% Matching language-tagged strings.
rdfs_pref_string(M, S, P, LRange, Lit, G) :-
  rdf_lts(M, S, P, LRange, Lit, G).
% Non-matching language-tagged strings.
rdfs_pref_string(M, S, P, LRange, Lit, G) :-
  t(M, S, P, V@LTag, G),
  % Avoid duplicates.
  \+ basic_filtering(LRange, LTag),
  Lit = V@LTag.
% Plain XSD strings.
rdfs_pref_string(M, S, P, _, V^^D, G) :-
  % @bug RDF prefix expansion does not work here.
  rdf_equal(D, xsd:string),
  t(M, S, P, V^^D, G).



%! rdfs_pref_string_lexical_form(+M, ?S, ?P, -Lex) is nondet.
%! rdfs_pref_string_lexical_form(+M, ?S, ?P, -Lex, ?G) is nondet.
%! rdfs_pref_string_lexical_form(+M, ?S, ?P, +LRange, -Lex, ?G) is nondet.
%
% Like rdfs_pref_string/[4-6], but returns only the lexical form.

rdfs_pref_string_lexical_form(M, S, P, Lex) :-
  rdfs_pref_string_lexical_form(M, S, P, Lex, _).


rdfs_pref_string_lexical_form(M, S, P, Lex, G) :-
  current_lrange(LRange),
  rdfs_pref_string_lexical_form(M, S, P, LRange, Lex, G).


rdfs_pref_string_lexical_form(M, S, P, LRange, Lex, G) :-
  rdfs_pref_string(M, S, P, LRange, Lit, G),
  rdf_literal_lexical_form(Lit, Lex).



%! rdfs_range(+M, ?P, ?C, ?G) is nondet.

rdfs_range(M, P, C, G) :-
  t(M, P, rdfs:range, C, G).



%! rdfs_strict_subclass(+M, ?C, ?D, ?G) is nondet.

rdfs_strict_subclass(M, C, D, G) :-
  rdfs_subclass(M, C, D, G),
  C \== D.



%! rdfs_subclass(+M, ?C, ?D, ?G) is nondet.

rdfs_subclass(_, C, C, _).
rdfs_subclass(M, C, D, G) :-
  t(M, C, rdfs:subClassOf, D, G).
